{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Lib (program) where

import Codec.Xlsx
import Codec.Xlsx.Formatted
import Codec.Xlsx.Types.StyleSheet
import Control.Applicative ((<|>))
import Control.Lens
import Data.Aeson.Types (parseFail, prependFailure, typeMismatch)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 qualified as L
import Data.List (foldl', tails)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Data.Yaml
  ( FromJSON (..)
  , ParseException
  , Value (..)
  , decodeFileEither
  , prettyPrintParseException
  , (.:)
  )
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.Terminal.Size (Window (Window), size)
import System.IO (stdout)

data Class
  = MondayClass
      { classInterval :: Interval
      }
  | TuesdayClass
      { classInterval :: Interval
      }
  | WednesdayClass
      { classInterval :: Interval
      }
  | ThursdayClass
      { classInterval :: Interval
      }
  | FridayClass
      { classInterval :: Interval
      }
  | SaturdayClass
      { classInterval :: Interval
      }
  | SundayClass
      { classInterval :: Interval
      }

instance Show Class where
  show x =
    case x of
      MondayClass inter -> "Monday: " <> show inter
      TuesdayClass inter -> "Tuesday: " <> show inter
      WednesdayClass inter -> "Wednesday: " <> show inter
      ThursdayClass inter -> "Thursday: " <> show inter
      FridayClass inter -> "Friday: " <> show inter
      SaturdayClass inter -> "Saturday: " <> show inter
      SundayClass inter -> "Sunday: " <> show inter

data Hour
  = H0
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | H7
  | H8
  | H9
  | H10
  | H11
  | H12
  | H13
  | H14
  | H15
  | H16
  | H17
  | H18
  | H19
  | H20
  | H21
  | H22
  | H23
  deriving (Eq, Ord, Bounded)

instance Show Hour where
  show x =
    case x of
      H0 -> "00"
      H1 -> "01"
      H2 -> "02"
      H3 -> "03"
      H4 -> "04"
      H5 -> "05"
      H6 -> "06"
      H7 -> "07"
      H8 -> "08"
      H9 -> "09"
      H10 -> "10"
      H11 -> "11"
      H12 -> "12"
      H13 -> "13"
      H14 -> "14"
      H15 -> "15"
      H16 -> "16"
      H17 -> "17"
      H18 -> "18"
      H19 -> "19"
      H20 -> "20"
      H21 -> "21"
      H22 -> "22"
      H23 -> "23"

data Minute
  = ZeroMinutes
  | HalfAnHour
  deriving (Eq, Ord, Bounded)

instance Show Minute where
  show x =
    case x of
      ZeroMinutes -> "00"
      HalfAnHour -> "30"

data Time = MkTime
  { timeHour :: Hour
  , timeMinute :: Minute
  }
  deriving (Eq, Ord, Bounded)

instance Show Time where
  show (MkTime hour minutes) = show hour <> ":" <> show minutes

instance Enum Time where
  toEnum i =
    case i of
      0 -> MkTime H0 ZeroMinutes
      1 -> MkTime H0 HalfAnHour
      2 -> MkTime H1 ZeroMinutes
      3 -> MkTime H1 HalfAnHour
      4 -> MkTime H2 ZeroMinutes
      5 -> MkTime H2 HalfAnHour
      6 -> MkTime H3 ZeroMinutes
      7 -> MkTime H3 HalfAnHour
      8 -> MkTime H4 ZeroMinutes
      9 -> MkTime H4 HalfAnHour
      10 -> MkTime H5 ZeroMinutes
      11 -> MkTime H5 HalfAnHour
      12 -> MkTime H6 ZeroMinutes
      13 -> MkTime H6 HalfAnHour
      14 -> MkTime H7 ZeroMinutes
      15 -> MkTime H7 HalfAnHour
      16 -> MkTime H8 ZeroMinutes
      17 -> MkTime H8 HalfAnHour
      18 -> MkTime H9 ZeroMinutes
      19 -> MkTime H9 HalfAnHour
      20 -> MkTime H10 ZeroMinutes
      21 -> MkTime H10 HalfAnHour
      22 -> MkTime H11 ZeroMinutes
      23 -> MkTime H11 HalfAnHour
      24 -> MkTime H12 ZeroMinutes
      25 -> MkTime H12 HalfAnHour
      26 -> MkTime H13 ZeroMinutes
      27 -> MkTime H13 HalfAnHour
      28 -> MkTime H14 ZeroMinutes
      29 -> MkTime H14 HalfAnHour
      30 -> MkTime H15 ZeroMinutes
      31 -> MkTime H15 HalfAnHour
      32 -> MkTime H16 ZeroMinutes
      33 -> MkTime H16 HalfAnHour
      34 -> MkTime H17 ZeroMinutes
      35 -> MkTime H17 HalfAnHour
      36 -> MkTime H18 ZeroMinutes
      37 -> MkTime H18 HalfAnHour
      38 -> MkTime H19 ZeroMinutes
      39 -> MkTime H19 HalfAnHour
      40 -> MkTime H20 ZeroMinutes
      41 -> MkTime H20 HalfAnHour
      42 -> MkTime H21 ZeroMinutes
      43 -> MkTime H21 HalfAnHour
      44 -> MkTime H22 ZeroMinutes
      45 -> MkTime H22 HalfAnHour
      46 -> MkTime H23 ZeroMinutes
      47 -> MkTime H23 HalfAnHour
      _ -> error "out of bounds enum"
  fromEnum t =
    case t of
      MkTime H0 ZeroMinutes -> 0
      MkTime H0 HalfAnHour -> 1
      MkTime H1 ZeroMinutes -> 2
      MkTime H1 HalfAnHour -> 3
      MkTime H2 ZeroMinutes -> 4
      MkTime H2 HalfAnHour -> 5
      MkTime H3 ZeroMinutes -> 6
      MkTime H3 HalfAnHour -> 7
      MkTime H4 ZeroMinutes -> 8
      MkTime H4 HalfAnHour -> 9
      MkTime H5 ZeroMinutes -> 10
      MkTime H5 HalfAnHour -> 11
      MkTime H6 ZeroMinutes -> 12
      MkTime H6 HalfAnHour -> 13
      MkTime H7 ZeroMinutes -> 14
      MkTime H7 HalfAnHour -> 15
      MkTime H8 ZeroMinutes -> 16
      MkTime H8 HalfAnHour -> 17
      MkTime H9 ZeroMinutes -> 18
      MkTime H9 HalfAnHour -> 19
      MkTime H10 ZeroMinutes -> 20
      MkTime H10 HalfAnHour -> 21
      MkTime H11 ZeroMinutes -> 22
      MkTime H11 HalfAnHour -> 23
      MkTime H12 ZeroMinutes -> 24
      MkTime H12 HalfAnHour -> 25
      MkTime H13 ZeroMinutes -> 26
      MkTime H13 HalfAnHour -> 27
      MkTime H14 ZeroMinutes -> 28
      MkTime H14 HalfAnHour -> 29
      MkTime H15 ZeroMinutes -> 30
      MkTime H15 HalfAnHour -> 31
      MkTime H16 ZeroMinutes -> 32
      MkTime H16 HalfAnHour -> 33
      MkTime H17 ZeroMinutes -> 34
      MkTime H17 HalfAnHour -> 35
      MkTime H18 ZeroMinutes -> 36
      MkTime H18 HalfAnHour -> 37
      MkTime H19 ZeroMinutes -> 38
      MkTime H19 HalfAnHour -> 39
      MkTime H20 ZeroMinutes -> 40
      MkTime H20 HalfAnHour -> 41
      MkTime H21 ZeroMinutes -> 42
      MkTime H21 HalfAnHour -> 43
      MkTime H22 ZeroMinutes -> 44
      MkTime H22 HalfAnHour -> 45
      MkTime H23 ZeroMinutes -> 46
      MkTime H23 HalfAnHour -> 47

data Interval = MkInterval
  { intervalStartingTime :: Time
  , intervalEndTime :: Time
  }

instance Show Interval where
  show (MkInterval beginning end) = show beginning <> "-" <> show end

newtype IDandSubj
  = IDandSubj (T.Text, Subject)

data Subject = MkSubject
  { subjName :: T.Text
  , subjProfessor :: T.Text
  , subjclasses :: [Class]
  -- maybe will change to S.Seq
  }

instance Show Subject where
  show (MkSubject sname sprof classes) =
    "{ Subject name: "
      <> show sname
      <> ", Subject professor: "
      <> show sprof
      <> ", Subject classes "
      <> show classes
      <> " }"

data Error
  = OverlappingClasses (Subject, Class) (Subject, Class)
  | RepeatedSubjId T.Text Subject Subject
  deriving (Show)

createInterval :: Time -> Time -> Maybe Interval
createInterval x y =
  if x < y
    then Just $ MkInterval x y
    else Nothing

instance FromJSON Time where
  parseJSON (String str) =
    prependFailure "parsing Time failed, " $ case res of
      [x, y] -> MkTime <$> h x <*> t y
      [] -> parseFail "inexistent hour for class"
      _ -> parseFail "unexpected ':'. More than 1."
    where
      res = T.splitOn ":" str
      h head' = case head' of
        "00" -> pure H0
        "01" -> pure H1
        "02" -> pure H2
        "03" -> pure H3
        "04" -> pure H4
        "05" -> pure H5
        "06" -> pure H6
        "07" -> pure H7
        "08" -> pure H8
        "09" -> pure H9
        "10" -> pure H10
        "11" -> pure H11
        "12" -> pure H12
        "13" -> pure H13
        "14" -> pure H14
        "15" -> pure H15
        "16" -> pure H16
        "17" -> pure H17
        "18" -> pure H18
        "19" -> pure H19
        "20" -> pure H20
        "21" -> pure H21
        "22" -> pure H22
        "23" -> pure H23
        "1" -> pure H1
        "2" -> pure H2
        "3" -> pure H3
        "4" -> pure H4
        "5" -> pure H5
        "6" -> pure H6
        "7" -> pure H7
        "8" -> pure H8
        "9" -> pure H9
        _ -> parseFail $ T.unpack $ "Invalid hour: " <> head'
      t tail' = case tail' of
        "00" -> pure ZeroMinutes
        "30" -> pure HalfAnHour
        _ -> parseFail $ T.unpack $ "Invalid minutes: " <> tail'
  parseJSON invalid =
    prependFailure
      "parsing Time failed, "
      (typeMismatch "String" invalid)

instance FromJSON Interval where
  parseJSON (Object obj) = do
    beginningTime <- obj .: "inicio" <|> obj .: "start"
    endTime <- obj .: "final" <|> obj .: "end"
    case createInterval beginningTime endTime of
      Nothing ->
        parseFail $
          "invalid Interval, class ends: "
            <> show beginningTime
            <> ", before it begins: "
            <> show endTime
      Just validInterval -> pure validInterval
  parseJSON invalid =
    prependFailure
      "parsing Interval failed, "
      (typeMismatch "Object" invalid)

instance {-# OVERLAPPING #-} FromJSON Class where
  parseJSON (Object obj) = prependFailure "parsing Class failed, " $ do
    day <- obj .: "dia" <|> obj .: "day"
    interval <-
      prependFailure (T.unpack $ "in day '" <> day <> "', ") $ parseJSON (Object obj)
    case day of
      "lunes" -> pure $ MondayClass interval
      "monday" -> pure $ MondayClass interval
      "martes" -> pure $ TuesdayClass interval
      "tuesday" -> pure $ TuesdayClass interval
      "miercoles" -> pure $ WednesdayClass interval
      "wednesday" -> pure $ WednesdayClass interval
      "jueves" -> pure $ ThursdayClass interval
      "thursday" -> pure $ ThursdayClass interval
      "viernes" -> pure $ FridayClass interval
      "friday" -> pure $ FridayClass interval
      "sabado" -> pure $ SaturdayClass interval
      "saturday" -> pure $ SaturdayClass interval
      "domingo" -> pure $ SundayClass interval
      "sunday" -> pure $ SundayClass interval
      _ -> parseFail $ "Invalid Class day: " <> T.unpack day
  parseJSON invalid =
    prependFailure
      "parsing Interval failed, "
      (typeMismatch "Object" invalid)

instance {-# OVERLAPPING #-} FromJSON IDandSubj where
  parseJSON (Object obj) = prependFailure "parsing Subject failed, " $ do
    name <- obj .: "nombre" <|> obj .: "name"
    let errorInClassName = "in class '" <> T.unpack name <> "', "
    classId <-
      prependFailure errorInClassName $ obj .: "id-clase" <|> obj .: "class-id"
    let errorInClassId =
          errorInClassName
            <> "with ID '"
            <> T.unpack classId
            <> "', "
    professor <-
      prependFailure errorInClassId $ obj .: "profesor" <|> obj .: "professor"
    let errorInClassProfessor =
          errorInClassId
            <> ("with Professor: '" <> T.unpack professor <> "', ")
    classes <-
      prependFailure errorInClassProfessor $ obj .: "dias" <|> obj .: "days"
    pure $ IDandSubj (classId, MkSubject name professor classes)
  parseJSON invalid =
    prependFailure
      "parsing Interval failed, "
      (typeMismatch "Object" invalid)

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (MkInterval a b) (MkInterval x y)
  | a == x && b == y = True
  | a < x && x < b = True
  | a < y && y < b = True
  | x < a && a < y = True
  | x < b && b < y = True
  | otherwise = False

classesOverlap :: Class -> Class -> Bool
classesOverlap class1 class2 =
  case (class1, class2) of
    (MondayClass inter1, MondayClass inter2) -> intervalsOverlap inter1 inter2
    (TuesdayClass inter1, TuesdayClass inter2) -> intervalsOverlap inter1 inter2
    (WednesdayClass inter1, WednesdayClass inter2) -> intervalsOverlap inter1 inter2
    (ThursdayClass inter1, ThursdayClass inter2) -> intervalsOverlap inter1 inter2
    (FridayClass inter1, FridayClass inter2) -> intervalsOverlap inter1 inter2
    (SaturdayClass inter1, SaturdayClass inter2) -> intervalsOverlap inter1 inter2
    _ -> False

getMapsFromValues
  :: [IDandSubj] -> Either Error (M.Map T.Text [T.Text], M.Map T.Text Subject)
getMapsFromValues values = go values (Right (M.empty, M.empty))
  where
    go
      :: [IDandSubj]
      -> Either Error (M.Map T.Text [T.Text], M.Map T.Text Subject)
      -> Either Error (M.Map T.Text [T.Text], M.Map T.Text Subject)
    go [] result = result
    go (IDandSubj (id', subj) : xs) res =
      do
        (keyMap, valMap) <- res
        case M.lookup id' valMap of
          Just repeatedSubj -> Left $ RepeatedSubjId id' subj repeatedSubj
          Nothing -> go xs (Right (M.alter alteringFunc sName keyMap, M.insert id' subj valMap))
      where
        (MkSubject sName _ _) = subj
        alteringFunc :: Maybe [T.Text] -> Maybe [T.Text]
        alteringFunc (Just valueInside) = Just (id' : valueInside)
        alteringFunc Nothing = Just [id']

genPossibleClassCombinations
  :: Applicative f
  => M.Map T.Text (f T.Text)
  -> f [T.Text] -- outpts the list of lists of ids as Text
genPossibleClassCombinations = sequenceA . M.elems

tuples :: Int -> [a] -> [[a]]
tuples =
  let go r =
        case compare r 0 of
          LT -> const []
          EQ -> const [[]]
          GT -> concatMap (\(y : ys) -> map (y :) (go (r - 1) ys)) . init . tails
   in go

validate :: M.Map T.Text Subject -> [[T.Text]] -> ([Error], [[IDandSubj]]) -- (errors, and successes)
validate allSubjectsMp = foldl foldingF ([], [])
  where
    validateSubj :: [T.Text] -> Maybe Error
    validateSubj combinations = validateClasses' asClasses -- combinations: ["1243", "1445", ..]
      where
        asClasses =
          mconcat $
            fmap
              (\txtId -> (txtId,) <$> subjclasses (allSubjectsMp M.! txtId))
              combinations

        validateClasses' :: [(T.Text, Class)] -> Maybe Error
        validateClasses' allClasses = foldl f Nothing pairCombinations
          where
            pairCombinations = toTup <$> tuples 2 allClasses
            toTup xs = case xs of
              [c1, c2] -> (c1, c2)
              _ ->
                error "This should never happen (list of more than 2 elements for combinations)" -- see `tuples` function
            f :: Maybe Error -> ((T.Text, Class), (T.Text, Class)) -> Maybe Error
            f Nothing ((id1, c1), (id2, c2)) =
              if classesOverlap c1 c2
                then
                  Just $
                    OverlappingClasses (allSubjectsMp M.! id1, c1) (allSubjectsMp M.! id2, c2)
                else Nothing
            f err _ = err

    foldingF :: ([Error], [[IDandSubj]]) -> [T.Text] -> ([Error], [[IDandSubj]])
    foldingF (errs, validSchedules) xs =
      case validateSubj xs of
        Just err -> (err : errs, validSchedules)
        Nothing ->
          let validCombination = fmap (\txtid -> IDandSubj (txtid, allSubjectsMp M.! txtid)) xs
           in (errs, validCombination : validSchedules)

program :: IO ()
program = do
  (res :: Either ParseException [IDandSubj]) <-
    decodeFileEither "test-english.yaml"
  sz <- size
  let layout = (\s -> LayoutOptions (AvailablePerLine s 1)) $
        case sz of
          Nothing -> 80
          Just (Window _ w) -> w
      prettyRender a = renderIO stdout $ layoutSmart layout a
  case res of
    Left err -> putStrLn $ prettyPrintParseException err
    Right result -> case collectValidationResults result of
      Left errs -> prettyRender (annotateErrors errs)
      Right lists -> do
        prettyRender (annotateSubjectLists lists)
        saveExcel lists

collectValidationResults :: [IDandSubj] -> Either [Error] [[IDandSubj]]
collectValidationResults xs = do
  (materias, db) <- first (: []) $ getMapsFromValues xs
  let allSubjectCombinations = genPossibleClassCombinations materias
  let validationResults = validate db allSubjectCombinations
  case validationResults of
    (errorList, []) -> Left errorList
    (_, successes) -> pure successes

-- pretty print

instance Pretty Class where
  pretty (MondayClass interval) = "Monday:   " <+> pretty interval
  pretty (TuesdayClass interval) = "Tuesday:  " <+> pretty interval
  pretty (WednesdayClass interval) = "Wednesday:" <+> pretty interval
  pretty (ThursdayClass interval) = "Thursday: " <+> pretty interval
  pretty (FridayClass interval) = "Friday:   " <+> pretty interval
  pretty (SaturdayClass interval) = "Saturday: " <+> pretty interval
  pretty (SundayClass interval) = "Sunday:   " <+> pretty interval

instance Pretty Hour where
  pretty H0 = "00"
  pretty H1 = "01"
  pretty H2 = "02"
  pretty H3 = "03"
  pretty H4 = "04"
  pretty H5 = "05"
  pretty H6 = "06"
  pretty H7 = "07"
  pretty H8 = "08"
  pretty H9 = "09"
  pretty H10 = "10"
  pretty H11 = "11"
  pretty H12 = "12"
  pretty H13 = "13"
  pretty H14 = "14"
  pretty H15 = "15"
  pretty H16 = "16"
  pretty H17 = "17"
  pretty H18 = "18"
  pretty H19 = "19"
  pretty H20 = "20"
  pretty H21 = "21"
  pretty H22 = "22"
  pretty H23 = "23"

instance Pretty Minute where
  pretty ZeroMinutes = "00"
  pretty HalfAnHour = "30"

instance Pretty Time where
  pretty (MkTime h m) = pretty h <> ":" <> pretty m

instance Pretty Interval where
  pretty (MkInterval start end) = pretty start <+> "-" <+> pretty end

instance Pretty Subject where
  pretty (MkSubject name professor classes) =
    "Subject: "
      <> pretty name
      <> line
      <> "Professor: "
      <> pretty professor
      <> line
      <> "Classes:"
      <> line
      <> indent 2 (vsep (map pretty classes))

instance Pretty Error where
  pretty (OverlappingClasses (subj1, class1) (subj2, class2)) =
    "Overlapping classes:"
      <> line
      <> indent 2 (vsep [pretty subj1 <+> pretty class1, pretty subj2 <+> pretty class2])
  pretty (RepeatedSubjId subjId subj1 subj2) =
    "Repeated subject ID: "
      <> pretty subjId
      <> line
      <> indent 2 (vsep [pretty subj1, pretty subj2])

annotateClass :: Class -> Doc AnsiStyle
annotateClass (MondayClass interval) = annotate (color Cyan <> bold) "Monday:   " <+> pretty interval
annotateClass (TuesdayClass interval) = annotate (color Cyan <> bold) "Tuesday:  " <+> pretty interval
annotateClass (WednesdayClass interval) = annotate (color Cyan <> bold) "Wednesday:" <+> pretty interval
annotateClass (ThursdayClass interval) = annotate (color Cyan <> bold) "Thursday: " <+> pretty interval
annotateClass (FridayClass interval) = annotate (color Cyan <> bold) "Friday:   " <+> pretty interval
annotateClass (SaturdayClass interval) = annotate (color Cyan <> bold) "Saturday: " <+> pretty interval
annotateClass (SundayClass interval) = annotate (color Cyan <> bold) "Sunday:   " <+> pretty interval

annotateSubject :: IDandSubj -> Doc AnsiStyle
annotateSubject (IDandSubj (txtid, MkSubject name professor classes)) =
  vsep
    [ annotateFieldName "Class ID: " <+> pretty txtid
    , annotateFieldName "Subject:  " <+> pretty name
    , annotateFieldName "Professor:" <+> pretty professor
    , annotateFieldName "Classes:"
    , indent 2 (vsep (map annotateClass classes))
    ]
  where
    annotateFieldName = annotate (color Blue <> bold)

annotateError :: Error -> Doc AnsiStyle
annotateError (OverlappingClasses (subj1, class1) (subj2, class2)) =
  annotate (color Red <> bold) "Overlapping classes:"
    <> line
    <> indent 2 (vsep [pretty subj1 <+> pretty class1, pretty subj2 <+> pretty class2])
annotateError (RepeatedSubjId subjId subj1 subj2) =
  annotate (color Red <> bold) "Repeated subject ID: "
    <> pretty subjId
    <> line
    <> indent 2 (vsep [pretty subj1, pretty subj2])

separateWith
  :: AnsiStyle -> Char -> Int -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
separateWith lineStyle lineChar numOfLines l r = l <> emptyLines <> separatingLines <> emptyLines <> r
  where
    separatingLines =
      vsep $
        replicate numOfLines $
          annotate lineStyle $
            pretty (replicate 30 lineChar)
    emptyLines =
      if numOfLines >= 2
        then line <> line
        else line

annotateErrors :: [Error] -> Doc AnsiStyle
annotateErrors es =
  annotate
    (color Red <> bold)
    (concatWith (separateWith bold '-' 1) (map annotateError es))

annotateSubjectList :: [IDandSubj] -> Doc AnsiStyle
annotateSubjectList ss = concatWith (separateWith (colorDull Yellow) '-' 1) (map annotateSubject ss)

annotateSubjectLists :: [[IDandSubj]] -> Doc AnsiStyle
annotateSubjectLists ss =
  concatWith
    (separateWith (color Magenta <> bold) '=' 2)
    (map annotateSubjectList ss)
    <> line

dayCoord :: Int -> CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
dayCoord col cval row = (row, ColumnIndex col, cval)
{-# INLINE dayCoord #-}

mondayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
mondayCoord = dayCoord 2

tuesdayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
tuesdayCoord = dayCoord 3

wednesdayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
wednesdayCoord = dayCoord 4

thursdayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
thursdayCoord = dayCoord 5

fridayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
fridayCoord = dayCoord 6

timeCoord :: Time -> RowIndex
timeCoord t = RowIndex $ fromEnum t + 2
{-# INLINE timeCoord #-}

getRowsFromInterval :: Interval -> [RowIndex]
getRowsFromInterval (MkInterval startingT endingT) = RowIndex . fromEnum <$> restOfTimes
  where
    restOfTimes = [startingT .. endingT]

getRowColumnCoordsOfClass
  :: CellValue -> Class -> [(RowIndex, ColumnIndex, CellValue)]
getRowColumnCoordsOfClass cellVal c = case c of
  MondayClass i -> mondayCoord cellVal <$> getRowsFromInterval i
  TuesdayClass i -> tuesdayCoord cellVal <$> getRowsFromInterval i
  WednesdayClass i -> wednesdayCoord cellVal <$> getRowsFromInterval i
  ThursdayClass i -> thursdayCoord cellVal <$> getRowsFromInterval i
  FridayClass i -> fridayCoord cellVal <$> getRowsFromInterval i
  _ -> error "more days not implemented"

writeSubjInWorksheet :: Worksheet -> IDandSubj -> Worksheet
writeSubjInWorksheet worksheet (IDandSubj (subjId, MkSubject {subjName, subjProfessor, subjclasses})) =
  foldl'
    (\ws (row, col, val) -> ws & cellValueAt (row, col) ?~ val)
    worksheet
    worksheetPositionsOfClasses
  where
    worksheetPositionsOfClasses :: [(RowIndex, ColumnIndex, CellValue)]
    worksheetPositionsOfClasses = concatMap (getRowColumnCoordsOfClass cellMsg) subjclasses

    cellMsg :: CellValue
    cellMsg = CellText $ subjName <> "\n(" <> subjId <> ")\n" <> subjProfessor

writeValidSchedule :: Worksheet -> [IDandSubj] -> Worksheet
writeValidSchedule = foldl' writeSubjInWorksheet

writeXlsxValidSchedules :: [[IDandSubj]] -> Xlsx
writeXlsxValidSchedules xs =
  fst
    ( foldl'
        ( \(xlsx, c :: Int) x ->
            let finalSheet = writeValidSchedule initialSheetStyled x
             in ( xlsx
                    & atSheet ("Schedule-" <> T.pack (show c)) ?~ finalSheet
                , c + 1
                )
        )
        (def, 1)
        xs
    )
    & xlStyles .~ renderStyleSheet finalStylesheet
  where
    trans :: Cell -> FormattedCell
    trans c =
      let myBorderStyle :: BorderStyle
          myBorderStyle =
            def
              & borderStyleColor ?~ (def & colorAutomatic ?~ True)
              & borderStyleLine ?~ LineStyleThin
       in def
            & formattedCell .~ c
            & formattedFormat
              .~ ( def
                    & formatAlignment
                      ?~ ( def -- Alignment
                            & alignmentWrapText ?~ True
                         )
                    & formatBorder
                      ?~ ( def -- Border
                            & borderTop ?~ myBorderStyle
                            & borderBottom ?~ myBorderStyle
                            & borderLeft ?~ myBorderStyle
                            & borderRight ?~ myBorderStyle
                            & borderVertical ?~ myBorderStyle
                         )
                 )
    initialSheetNoStyled =
      def
        & wsCells .~ M.fromList [((y, x), def) | x <- [1 .. 6], y <- [1 .. 49]]
        & cellValueAt (1, 2) ?~ CellText "Monday"
        & cellValueAt (1, 3) ?~ CellText "Tuesday"
        & cellValueAt (1, 4) ?~ CellText "Wednesday"
        & cellValueAt (1, 5) ?~ CellText "Thursday"
        & cellValueAt (1, 6) ?~ CellText "Friday"
        & \ws ->
          foldl'
            (\ws' (row, col, val) -> ws' & cellValueAt (row, col) ?~ val)
            ws
            timeAnnotations
    (Formatted finalCellMap finalStylesheet finalMerges) = formatted (trans <$> (initialSheetNoStyled ^. wsCells)) minimalStyleSheet
    initialSheetStyled =
      initialSheetNoStyled
        & wsCells .~ finalCellMap

timeAnnotations :: [(RowIndex, ColumnIndex, CellValue)]
timeAnnotations =
  [ (RowIndex $ fromEnum v + 2, ColumnIndex 1, CellText $ T.pack $ show v)
  | v <- [minBound :: Time .. maxBound]
  ]

saveExcel :: [[IDandSubj]] -> IO ()
saveExcel xs = do
  ct <- getPOSIXTime
  let xlsx = writeXlsxValidSchedules xs
  L.writeFile "schedules.xlsx" $ fromXlsx ct xlsx
