{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib
  ( LanguageMode (..)
  , Options (..)
  , execParser
  , options
  , program
  ) where

import Combinatorics                 ( tuples )

import Data.Aeson.Types              ( Object, parseFail, prependFailure,
                                       typeMismatch )
import Data.Bifunctor                ( Bifunctor (first) )
import Data.Map.Strict               qualified as M
import Data.Text                     qualified as T
import Data.Yaml                     ( FromJSON (..), ParseException,
                                       Value (..), decodeFileEither,
                                       prettyPrintParseException, (.:) )

import Options.Applicative           ( Alternative ((<|>)), Parser, ParserInfo,
                                       action, execParser, flag, footer,
                                       fullDesc, header, help, helper, info,
                                       long, metavar, strArgument, (<**>) )

import Prettyprinter                 ( Doc, LayoutOptions (LayoutOptions),
                                       PageWidth (AvailablePerLine),
                                       Pretty (pretty), annotate, concatWith,
                                       indent, layoutSmart, line, vsep, (<+>) )
import Prettyprinter.Render.Terminal ( AnsiStyle,
                                       Color (Blue, Cyan, Magenta, Red, Yellow),
                                       bold, color, colorDull, renderIO )

import System.Console.Terminal.Size  ( Window (Window), size )
import System.IO                     ( stdout )

-- parsing and language options
newtype Options
  = Options (LanguageMode, FilePath)

data LanguageMode
  = English
  | Spanish

options :: ParserInfo Options
options =  info (opts <**> helper)
  (  fullDesc
  <> header "schedule-maker - A tool to help you build your schedule."
  <> footer "For more info visit here: https://codeberg.org/0rphee/brainhuck"
  )

opts :: Parser Options
opts = curry Options <$> language <*> filePathOpts


language :: Parser LanguageMode
language = flag English Spanish
  (  long "es"
  <> help "Parses yaml file in spanish, instead of english"
  )

filePathOpts :: Parser FilePath
filePathOpts = strArgument
  (  metavar "FILENAME"
  <> help "Input file"
  <> action "directory"
  <> action "file"
  )



data Class
  = MondayClass { classInterval :: Interval
                }
  | TuesdayClass { classInterval :: Interval
                 }
  | WednesdayClass { classInterval :: Interval
                   }
  | ThursdayClass { classInterval :: Interval
                  }
  | FridayClass { classInterval :: Interval
                }
  | SaturdayClass { classInterval :: Interval
                  }
  | SundayClass { classInterval :: Interval
                }
  deriving (Show)

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
  deriving (Eq, Ord)

instance Show Hour where
 show x
  = case x of
    H0  -> "00"
    H1  -> "01"
    H2  -> "02"
    H3  -> "03"
    H4  -> "04"
    H5  -> "05"
    H6  -> "06"
    H7  -> "07"
    H8  -> "08"
    H9  -> "09"
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
  deriving (Eq, Ord)

instance Show Minute where
  show x
    = case x of
      ZeroMinutes -> "00"
      HalfAnHour  -> "30"

data Time
  = MkTime { timeHour   :: Hour
           , timeMinute :: Minute
           }
  deriving (Eq, Ord)

instance Show Time where
  show (MkTime hour minutes) = show hour <> ":" <> show minutes


data Interval
  = MkInterval { intervalStartingTime :: Time
               , intervalEndTime      :: Time
               }

class Functor w => JSONLanguageMode w where
  getJSONResult :: w v -> v
  putJSONResult :: v -> w v

newtype JSONParseSpanish v
  = ESparse v
  deriving (Functor)

instance JSONLanguageMode JSONParseSpanish where
  getJSONResult (ESparse v) = v
  putJSONResult = ESparse

newtype JSONParseEnglish a
  = ENparse a
  deriving (Functor)

instance JSONLanguageMode JSONParseEnglish where
  getJSONResult (ENparse v) = v
  putJSONResult = ENparse


instance Show Interval where
  show (MkInterval beginning end) = show beginning <> "-" <> show end

newtype IDandSubj
  = IDandSubj (T.Text, Subject)

data Subject
  = MkSubject { subjName      :: T.Text
              , subjProfessor :: T.Text
              , subjclasses   :: [Class]
                -- maybe will change to S.Seq
              }

instance Show Subject where
  show (MkSubject sname sprof classes) = "{ Subject name: " <> show sname
                              <> ", Subject professor: " <> show sprof  <> ", Subject classes "<> show classes <>  " }"

data Error
  = OverlappingClasses (Subject, Class) (Subject, Class)
  | RepeatedSubjId T.Text Subject Subject

createInterval :: Time -> Time -> Maybe Interval
createInterval x y = if x < y
                     then Just $ MkInterval x y
                     else Nothing

hourMap :: M.Map T.Text Hour
hourMap = M.fromList [ ("1",  H1)
                     , ("2",  H2)
                     , ("3",  H3)
                     , ("4",  H4)
                     , ("5",  H5)
                     , ("6",  H6)
                     , ("7",  H7)
                     , ("8",  H8)
                     , ("9",  H9)
                     , ("00", H0)
                     , ("00", H1)
                     , ("02", H2)
                     , ("03", H3)
                     , ("04", H4)
                     , ("05", H5)
                     , ("06", H6)
                     , ("07", H7)
                     , ("08", H8)
                     , ("09", H9)
                     , ("10", H10)
                     , ("11", H11)
                     , ("12", H12)
                     , ("13", H13)
                     , ("14", H14)
                     , ("15", H15)
                     , ("16", H16)
                     , ("17", H17)
                     , ("18", H18)
                     , ("19", H19)
                     , ("20", H20)
                     , ("21", H21)
                     , ("22", H22)
                     , ("23", H23)
                     ]

instance FromJSON Time where
  parseJSON (String str)
     = prependFailure "parsing Time failed, " $ case res of
       [x,y] -> MkTime <$> h x <*> t y
       []    -> parseFail "inexistent hour for class"
       _     -> parseFail "unexpected ':'. More than one ':'."
    where res =  T.splitOn ":" str
          h head' = case M.lookup head' hourMap of
                      Just hour -> pure hour
                      _    -> parseFail $ T.unpack $ "Invalid hour: " <> head'
          t tail' = case tail' of
                      "00" -> pure ZeroMinutes
                      "30" -> pure HalfAnHour
                      _    -> parseFail $ T.unpack $ "Invalid minutes: " <> tail'
  parseJSON invalid =
        prependFailure "parsing Time failed, "
            (typeMismatch "String" invalid)

instance FromJSON (JSONParseSpanish Interval) where
  parseJSON (Object obj) = do
    beginningTime <- obj .: "inicio"
    endTime       <- obj .: "final"
    case createInterval beginningTime endTime of
      Nothing              ->
                  parseFail $ "Intervalo inválido, clase termina: "
                            <> show beginningTime
                            <> ", antes de que comience: "
                            <> show endTime
      Just validInterval -> pure $ ESparse validInterval
  parseJSON invalid =
        prependFailure "lectura del Intervalo fallida, "
            (typeMismatch "Object" invalid)

instance FromJSON (JSONParseEnglish Interval) where
  parseJSON (Object obj) = do
    beginningTime <- obj .: "start"
    endTime       <- obj .: "end"
    case createInterval beginningTime endTime of
      Nothing              ->
                  parseFail $ "invalid Interval, class ends: "
                            <> show beginningTime
                            <> ", before it begins: "
                            <> show endTime
      Just validInterval -> pure $ ENparse validInterval
  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid)

dayMap :: M.Map T.Text (Interval -> Class)
dayMap = M.fromList [ ("lunes", MondayClass)
                    , ("martes", TuesdayClass)
                    , ("miercoles", WednesdayClass)
                    , ("jueves", ThursdayClass)
                    , ("viernes", FridayClass)
                    , ("sabado", SaturdayClass)
                    , ("domingo", SundayClass)
                    , ("monday", MondayClass)
                    , ("tuesday", TuesdayClass)
                    , ("wednesday", WednesdayClass)
                    , ("thursday", ThursdayClass)
                    , ("friday", FridayClass)
                    , ("saturday", SaturdayClass)
                    , ("sunday", SundayClass)
                    ]

parseClassGeneralized classFailureMsg dayParser dayFailureMsg invalidClassDay (Object obj)
  = prependFailure classFailureMsg $ do
  day <- dayParser obj
  (inter :: JSONLanguageMode lan => lan Interval) <- prependFailure (T.unpack $ dayFailureMsg <> day <> "', ") $ parseJSON (Object obj)
  case M.lookup day dayMap of
    Just constructor -> pure $ constructor <$> inter
    _                -> parseFail $ T.unpack $ invalidClassDay <> day
parseClassGeneralized _ _ _ _ invalid = putJSONResult <$>
  prependFailure "parsing Interval failed, "
      (typeMismatch "Object" invalid)

instance FromJSON (JSONParseSpanish Class) where
  parseJSON = parseClassGeneralized "lectura de Clase fallida, " (\obj -> obj .: "dia" <|> obj .: "día") "en día '" "Día de clase inválido: "

instance FromJSON (JSONParseEnglish Class) where
  parseJSON = parseClassGeneralized  "parsing Subject failed, " (.: "day") "in day '" "Invalid Class day: "


instance FromJSON (JSONParseEnglish IDandSubj) where
  parseJSON (Object obj) = prependFailure "parsing Subject failed, " $ do
    name      <- obj .: "name"
    let errorInClassName = "in class '" <> T.unpack name <> "', "
    classId   <- prependFailure errorInClassName $ obj .: "class-id"
    let errorInClassId = errorInClassName
                      <> "with ID '" <> T.unpack classId <> "', "
    professor <- prependFailure errorInClassId $ obj .: "professor"
    let errorInClassProfessor = errorInClassId
                              <> ("with Professor: '" <> T.unpack professor <> "', ")
    (classes :: [JSONParseEnglish Class]) <- prependFailure errorInClassProfessor $ obj .: "days"
    pure . ENparse $ IDandSubj (classId, MkSubject name professor (getJSONResult <$> classes))

  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid)

instance FromJSON (JSONParseSpanish IDandSubj) where
  parseJSON (Object obj) = prependFailure "lectura de Materia fallida, " $ do
    name      <- obj .: "nombre"
    let errorInClassName = "en clase'" <> T.unpack name <> "', "
    classId   <- prependFailure errorInClassName $ obj .: "id-clase"
    let errorInClassId = errorInClassName
                      <> "con ID '" <> T.unpack classId <> "', "
    professor <- prependFailure errorInClassId $ obj .: "profesor"
    let errorInClassProfessor = errorInClassId
                              <> ("con Profesor: '" <> T.unpack professor <> "', ")
    (classes :: [JSONParseSpanish Class]) <- prependFailure errorInClassProfessor $ obj .: "dias" <|> obj .: "días"
    pure . ESparse $ IDandSubj (classId, MkSubject name professor (getJSONResult <$> classes))

  parseJSON invalid =
        prependFailure "lectura de Intervalo fallida, "
            (typeMismatch "Object" invalid)

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (MkInterval a b) (MkInterval x y)
  | a == x && b == y = True
  | a < x && x < b   = True
  | a < y && y < b   = True
  | x < a && a < y   = True
  | x < b && b < y   = True
  | otherwise        = False

classesOverlap :: Class -> Class -> Bool
classesOverlap class1 class2
  = case (class1, class2) of
      (MondayClass inter1, MondayClass inter2)       -> intervalsOverlap inter1 inter2
      (TuesdayClass inter1, TuesdayClass inter2)     -> intervalsOverlap inter1 inter2
      (WednesdayClass inter1, WednesdayClass inter2) -> intervalsOverlap inter1 inter2
      (ThursdayClass inter1, ThursdayClass inter2)   -> intervalsOverlap inter1 inter2
      (FridayClass inter1, FridayClass inter2)       -> intervalsOverlap inter1 inter2
      (SaturdayClass inter1, SaturdayClass inter2)   -> intervalsOverlap inter1 inter2
      _                                                  -> False

getMapsFromValues :: [IDandSubj] -> Either Error (M.Map T.Text [T.Text], M.Map T.Text Subject)
getMapsFromValues values = go values (Right (M.empty, M.empty))
  where go :: [IDandSubj] -> Either Error (M.Map T.Text [T.Text], M.Map T.Text Subject) -> Either Error (M.Map T.Text [T.Text], M.Map T.Text Subject)
        go [] result = result
        go (IDandSubj (id', subj):xs) res
          = do
          (keyMap, valMap) <- res
          case M.lookup id' valMap of
            Just repeatedSubj -> Left $ RepeatedSubjId id' subj repeatedSubj
            Nothing -> go xs (Right (M.alter alteringFunc sName keyMap , M.insert id' subj valMap))
          where (MkSubject sName _ _) = subj
                alteringFunc :: Maybe [T.Text] -> Maybe [T.Text]
                alteringFunc (Just valueInside) = Just (id':valueInside)
                alteringFunc Nothing            = Just [id']

genPossibleClassCombinations :: (Applicative f)
                            =>  M.Map T.Text (f T.Text) -> f [T.Text] -- outpts the list of lists of ids as Text
genPossibleClassCombinations = sequenceA . M.elems
-- genPossibleClassCombinations = map M.elems . sequenceA

validate :: M.Map T.Text Subject -> [[T.Text]] -> ([Error], [[IDandSubj]]) -- (errors, and successes)
validate allSubjectsMp = foldl foldingF ([],[])
  where validateSubj :: [T.Text] -> Maybe Error
        validateSubj combinations = validateClasses' asClasses -- combinations: ["1243", "1445", ..]
          where asClasses = mconcat $ fmap (\txtId -> (txtId, ) <$> subjclasses (allSubjectsMp M.! txtId)) combinations

                validateClasses' :: [(T.Text, Class)] -> Maybe Error
                validateClasses' allClasses = foldl f Nothing pairCombinations
                  where pairCombinations = toTup <$> tuples 2 allClasses
                        toTup xs = case xs of
                                     [c1, c2] -> (c1, c2)
                                     _ -> error "This should never happen (list of more than 2 elements for combinations)" -- see `tuples` function
                        f :: Maybe Error -> ((T.Text, Class), (T.Text, Class)) -> Maybe Error
                        f Nothing ((id1, c1), (id2, c2)) = if classesOverlap c1 c2
                                                           then Just $ OverlappingClasses (allSubjectsMp M.! id1, c1) (allSubjectsMp M.! id2, c2)
                                                           else Nothing
                        f err _ = err

        foldingF :: ([Error], [[IDandSubj]]) -> [T.Text] -> ([Error], [[IDandSubj]])
        foldingF (errs, validSchedules) xs
          = case validateSubj xs of
              Just err -> (err:errs, validSchedules)
              Nothing  -> let validCombination = fmap (\ txtid -> IDandSubj (txtid, allSubjectsMp M.! txtid)) xs
                           in (errs, validCombination :validSchedules)

decodeSpanish :: FilePath -> IO (Either ParseException [IDandSubj])
decodeSpanish filePath = do
  res :: Either ParseException [JSONParseSpanish IDandSubj] <- decodeFileEither filePath
  pure $ fmap (getJSONResult <$>) res

decodeEnglish :: FilePath -> IO (Either ParseException [IDandSubj])
decodeEnglish filePath = do
  res :: Either ParseException [JSONParseEnglish IDandSubj] <- decodeFileEither filePath
  pure $ fmap (getJSONResult <$>) res


program :: LanguageMode -> FilePath -> IO ()
program lang filePath = let ?lang = lang in do
  res :: Either ParseException [IDandSubj] <- case lang of
                                                Spanish -> decodeSpanish filePath
                                                English -> decodeEnglish filePath
  terminalSize <- size
  let layout = (\s -> LayoutOptions (AvailablePerLine s 1) )
             $ case terminalSize of
                 Nothing           -> 80
                 Just (Window _ w) -> w
  case res of
    Left err     -> putStrLn $ prettyPrintParseException err
    Right result -> renderIO stdout $ layoutSmart layout $ either annotateErrors annotateSubjectLists $ collectValidationResults result

collectValidationResults :: [IDandSubj] -> Either [Error] [[IDandSubj]]
collectValidationResults xs = do
  (materias, db) <- first (:[]) $ getMapsFromValues xs
  let allSubjectCombinations = genPossibleClassCombinations materias
  let validationResults = validate db allSubjectCombinations
  case validationResults of
    (errorList, []) -> Left errorList
    (_, successes)  -> pure successes

instance Pretty Hour where
  pretty H0  = "00"
  pretty H1  = "01"
  pretty H2  = "02"
  pretty H3  = "03"
  pretty H4  = "04"
  pretty H5  = "05"
  pretty H6  = "06"
  pretty H7  = "07"
  pretty H8  = "08"
  pretty H9  = "09"
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
  pretty HalfAnHour  = "30"


instance Pretty Time where
  pretty (MkTime h m) = pretty h <> ":" <> pretty m

instance Pretty Interval where
  pretty (MkInterval start end) = pretty start <+> "-" <+> pretty end

annotateClass :: (?lang :: LanguageMode) => Class -> Doc AnsiStyle
annotateClass (MondayClass interval)    = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Lunes:    "
                English -> "Monday:   "
annotateClass (TuesdayClass interval)   = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Martes:   "
                English -> "Tuesday:  "
annotateClass (WednesdayClass interval) = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Miércoles:"
                English -> "Wednesday:"
annotateClass (ThursdayClass interval)  = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Jueves:   "
                English -> "Thursday: "
annotateClass (FridayClass interval)    = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Viernes:  "
                English -> "Friday:   "
annotateClass (SaturdayClass interval)  = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Sábado:   "
                English -> "Saturday: "

annotateClass (SundayClass interval)    = annotate (color Cyan <> bold) msg <+> pretty interval
  where msg = case ?lang of
                Spanish -> "Domingo:  "
                English -> "Sunday:   "


annotateSubject :: (?lang :: LanguageMode) => Subject -> Doc AnsiStyle
annotateSubject (MkSubject name professor classes)
  = vsep [ annotateFieldName subjMsg <+> pretty name
         , annotateFieldName profMsg  <+> pretty professor
         , annotateFieldName classsMsg
         , indent 2 (vsep (map annotateClass classes))]
  where annotateFieldName = annotate (color Blue <> bold)
        (subjMsg, profMsg, classsMsg)
          = case ?lang of
              Spanish -> ("Materia:    ", "Profesor:   ", "Clases:")
              English -> ("Subject:    ", "Professor:  ", "Classes:")


annotateIDandSubject :: (?lang :: LanguageMode) => IDandSubj -> Doc AnsiStyle
annotateIDandSubject (IDandSubj (txtid, subj))
  = vsep [ annotateFieldName classIdMsg <+> pretty txtid
         , annotateSubject subj
         ]
  where annotateFieldName = annotate (color Blue <> bold)
        classIdMsg
          = case ?lang of
              Spanish -> "ID de Clase:"
              English -> "Class ID:   "


annotateError :: (?lang :: LanguageMode) => Error -> Doc AnsiStyle
annotateError (OverlappingClasses (subj1, class1) (subj2, class2)) =
  annotate (color Red <> bold) msg <> line <>
  indent 2 (vsep [annotateSubject subj1 <+> annotateClass class1, annotateSubject subj2 <+> annotateClass class2])
  where msg = case ?lang of
                Spanish -> "Clases sobrepuestas:"
                English -> "Overlapping classes:"
annotateError (RepeatedSubjId subjId subj1 subj2) =
  annotate (color Red <> bold) msg <> pretty subjId <> line <>
  indent 2 (vsep [annotateSubject subj1, annotateSubject subj2])
  where msg = case ?lang of
                Spanish -> "ID de materia repetido:"
                English -> "Repeated subject ID: "


separateWith :: AnsiStyle -> Char -> Int -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
separateWith lineStyle lineChar numOfLines l r = l <> emptyLines  <> separatingLines <> emptyLines <> r
  where separatingLines = vsep $ replicate numOfLines $ annotate lineStyle $ pretty (replicate 30 lineChar)
        emptyLines = if numOfLines >= 2
                     then line <> line
                     else line


annotateErrors :: (?lang :: LanguageMode) => [Error] -> Doc AnsiStyle
annotateErrors es = annotate (color Red <> bold) (concatWith (separateWith bold '-' 1) (map annotateError es))

annotateSubjectList :: (?lang :: LanguageMode) => [IDandSubj] -> Doc AnsiStyle
annotateSubjectList ss = concatWith (separateWith (colorDull Yellow) '-' 1) (map annotateIDandSubject ss)
-- annotateSubjectList ss = fillSep (map annotateSubject ss)

annotateSubjectLists :: (?lang :: LanguageMode) => [[IDandSubj]] -> Doc AnsiStyle
annotateSubjectLists ss = concatWith (separateWith (color Magenta <> bold) '=' 2) (map annotateSubjectList ss) <> line


