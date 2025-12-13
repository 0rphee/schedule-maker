{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Validation (collectValidationResults, runProgLogic) where

import CmdLineOpts
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (forM_)
import Data.List (foldl', tails)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Yaml
import PPrint
import Prettyprinter
import Prettyprinter.Render.Terminal (renderIO)
import System.Console.Terminal.Size
import System.IO (stdout)
import Types
import WriteXlsx (saveExcel)
import WriteiCal (saveMultipleICals)

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
validate allSubjectsMp = foldl' foldingF ([], [])
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
        validateClasses' allClasses = foldl' f Nothing pairCombinations
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

collectValidationResults :: [IDandSubj] -> Either [Error] [[IDandSubj]]
collectValidationResults xs = do
  (!materias, !db) <- first (: []) $ getMapsFromValues xs
  let allSubjectCombinations = genPossibleClassCombinations materias
  let validationResults = validate db allSubjectCombinations
  case validationResults of
    (!errorList, []) -> Left errorList
    (_, !successes) -> pure successes

runProgLogic :: Options -> IO ()
runProgLogic = \case
  PrintExampleYaml lang -> printYaml lang
  NormalOptions yamlSource prettyPrintToStdout outputFilePath writeICals -> do
    res <- decodeFileEither yamlSource -- "test-english.yaml"
    sz <-
      size >>= \case
        Nothing -> pure 80
        Just (Window _ w) -> pure w

    let layout = LayoutOptions (AvailablePerLine sz 1)
        prettyRender a = renderIO stdout $ layoutSmart layout a

    case res of
      Left err -> putStrLn $ prettyPrintParseException err -- yaml parsing errors
      Right result -> do
        when writeICals $ forM_ result $ \idandsubj@(IDandSubj (i, s)) ->
          saveMultipleICals [[idandsubj]] (T.unpack $ s.subjName <> "(" <> i <> ")")
        case collectValidationResults result of
          Left errs -> prettyRender (annotateErrors errs) -- validation errors
          Right lists -> do
            when prettyPrintToStdout $ prettyRender (annotateSubjectLists lists)
            when writeICals $ saveMultipleICals lists "schedule"
            saveExcel lists outputFilePath
