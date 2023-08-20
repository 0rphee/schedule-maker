module WriteXlsx (saveExcel) where

import Codec.Xlsx
import Codec.Xlsx.Formatted
import Control.Lens
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Foldable (foldl')
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Types

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

saturdayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
saturdayCoord = dayCoord 7

sundayCoord :: CellValue -> RowIndex -> (RowIndex, ColumnIndex, CellValue)
sundayCoord = dayCoord 8

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
  SaturdayClass i -> saturdayCoord cellVal <$> getRowsFromInterval i
  SundayClass i -> sundayCoord cellVal <$> getRowsFromInterval i

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
    cellMsg = CellText $ subjName <> " (" <> subjId <> ") " <> subjProfessor

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
        & wsCells .~ M.fromList [((y, x), def) | x <- [1 .. 8], y <- [1 .. 49]]
        & cellValueAt (1, 2) ?~ CellText "Monday"
        & cellValueAt (1, 3) ?~ CellText "Tuesday"
        & cellValueAt (1, 4) ?~ CellText "Wednesday"
        & cellValueAt (1, 5) ?~ CellText "Thursday"
        & cellValueAt (1, 6) ?~ CellText "Friday"
        & cellValueAt (1, 7) ?~ CellText "Saturday"
        & cellValueAt (1, 8) ?~ CellText "Sunday"
        & \ws ->
          foldl'
            (\ws' (row, col, val) -> ws' & cellValueAt (row, col) ?~ val)
            ws
            timeAnnotations
    (Formatted finalCellMap finalStylesheet finalMerges) = formatted (trans <$> (initialSheetNoStyled ^. wsCells)) minimalStyleSheet
    initialSheetStyled =
      initialSheetNoStyled
        & wsCells .~ finalCellMap
        & wsColumnsProperties
          .~ [ -- hour column
               ColumnsProperties
                { cpMin = 1
                , cpMax = 1
                , cpWidth = Just 6
                , cpStyle = Nothing
                , cpHidden = False
                , cpCollapsed = False
                , cpBestFit = True
                }
             , -- rest of columns
               ColumnsProperties
                { cpMin = 2
                , cpMax = 8
                , cpWidth = Just 20
                , cpStyle = Nothing
                , cpHidden = False
                , cpCollapsed = False
                , cpBestFit = True
                }
             ]

timeAnnotations :: [(RowIndex, ColumnIndex, CellValue)]
timeAnnotations =
  [ (RowIndex $ fromEnum v + 2, ColumnIndex 1, CellText $ T.pack $ show v)
  | v <- [minBound :: Time .. maxBound]
  ]

saveExcel :: [[IDandSubj]] -> FilePath -> IO ()
saveExcel xs outputFPath = do
  ct <- getPOSIXTime
  let xlsx = writeXlsxValidSchedules xs
  L.writeFile outputFPath $ fromXlsx ct xlsx
