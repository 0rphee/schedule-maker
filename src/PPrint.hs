module PPrint (annotateSubjectLists, annotateError, annotateErrors) where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Types

-- pretty print

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
