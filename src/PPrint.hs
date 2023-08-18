module PPrint (annotateSubjectLists, annotateError, annotateErrors, printYaml) where

import CmdLineOpts (ExampleYamlLanguage (..))
import Data.Text.IO qualified as T
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

-- print example yaml file with classes
printYaml :: ExampleYamlLanguage -> IO ()
printYaml lan = case lan of
  English -> printEnglish
  Spanish -> printSpanish
  where
    printEnglish =
      T.putStrLn
        "\n# Example yaml file with classes\n\n\
        \# There may be multiple entries for classes with the same subject name (ex. 'Diferential Calculus'), but the resulting schedules will only have 1 class of each type.\n\
        \- name: Subject1\n\
        \  class-id: \"0001\" # This can be any string, but it should be unique to each class.\n\
        \  professor: John Doe\n\
        \  days:\n\
        \    - day: monday\n\
        \      start: 17:30\n\
        \      end: 19:00\n\
        \    - day: tuesday\n\
        \      start: 17:30\n\
        \      end: 19:00\n\
        \    - day: thursday\n\
        \      start: 17:30\n\
        \      end: 19:00\n\
        \    - day: wednesday\n\
        \      start: 7:00\n\
        \      end: 8:30\n\
        \    - day: friday\n\
        \      start: 7:00\n\
        \      end: 8:30\n\
        \    - day: saturday\n\
        \      start: 7:00\n\
        \      end: 8:30\n\
        \    - day: sunday\n\
        \      start: 7:00\n\
        \      end: 8:30\n\
        \- name: Subject2\n\
        \  class-id: \"0002\"\n\
        \  professor: Robert Cohen\n\
        \  days:\n\
        \    - day: monday\n\
        \      start: 7:00\n\
        \      end: 8:30\n\
        \    - day: friday\n\
        \      start: 7:00\n\
        \      end: 8:30\n"
    printSpanish =
      T.putStrLn
        "\n# Ejemplo de archivo yaml con clases\n\n\
        \# Puede haber entradas múltiples para clases con el mismo nombre de materia (ej. 'Cálculo diferencial'), pero los horarios resultantes solo tendrán 1 clase de cada materia.\n\
        \- nombre: Materia1\n\
        \  id-clase: \"0001\" # Esta puede ser cualquier cadena, pero debe ser única para cada clase.\n\
        \  profesor: Roberto Vega\n\
        \  dias:\n\
        \    # Los días de la semana NO pueden estar acentuados.\n\
        \    - dia: lunes\n\
        \      inicio: 17:30\n\
        \      final: 19:00\n\
        \    - dia: martes\n\
        \      inicio: 17:30\n\
        \      final: 19:00\n\
        \    - dia: miercoles\n\
        \      inicio: 7:00\n\
        \      final: 8:30\n\
        \    - dia: jueves\n\
        \      inicio: 17:30\n\
        \      final: 19:00\n\
        \    - dia: viernes\n\
        \      inicio: 7:00\n\
        \      final: 8:30\n\
        \    - dia: sabado\n\
        \      inicio: 7:00\n\
        \      final: 8:30\n\
        \    - dia: domingo\n\
        \      inicio: 7:00\n\
        \      final: 8:30\n\
        \- nombre: Materia2\n\
        \  id-clase: \"0002\"\n\
        \  profesor: Juan Miranda\n\
        \  dias:\n\
        \    - dia: lunes\n\
        \      inicio: 7:00\n\
        \      final: 8:30\n\
        \    - dia: viernes\n\
        \      inicio: 7:00\n\
        \      final: 8:30\n"
