[![Build](https://github.com/0rphee/schedule-maker/actions/workflows/pre-release.yml/badge.svg)](https://github.com/0rphee/schedule-maker/actions/workflows/pre-release.yml)

# schedule-maker
A command-line utility to create your school schedules.

Creates an .xlsx file with your school schedules from a .yaml file with your classes.

<!-- DO NOT REMOVE ANNOTATIONS, GITHUB ACTIONS DEPENDS ON THEM -->
<!-- REPLACE_EXE_HELP -->
```
schedule-maker - a command-line utility to create your school schedules.

Usage: schedule-maker (--print-yaml-example LANGUAGE | FILENAME 
                        [-p|--pretty-print] [-o|--output FILENAME])

  Create an .xlsx file with your school schedules from a .yaml file with your
  classes.

Available options:
  --print-yaml-example LANGUAGE
                           Language option: 'es' (spanish) or 'en' (english)
  FILENAME                 YAML input file
  -p,--pretty-print        Print to stdout the validated schedules
  -o,--output FILENAME     Write output to FILE (.xlsx)
                           (default: "schedules.xlsx")
  -h,--help                Show this help text

still a work in progress, source code here:
https://github.com/0rphee/schedule-maker
```

## English yaml

```yaml
# Example yaml file with classes

# There may be multiple entries for classes with the same subject name (ex. 'Diferential Calculus'), but the resulting schedules will only have 1 class of each type.
- name: Subject1
  class-id: "0001" # This can be any string, but it should be unique to each class.
  professor: John Doe
  days:
    - day: monday
      start: 17:30
      end: 19:00
    - day: tuesday
      start: 17:30
      end: 19:00
    - day: thursday
      start: 17:30
      end: 19:00
    - day: wednesday
      start: 7:00
      end: 8:30
    - day: friday
      start: 7:00
      end: 8:30
    - day: saturday
      start: 7:00
      end: 8:30
    - day: sunday
      start: 7:00
      end: 8:30
- name: Subject2
  class-id: "0002"
  professor: Robert Cohen
  days:
    - day: monday
      start: 7:00
      end: 8:30
    - day: friday
      start: 7:00
      end: 8:30
```

## Spanish yaml

```yaml
# Ejemplo de archivo yaml con clases

# Puede haber entradas múltiples para clases con el mismo nombre de materia (ej. 'Cálculo diferencial'), pero los horarios resultantes solo tendrán 1 clase de cada materia.
- nombre: Materia1
  id-clase: "0001" # Esta puede ser cualquier cadena, pero debe ser única para cada clase.
  profesor: Roberto Vega
  dias:
    # Los días de la semana NO pueden estar acentuados.
    - dia: lunes
      inicio: 17:30
      final: 19:00
    - dia: martes
      inicio: 17:30
      final: 19:00
    - dia: miercoles
      inicio: 7:00
      final: 8:30
    - dia: jueves
      inicio: 17:30
      final: 19:00
    - dia: viernes
      inicio: 7:00
      final: 8:30
    - dia: sabado
      inicio: 7:00
      final: 8:30
    - dia: domingo
      inicio: 7:00
      final: 8:30
- nombre: Materia2
  id-clase: "0002"
  profesor: Juan Miranda
  dias:
    - dia: lunes
      inicio: 7:00
      final: 8:30
    - dia: viernes
      inicio: 7:00
      final: 8:30
```
