[![Build](https://github.com/0rphee/schedule-maker/actions/workflows/pre-release.yml/badge.svg)](https://github.com/0rphee/schedule-maker/actions/workflows/pre-release.yml)

# schedule-maker

A command-line utility to create your school schedules. Validates every possible combination of your school schedule verifying that there aren't any overlapping classes.

Creates an .xlsx file with your school schedules from a .yaml file with your classes.

Binaries for windows, mac, and linux (built on ubuntu) can be found in the Github releases page.

<!-- DO NOT REMOVE ANNOTATIONS, GITHUB ACTIONS DEPENDS ON THEM -->
<!-- REPLACE_EXE_HELP -->

```
schedule-maker - a command-line utility to create your school schedules.

Usage: schedule-maker (--print-yaml-example LANGUAGE | FILENAME
                        [-p|--pretty-print] [-o|--output FILENAME] [-i|--ical])

  Create an .xlsx file with your school schedules from a .yaml file with your
  classes.

Available options:
  --print-yaml-example LANGUAGE
                           Language option: 'es' (spanish) or 'en' (english)
  FILENAME                 YAML input file
  -p,--pretty-print        Print to stdout the validated schedules
  -o,--output FILENAME     Write output to FILE (.xlsx)
                           (default: "schedules.xlsx")
  -i,--ical                Write the schedules to iCal files (schedule1.ics,
                           schedule2.ics)
  -h,--help                Show this help text

still a work in progress, source code here:
https://github.com/0rphee/schedule-maker
```

(Examples of valid yaml to use this tool below.)

## Preview of .xlsx output

(This image only shows **one** of the generated _valid_ schedules generated from `test-english.yaml`, found in the test `schedules.xlsx` found in the repo.)

<img width="911" alt="img" src="https://github.com/0rphee/schedule-maker/assets/79347623/e97ff510-3167-459c-95fd-da8b40264357">

## Preview of .ics output

(This image only shows **one** of the generated _valid_ schedules generated from `test-english.yaml`, the program will one `schedule*.ics` per valid schedule, this files can be imported by calendar apps, like Google Calendar.)

<img width="1440" alt="ics-test" src="https://github.com/0rphee/schedule-maker/assets/79347623/b34c3487-9a62-4af6-b7de-341dd96018ec">

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
