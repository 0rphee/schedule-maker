[![test-gh-actions](https://github.com/0rphee/schedule-maker/actions/workflows/test-gh-actions.yml/badge.svg?branch=gh-actions)](https://github.com/0rphee/schedule-maker/actions/workflows/test-gh-actions.yml)

# schedule-maker
A command-line utility to create your school schedules.

Creates an .xlsx file with your school schedules from a .yaml file with your classes.

<!-- DO NOT REMOVE ANNOTATIONS, GITHUB ACTIONS DEPEND ON THEM -->
<!-- REPLACE_EXE_HELP -->
```
schedule-maker - a command-line utility to create your school schedules.

Usage: schedule-maker-exe (--print-yaml-example LANGUAGE | FILENAME 
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

