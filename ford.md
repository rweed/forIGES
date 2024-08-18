project: forIGES
summary: A Modern Fortran Library for Reading and Creating IGES CAD files 
src_dir: ./src
output_dir: ./doc
project_github: https://github.com/rweed/forIGES
github: https://github.com/rweed
author: Richard Weed 
version: 0.1
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
display: public
         protected
         private
source: true
graph:  true
preprocessor: gfortran -E
preprocess: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
 
{!README.md!}
