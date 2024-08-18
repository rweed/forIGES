Program testReadFile
! Program to test reading of a single IGES file and creating entity objects
!! Tests the following capabilities:
!! 1. getting input and output file names from command line
!! 2. Reading the IGES file and building an IGES_t class that contains all
!!    the information in the different file sections in the class components
!! 3. Echo the contents of the IGES_t class to stdout

  USE ISO_FORTRAN_ENV, stdin=>input_unit, stdout=>output_unit
  USE forIGES, ONLY: IGES_t
  USE commandLineUtils, ONLY: getFileNames

  Implicit NONE

  Character(:), ALLOCATABLE :: filename
  Character(:), ALLOCATABLE :: outfile 

  Type(IGES_t) :: iges
 
! Gets input and output file names from the command line. 
! testRead.x -i=input_filename -o=output_filename

  Call getFileNames(filename, outfile)
 
  Write(stdout, *) '' 
  Write(stdout, *) ' ***** Test of IGES Reader for a single file  *****' 
  Write(stdout, *) '' 

  Write(stdout, *) ' Creating IGES Model From File'
  Write(stdout,*) ''

  Call iges%readFile(filename)

  Write(stdout,*) ''
  Write(stdout, '(" Echoing IGES Model data To file : ",A)') outfile
  Write(stdout,*) ''

  Call iges%output(outfile)
  Call iges%dealloc()

  STOP

End Program testReadFile
