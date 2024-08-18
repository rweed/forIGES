Program testReadList
!! Program to test reading of a list of IGES file and creating entity objects
!! Tests the following capabilities:
!! 1. getting list input file name from command line and setting output 
!!    filenames based on the input file names
!! 2. Reading the IGES file and building an IGES_t class that contains all
!!    the information in the different file sections in the class components
!! 3. Echo the contents of the IGES_t class to stdout

  USE ISO_FORTRAN_ENV, stdin=>input_unit, stdout=>output_unit
  USE forIGES,          ONLY: IGES_t
  USE commandLineUtils, ONLY: getFileNames

  Implicit NONE

  Integer                   :: i, nfiles, iend, tunit, idot
  Character(:), Allocatable :: testfile
  Character(80)             :: infile
  Character(:), Allocatable :: outfile
  Character(1)              :: dum
 
  Type(IGES_t) :: iges

! Assumes command line is 
! testIGES.x -i=filename
 
  Call getFileNames(testfile)

  Open(newunit=tunit,FILE=TRIM(ADJUSTL(testfile)), FORM="FORMATTED",         &
                     STATUS="OLD")

  nfiles = 0
  COUNT: Do
    dum = " "
    Read(tunit,'(A)', iostat=iend) dum
    If(iend /= 0) EXIT COUNT
    nfiles = nfiles+1
  End Do COUNT

  REWIND(tunit)

  Write(stdout,*) ''
  Write(stdout,'(" Testing reading of ",i0," IGES files")') nfiles
  Write(stdout,*) ''
 
  FILES: Do i=1,nfiles

    Read(tunit,'(A)', iostat=iend) infile
    If (iend /= 0) EXIT FILES

    Write(stdout,*) ''
    Write(stdout,'(" Processing input file : ",A)') TRIM(ADJUSTL(infile))
    Write(stdout,*) ''

    idot    = INDEX(infile,".igs") - 1
    outfile = TRIM(ADJUSTL(infile(1:idot))) // ".out" 
      
    Call iges%readFile(TRIM(ADJUSTL(infile)))

    Write(stdout,*) ''
    Write(stdout,'(" Output for this file is written to : ",A)') TRIM(ADJUSTL(outfile))
    Write(stdout,*) ''

    Call iges%output(outfile)
    Call iges%dealloc()

  End Do FILES

  STOP

End Program testReadList
