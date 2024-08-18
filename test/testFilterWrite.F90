Program testFilterWrite
!! Program to test reading an IGES file and extracting user specified entities
!! to new DE and PE lists and then using the lists to create a new output file.
!! The surf128.igs file from test_files directory is used for these tests
!! because it contains both transformation matrix entities (124) and NURBS
!! surface entities(128).
!! Tests the following capabilities
!! 1. Read iges file ./test_files/surf128.igs and create an IGES_t object
!! 2. Creates DE and PE lists from the IGES_t object arrays that are filtered
!!    to contain just the 124 and 128 entities in the input file
!! 3. Converts the lists into arrays
!! 4. Uses the arrays to create the D and P section record arrays.
!! 5. Outputs a new IGES file name "new_surf128.igs"
 

  USE forIGES

  Implicit NONE

  Integer                    :: i, np, nd
  Character(:), Allocatable  :: in_filename
  Character(:), Allocatable  :: newFilename

  Integer                     :: types(2) 
  Type(IGES_t)                :: igesin
  Type(PElist_t)              :: newPlist
  Type(DElist_t)              :: newDlist
  Type(Gsection_t)            :: G
  Type(PE_t),     Allocatable :: PE(:)
  Type(Dentry_T), Allocatable :: DE(:)
  Character(80),  Allocatable :: S(:)

  Character(80), Allocatable :: Ssection(:)
  Character(80), Allocatable :: Gsection(:)
  Character(80), Allocatable :: Dsection(:)
  Character(80), Allocatable :: Psection(:) 

  Write(stdout, *) '' 
  Write(stdout, *) ' ***** Test of IGES array entity filter  *****' 
  Write(stdout, *) ''

! Use surf128.igs for this test because it contains both tranformation
! matricies and NURBS surfaces
 
  in_filename = "./test_files/surf128.igs" 
  newFilename = "new_surf128.igs"

! Extract the transformation matrices (124) and the NURBS surfaces(128)

  types = [124, 128]

  Write(stdout, *) ' Creating IGES Model From File'
  Write(stdout,*) ''

  Call igesin%readFile(in_filename)

  Write(stdout,*) ''
  Write(stdout, *) ' Echoing IGES Input Model Data To stdout'
  Write(stdout,*) ''

  Call igesin%output()

! Filter out non 124 or 128 entities from current igesin class and create
! new lists  

  Call filterDParrays(igesin%D%DE, igesin%P%PE, newDlist, newPlist, types)  

!  Write(stdout,*) ''
!  Write(stdout,*) ' ** Echoing new DElist contents to stdout **'
!  Write(stdout,*) ''
!  Call newDlist%printList()
!  Write(stdout,*) ''
!  Write(stdout,*) ' ** Echoing new PElist contents to stdout **'
!  Write(stdout,*) ''
!  Call newPlist%printList()

! Now create array versions of newDlist and newPlist using copyDlistToArray
! and copyPlistToArray

  Call copyDlistToArray(newDlist, DE)
  Call copyPlistToArray(newPlist, PE)

! Now create the individual IGES output file sections

! First create an S section

! Define a one line S section for testing

  ALLOCATE(S(3))

  S(1) = REPEAT(" ",80)
  S(2) = REPEAT(" ",80)
  S(3) = REPEAT(" ",80)
  S(1) = " Test of creation and output of a new IGES file from filtering test"
  S(2) = " file surf128.igs to extract just the transformation matrix and"
  S(3) = " NURBS surface entities"

  Call makeSsection(S, Ssection)  

! Next create a G section using default G class values and newFilename

  Call G%init(filename=newFilename)
  Call makeGsection(G, Gsection)

! Next create the D and P sections from DE and PE arrays

  Call makeDPsections(DE, PE, Dsection, Psection)

! Set number of decimal points output for all real values in output file.
! Default is 9 decimal places. We set it to 7 here to reduce the size of
! the output file.

  Call setRealFormat(7)

! Write new IGES file  

  Call writeIGESfile(newFilename, Ssection, Gsection, Dsection, Psection)
  Call igesin%dealloc()
  Call newPlist%delete()
  Call newDlist%delete()
 
  STOP

End Program testFilterWrite
