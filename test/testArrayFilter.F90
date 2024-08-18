Program testArrayFilter
!! Program to test reading an IGES file and extracting user specified entities
!! to new DE and PE lists and then echoing the list contents to stdout. The
!! surf128.igs file from test_files directory is used for these tests because
!! it contains both transformation matrix entities (124) and NURBS surface
!! entities
!! Tests the following capabilities:
!! 1. Read iges file ./test_files/surf128.igs and create an IGES_t object
!! 2. Creates DE and PE lists from the IGES_t object arrays that are filtered
!!    to contain just the 124 and 128 entities in the input file
!! 3. Output the contents of the new lists to stdout  

  USE forIGES

  Implicit NONE

  Integer                    :: i, np, nd
  Character(:), ALLOCATABLE  :: in_filename

  Integer        :: types(2) 
  Type(IGES_t)   :: igesin
  Type(PElist_t) :: newPlist
  Type(DElist_t) :: newDlist
  
  Write(stdout, *) '' 
  Write(stdout, *) ' ***** Test of IGES array entity filter  *****' 
  Write(stdout, *) ''

! Use surf128.igs for this test because it contains both tranformation
! matricies and NURBS surfaces
 
  in_filename = "./test_files/surf128.igs" 

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

  Write(stdout,*) ''
  Write(stdout,*) ' ** Echoing new DElist contents to stdout **'
  Write(stdout,*) ''
  Call newDlist%printList()
  Write(stdout,*) ''
  Write(stdout,*) ' ** Echoing new PElist contents to stdout **'
  Write(stdout,*) ''
  Call newPlist%printList()

  Call igesin%dealloc()
  Call newPlist%delete()
  Call newDlist%delete()
 
  STOP

End Program testArrayFilter
