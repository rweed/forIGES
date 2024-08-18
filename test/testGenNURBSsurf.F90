Program testGenNURBSsurf
 !! Program to creating a NURBS surface and outputing it to an IGES file
 !! Tests the following capabilites:
 !! 1. Using init methods to create a DE and a single 128 entity 
 !! 3. Creation of DE and PE lists to contain the D and P section entries 
 !! 4. Defining the individual S, G, D, and P section record arrays
 !! 5. Setting the desired number of decimal places in REAL data output
 !! 6. Writing a new IGES file using the section record arrays 

  USE forIGES

  Implicit NONE

  Type(Gsection_t)  :: G
  Type(Dentry_t)    :: D
  Type(entity128_t) :: surf128
  Type(DElist_t)    :: Dlist
  Type(PElist_t)    :: Plist

  Character(:),  Allocatable :: filename
  Character(80), Allocatable :: Ssec(:)
  
  Character(80), Allocatable :: Ssection(:) 
  Character(80), Allocatable :: Gsection(:) 
  Character(80), Allocatable :: Dsection(:) 
  Character(80), Allocatable :: Psection(:)

  Integer :: K1, K2, M1, M2, PROP1, PROP2, PROP3, PROP4, PROP5

  Real(WP), ALLOCATABLE :: S(:) 
  Real(WP), ALLOCATABLE :: T(:)
  Real(WP), ALLOCATABLE :: X(:,:)
  Real(WP), ALLOCATABLE :: Y(:,:) 
  Real(WP), ALLOCATABLE :: Z(:,:) 
  Real(WP), ALLOCATABLE :: W(:,:) 
  Real(WP)              :: U(0:1)
  Real(WP)              :: V(0:1)

  filename = "RogersBsurf.igs"

! The following data is taken from Rogers Mathematical Elements of Computer
! Graphics, p459 and defines a cubic B-spline surface

  K1    = 3
  K2    = 3
  M1    = 3
  M2    = 3
  PROP1 = 0
  PROP2 = 0
  PROP3 = 1
  PROP4 = 0
  PROP5 = 0

! Note array bounds for S etc. This is how they are defined in IGES 5.3 manual

  ALLOCATE(S(-M1:K1+1),   SOURCE = 0.0_WP) 
  ALLOCATE(T(-M2:K2+1),   SOURCE = 0.0_WP)
  ALLOCATE(W(0:K1, 0:K2), SOURCE = 0.0_WP)
  ALLOCATE(X(0:K1, 0:K2), SOURCE = 0.0_WP)
  ALLOCATE(Y(0:K1, 0:K2), SOURCE = 0.0_WP)
  ALLOCATE(Z(0:K1, 0:K2), SOURCE = 0.0_WP)

  S = REAL([0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 2.0, 2.0],WP)
  T = REAL([0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 2.0, 2.0],WP)
  W = 1.0_WP   ! B-spline so weights are all 1 or the same.
  X(:,0) = REAL([-15.0, -5.0, 5.0, 15.0],WP)
  X(:,1) = REAL([-15.0, -5.0, 5.0, 15.0],WP)
  X(:,2) = REAL([-15.0, -5.0, 5.0, 15.0],WP)
  X(:,3) = REAL([-15.0, -5.0, 5.0, 15.0],WP)

  Y(:,0) = REAL([0.0, 5.0,  5.0,  0.0],WP)
  Y(:,1) = REAL([5.0, 10.0, 10.0, 5.0],WP)
  Y(:,2) = REAL([5.0, 10.0, 10.0, 5.0],WP)
  Y(:,3) = REAL([0.0, 5.0,  5.0,  0.0],WP)

  Z(:,0) = REAL([ 15.0, 15.0, 15.0, 15.0],WP)
  Z(:,1) = REAL([ 5.0,   5.0,  5.0,  5.0],WP)
  Z(:,2) = REAL([-5.0,  -5.0, -5.0, -5.0],WP)
  Z(:,3) = REAL([-15.0,-15.0,-15.0,-15.0],WP)

  U = [0.0_WP, 2.0_WP]
  V = [0.0_WP, 2.0_WP]

! Call init method to build a entity128_t entity

  Call surf128%init(1, 0, K1, K2, M1, M2, PROP1, PROP2, PROP3, PROP4, PROP5,  &
                    S, T, W, X, Y, Z, U, V) 

! Call init method to build a Dentry_t entry

  Call D%init(entity_type=128, param_data=1, transformation_matrix=0,      &
              form_number=0)

! Create Plist and Dlist to hold the entity and directory entry
! Probably could have just made these single dimension arrays and skipped
! the lists but I think most folks will use a list to hold there data
! since you can add things without worrying about the array dimension

  Call Plist%init()   
  Call Dlist%init()
  Call Plist%append(surf128)
  Call Dlist%append(D)

! Set filename and sender_id to filename
 
  Call G%init(filename=filename)

! Define S section strings and make an Ssection

  ALLOCATE(Ssec(3))
  Ssec(1) = REPEAT(" ",80)
  Ssec(2) = REPEAT(" ",80)
  Ssec(3) = REPEAT(" ",80)
  Ssec(1) = " Test of creating an IGES file for a B-spline surface from" 
  Ssec(2) = " user data. The data comes from Rogers Mathematical Elements"      
  Ssec(3) = " for Computer Graphics, page 459"

  Call makeSsection(Ssec, Ssection)

! Make the G section

  Call makeGsection(G, Gsection)

! Make the D and P sections

  Call makeDPsections(Dlist, Plist, Dsection, Psection)

  Write(stdout, *) '' 
  Write(stdout, *) ' ***** Test of IGES file creation from user data *****' 
  Write(stdout, *) ''

! Set number of decimal points output for all real values in output file.
! Default is 9 decimal places. We set it to 7 here to reduce the size of
! the output file.

  Call setRealFormat(7)

! Write new IGES file  

  Call writeIGESfile(filename, Ssection, Gsection, Dsection, Psection)

! Clean up lists etc

  Call Plist%delete()
  Call Dlist%delete()
 
  STOP

End Program testGenNURBSsurf
