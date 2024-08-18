Program testWriteFile
 !! Program to test reading an IGES file,  creating entity objects, and then
 !! outputing a new IGES file containing the entities supported by forIGES.
 !! Tests the following capabilites:
 !! 1. Reading input and output filenames from the command line
 !! 2. Reading the input IGES file and creating an IGES_t object that holds all
 !!    of the individual section data for the file
 !! 3. Creation of a PE list using the PElist_t class
 !! 4. Defining the individual S, G, D, and P section record array
 !! 5. Setting the desired number of decimal places in REAL data output
 !! 6. Writing a new IGES file using the section record arrays 

  USE forIGES
  USE commandLineUtils, ONLY: getFileNames

  Implicit NONE

  Integer                        :: i, np
  Character(:), Allocatable      :: in_filename
  Character(:), Allocatable      :: IGES_filename

  Type(IGES_t)                   :: igesin

  Type(Gsection_t)               :: G

  Type(PElist_t)              :: PElist
  Type(Dentry_t), Allocatable :: D(:)
  Character(80),  Allocatable :: S(:)
  
  Character(80), Allocatable :: Ssection(:) 
  Character(80), Allocatable :: Gsection(:) 
  Character(80), Allocatable :: Dsection(:) 
  Character(80), Allocatable :: Psection(:)
 
  Write(stdout, *) '' 
  Write(stdout, *) ' ***** Test of IGES file creation  *****' 
  Write(stdout, *) ''

! Get input and output filenames from command line
 
  Call getFilenames(in_filename, IGES_filename)

  Write(stdout,*) ' Creating IGES Model From File'
  Write(stdout,*) ''

  Call igesin%readFile(in_filename)

  Write(stdout,*) ''
  Write(stdout, *) ' Echoing IGES Input Model Data To stdout'
  Write(stdout,*) ''

  Call igesin%output()

 ! Now define new IGES model using old model data

  np = SIZE(igesin%P%PE)

! Initialize P entity list and then add all input entities to list
! Could also use igesin%makePElist method. However it is assumed a
! user will want to create a list of new entities instead of just
! copying an existing IGES_t instance

  Call PElist%init()
  Do i=1, np
    Call PElist%append(igesin%P%PE(i)%entity)
  End Do

! Define a one line S section for testing

  ALLOCATE(S(1))

  S(1) = REPEAT(" ",80)
  S(1) = " Test of creation and output of a new IGES file using forIGES" 

  Call makeSsection(S, Ssection) ! adds a sequence number field to S(1)

! Create a new G section based on default values and filename

  Call G%init(filename=IGES_filename) ! could also change other G values here
  Call makeGsection(G, Gsection)     

! Create default Dentries values for various non-geometric parameters
! Here we are just using the old values. Things like starting parameter
! sequence numbers and number of lines for associated parameter section
! are created based on requested output. Note for most user output you
! need to be careful about setting the transformation matrix value so
! it is consistent with the parameter entity that needs to be rotated.
! Also, the care should be taken seting the form number and the status numbers.
! Things like font pattern, weight, level and view etc. are best set to zero

  ALLOCATE(D(np)) 
  Do i=1,np
    D(i)%line_font_pattern     = igesin%D%DE(i)%line_font_pattern
    D(i)%line_weight_number    = igesin%D%DE(i)%line_weight_number
    D(i)%color_number          = igesin%D%DE(i)%color_number
    D(i)%level                 = igesin%D%DE(i)%level
    D(i)%view                  = igesin%D%DE(i)%view
    D(i)%status_numbers        = igesin%D%DE(i)%status_numbers
    D(i)%entity_sub_number     = igesin%D%DE(i)%entity_sub_number
    D(i)%transformation_matrix = igesin%D%DE(i)%transformation_matrix
    D(i)%form_number           = igesin%D%DE(i)%form_number
  End Do

! Here we will use a D array and a PElist to define the D and P sections.     
! Could also use a PE array with a D array or a DElist and a PElist

  Call makeDPsections(D, PElist, Dsection, Psection) 

! Set number of decimal points output for all real values in output file.
! Default is 9 decimal places. We set it to 7 here to reduce the size of
! the output file.

  Call setRealFormat(7)

! Write new IGES file  

  Call writeIGESfile(IGES_filename, Ssection, Gsection, Dsection, Psection)

! Clean up IGES etc

  Call igesin%dealloc()
  Call PElist%delete()
 
  STOP

End Program testWriteFile
