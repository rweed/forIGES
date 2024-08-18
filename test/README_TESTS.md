### Test programs and examples
The programs in this directory are used to test the basic capabilities of the forIGES utilities and serve as examples for how to do simple tasks. The
executables for these files are created by typing make tests after you build the
main libraries ie.

1. make
2. make tests

Each executable will have a .x file extension because I find that is the
safest way on Linux to make sure only the executables are deleted by a 
make cleanall

### Test program descriptions

####testReadList.F90
Reads a list of IGES files found in the test_files directory and then processes
each file to create a IGES_t object. Unsupported entities are flagged and
replaced by an unsupported placeholder. The data stored in each IGES_T object
is then printed out to a FILE.out file where FILE is the name of the IGES file
in the list.  

To run testReadList type:

testReadList.x -i=tfiles.lst

where testlist.in holds a list of IGES filenames (with paths)

####testReadFile.F90

Reads a single IGES files then processes each file to create a IGES_t object. 
Unsupported entities are flagged and replaced by an unsupported placeholer.
The data stored in the resulting IGES_t object is then printed out to a user
specified file.  

To run testReadFile type:

testReadFile.x -i=input.igs -o=output.igs

#### testWriteFile.F90

Reads an IGES file, creates and IGES_t object and then writes the IGES_t object
to an new IGES file without the unsupported entities

To run testWriteFile type

testWriteFile.x -i=input.igs -o=output.igs

#### testArrayFilter.F90

Reads an IGES file and generates lists of Directory section entries and 
Parameter section entities that are contain only the entity types defined in
an array by the user. The list contents are echoed to stdout. The IGES file
that is read is hardwired to ./test_files/surf128.igs because it contains both
transformation array entities and NURBS surface entities which are extracted
from the input file

To run testArrayFilter type

testAfilter.x

#### testFilterWrite.F90

testFilterWrite extends testArrayFilter to create a new IGES file based on the
data in the filtered lists.

To run testFilterWrite type

testFilterWrite.x

#### testGenNURBSsurf.F90

testGenNURBSsurf generates a cubic B-spline surface using data from Rogers
Mathematical Elements of Computer Graphics, p459. It illustrates the case
where a user creates there own NURBS surface and wants to output it to a CAD
system. A file named RogersBspline.igs will be created

To run testGenNURBSsurf type

testGenSurf.x

