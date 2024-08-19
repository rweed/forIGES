### Description

forIGES is a set of Modern Fortran utilities for reading and writing a subset ofthe IGES CAD file entities described in the IGES 5.3 specification. The focus of forIGES is on geometric entities such as NURBS curves and surfaces. Therefore, the complete IGES specification is not supported. Users should obtain a copy of the IGES 5.3 specification prior to attempting to use this software. A PDF of the 5.3 specification can be obtained from [here](https://web.archive.org/web/20120821190122/http://www.uspro.org/documents/IGES5-3_forDownload.pdf) 

### Supported IGES entities

The following IGES entities are supported. The entities chosen are a mixture 
of entities supported by previous work documented in the References.
 
Type Description  

-   0  Null  
- 100  Circular  arc  
- 102  Composite curve  
- 104  Conic arc  
- 106  Copious data (forms 1-3, 11-13, 63)  
- 108  Plane  
- 110  Line  
- 112  Parametric Spline Curve  
- 114  Parametric Spline Surface   
- 116  Point  
- 118  Ruled Surface  
- 120  Surface of Revolution  
- 122  Tabulated Cylinder  
- 123  Direction  
- 124  Transformation Matrix  
- 126  Rational B-spline curve  
- 128  Rational B-spline surface  
- 130  Offset Curve  
- 140  Offset Surface  
- 141  Boundary  
- 142  Curve on a Parametric Surface  
- 143  Bounded Surface  
- 144  Trimmed (parametric) surface  
- 190  Plane Surface   
- 192  Right Circular Cylinder Surface  
- 194  Right Circular Conical Surface  
- 196  Spherical Surface  
- 198  Toroidal Surface  
- 212  General Note  
- 308  Subfigure Definition  
- 314  Color Definition  
- 402  Associativity Instance  
- 406  Property (forms 1 and 15)  
- 408  Singular Subfigure instance  
- 502  Vertex list  
- 504  Edge  
- 508  Loop  
- 510  Face  

### Building forIGES

Currently forIGES has only been built and tested on Linux systems. Support for Windows and Macs is planned for the future but is not currently available. However, given the relatively small number of files that make up forIGES it should be simple for Windows users to create their own Visual Studio project from the files in the src and test directories. forIGES has been built and tests run with the following compilers:

 > Intel    ifx       version 2024.1  
 > Intel    ifort     version 2021.12  
 > GCC      gfortran  version 13 and 14  
 > NVIDIA   nvfortran version 24.7  
 > AMD AOCC flang     version 16.0.3 

Both Intel compilers, GCC, and NVIDIA appear to compile without error and generate correct results for all the forIGES test cases. flang appears to compile without error and runs the read tests correctly but produces erroneous results for the IGES file creation tests so flang cannot be recommended at this time.  

#### Building forIGES Using Make

On Linux systems the easiest way to build forIGES is using the supplied makefiles and configure bash script.

The steps are as follows:

1. Run configure to set the desired compiler and optimization level. The defaults are gfortran and no optimization

2. Type make - this will build the library and associated mod files and place them in the lib and module directories. By default both an archive named libforIGES.a and a shared object named libforIGES.so are built

3. Type make tests to build the executables for the test programs.

>  Examples:  

>  default gfortran: 
>>  source ./configure

>  Intel compilers:  
>>  source ./configure  \-\-compiler=intel-ifort   
>>  or  
>>  source ./configure  \-\-compiler=intel-ifx

>   NVIDIA compilers:  
>>  source ./configure  \-\-compiler=nvidia

>   AMD compilers: 
>>  source ./configure  \-\-compiler=amd

>  make  
>  make tests  

>  make cleanall \- deletes all files not needed for a new build  
 
#### Building forIGES Using FPM

The forIGES libraries can also be built using the Fortran Package Manager [FPM](https://fpm.fortran-lang.org) 

To build using fpm do the following:  

>   fpm build  
>   fpm install --prefix=your_install_path  

These will install a libforIGES.a archive in lib and the mod files in include. The default compiler for FPM is gfortran. Other compilers can be used by setting the FPM_FC and FPM_FFLAGS environment variables.

### Building Code HTML Documentation with FORD

A complete set of HTML documentation describing the forIGES API can be generated using 
[FORD](https://forddocs.readthedocs.io). The resulting HTML pages will describe each subroutine and function interface along with definitions of various variables and derived types.

To build the FORD documentation on your local system type the following:  

>  ford ford.md  

The resulting documentation will be placed in a local doc directory

Note FORD and FPM are Python applications and can be installed using pipx or pip
FORD also requires a recent copy of graphViz.

### Using forIGES

The best way to learn how to use forIGES is to study the supplied test programs in the test directory. They illustrate the basics of reading and writing IGES files with forIGES.

### Getting Help, Reporting Bugs etc.

Please report any bugs, new feature requests etc. on the forIGES Github repository under issues.

### Future Plans

Based on user feedback, support for more entities can be added. A more detailed Users Guide in Markdown and PDF forms that gives an overview of the software and the procedures used to test it can be found in the ./doc directory. Support for Windows and Macs is also planned. The ultimate goal of this project is to provide a way to extract non-NURBS geometric entities from an IGES file and then convert them to a NURBS description (where possible) as was done in References 2 through 5.

### References

 1. US PRO, "Initial Graphics Exchange Specification IGES 5.3", 1996  <https://web.archive.org/web/20120821190122/http://www.uspro.org/documents/IGES5-3_forDownload.pdf>  

 2. Yu, Tzu-Yi, "CAGD Techniques in Grid Generation," Ph.D. Dissertation, Mississippi State University, Miss. State MS, December 1995  <https://ntrs.nasa.gov/api/citations/19970014211/downloads/19970014211.pdf> 

 3. Yu, T. and Soni, B., "NURBS in Structured Grid Generation", *Handbook of Grid Generation*, Chapter 30, Ed. J.F Thompson, et al, Taylor Francis Publishing, December 28, 1998 <https://doi.org/10.1201/9781420050349> also <http://ebrary.free.fr/Mesh%20Generation/Handbook_of_Grid_%20Generation,1999/chap30.pdf>  

 4. Blake, M., Chou, J., Kerr, P., Thorp, S., "NASA Geometry Data Exchange Specification for Computational Fluid Dynamics (NASA IGES)", NASA RP 1338, 1994  <https://ntrs.nasa.gov/api/citations/19950005054/downloads/19950005054.pdf>   

 5. Evans, A. and Miller D., "NASA IGES and NASA-IGES NURBS-Only Standard," *Handbook of Grid Generation*, Chapter 31, Ed. J.F Thompson, et al, Taylor Francis Publishing, December 28, 1998 <https://doi.org/10.1201/9781420050349>

 6. SINTEF GoTools <https://www.sintef.no/en/software/software-applied-mathematics/gotools/>   

