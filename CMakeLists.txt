# forIGES
# compiling with gfortran under win 10@64
# 20240819
# by ceanwang@gmail.com

cmake_minimum_required (VERSION 3.20)
set ( APPLICATION_ID forIGES )
project (${APPLICATION_ID})
enable_language(Fortran)

option(BUILD_TEST "Set ON if build test" ON)

set(EXECUTABLE_OUTPUT_PATH "../bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "../lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "../module")

link_directories("../lib")

# get all *.f90 files recursively
file(GLOB_RECURSE SRC_FILES "src/*.F90")
add_library(${APPLICATION_ID} ${SRC_FILES})

if (BUILD_TEST)
    add_executable(testArrayFilter test/testArrayFilter.F90)
    target_link_libraries (testArrayFilter ${APPLICATION_ID})
    add_executable(testFilterWrite test/testFilterWrite.F90)
    target_link_libraries (testFilterWrite ${APPLICATION_ID})
    add_executable(testGenNURBSsurf test/testGenNURBSsurf.F90)
    target_link_libraries (testGenNURBSsurf ${APPLICATION_ID})
    add_executable(testReadFile test/testReadFile.F90)
    target_link_libraries (testReadFile ${APPLICATION_ID})
    add_executable(testReadList test/testReadList.F90)
    target_link_libraries (testReadList ${APPLICATION_ID})
    add_executable(testWriteFile test/testWriteFile.F90)
    target_link_libraries (testWriteFile ${APPLICATION_ID})
endif (BUILD_TEST)
