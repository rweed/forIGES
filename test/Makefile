include $(CONFIGPATH)/paths
include $(CONFIGPATH)/makevars.$(COMPILERVARS)

LIB=$(BASEPATH)/lib/libforIGES.a

EXE=testReadList.x testReadFile.x testWriteFile.x testAfilter.x testFilterWrite.x testGenSurf.x

all: $(EXE)

testReadList.x: testReadList.F90
	$(FC) -o ./testReadList.x $(FCFLAGS) $(INCLUDES) $(DEFINES) ./testReadList.F90 $(LIB)

testReadFile.x: testReadFile.F90
	$(FC) -o ./testReadFile.x $(FCFLAGS) $(INCLUDES) $(DEFINES) ./testReadFile.F90 $(LIB)

testWriteFile.x: testWriteFile.F90
	$(FC) -o ./testWriteFile.x $(FCFLAGS) $(INCLUDES) $(DEFINES) ./testWriteFile.F90 $(LIB)

testAfilter.x: testArrayFilter.F90
	$(FC) -o ./testAfilter.x $(FCFLAGS) $(INCLUDES) $(DEFINES) ./testArrayFilter.F90 $(LIB)

testFilterWrite.x: testFilterWrite.F90
	$(FC) -o ./testFilterWrite.x $(FCFLAGS) $(INCLUDES) $(DEFINES) ./testFilterWrite.F90 $(LIB)

testGenSurf.x: testGenNURBSsurf.F90
	$(FC) -o ./testGenSurf.x $(FCFLAGS) $(INCLUDES) $(DEFINES) ./testGenNURBSsurf.F90 $(LIB)

clean:
	rm -rf *.out ./*.o *.mod ./*.x ./*~
	rm -rf ./test_files/*.out
