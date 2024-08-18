SHELL=/bin/sh

include $(CONFIGPATH)/paths
include $(CONFIGPATH)/makevars.$(COMPILERVARS)
include $(CONFIGPATH)/Liblist
include $(CONFIGPATH)/testlist

default: $(TARGETLIBNAME) 

$(TARGETLIBNAME): $(LIBLIST)

$(LIBLIST): FORCE
	cd $(BASEPATH)/src; $(MAKE) TARGETLIB=$(TARGETLIBNAME)

tests: FORCE
	cd $(BASEPATH)/test; $(MAKE) all

clean: FORCE
	cd $(BASEPATH)/src; $(MAKE) clean

cleantestdir: FORCE
	cd $(BASEPATH)/test; $(MAKE) clean; \

cleantest: cleantestdir
	rm -f $(BASEPATH)/test/*~

cleanlib: FORCE
	rm -f $(BASEPATH)/lib/*.a $(BASEPATH)/lib/*.so

cleanbin: FORCE
	rm -f $(BASEPATH)/bin/*.x

cleanconfig: FORCE
	rm -f $(BASEPATH)/config/*~

cleanmods: FORCE
	rm -f $(BASEPATH)/modules/*.$(MOD) $(BASEPATH)/modules/*.$(SMOD)

cleanbase: FORCE
	rm -f $(BASEPATH)/*~

cleanall: clean cleanlib cleanmods cleanconfig cleanbase cleanbin cleantestdir

FORCE:
