include $(CONFIGPATH)/paths
include $(CONFIGPATH)/makevars.$(COMPILERVARS)
include ./objslist
include ./thislib.inc

$(THISLIB):
	make "FCFLAGS=$(FCFLAGS)" "DEFINES=$(DEFINES)" $(THISLIB).a $(THISLIB).so 
$(THISLIB).a: $(OBJS)
	$(AR) $@ $(OBJS)
	$(RANLIB) $@
	$(AR) $(LIBPATH)/$(TARGETLIB).a $(OBJS)
	$(RANLIB) $(LIBPATH)/$(TARGETLIB).a
	rm -f $(LIBPATH)/$@
	mv $@ $(LIBPATH)/$@

$(THISLIB).so: $(OBJS)
	$(FC) -shared $(LDFLAGS) -o $@ $(OBJS)
	rm -f $(LIBPATH)/$@
	mv $@ $(LIBPATH)/$@

include $(CONFIGPATH)/makerules.$(RULES)
