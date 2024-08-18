SHELL=/bin/sh

include $(THISDIR)/makefile.inc

default: $(TARGETLIBNAME) 

$(TARGETLIBNAME): $(LIBLIST)

