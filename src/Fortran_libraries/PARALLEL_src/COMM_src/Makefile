#
#
#

COMMDIR = $$(PARA_SRCDIR)/COMM_src
SOURCES = $(shell ls *.f90)
MOD_COMM = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'COMMDIR = $(COMMDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_COMM)' >> $(MAKENAME)


mod_list:
	@echo  MOD_COMM=  \\ >> $(MAKENAME)
	@echo  $(MOD_COMM) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(COMMDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
