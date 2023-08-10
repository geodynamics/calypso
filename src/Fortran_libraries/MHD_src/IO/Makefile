#
#
#

MHD_IO_DIR = $$(MHD_SRCDIR)/IO
SOURCES = $(shell ls *.f90 *.F90)
MOD_MHD_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_IO_DIR = $(MHD_IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_MHD_IO)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_MHD_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_MHD_IO) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_IO_DIR)' $(SOURCES)
