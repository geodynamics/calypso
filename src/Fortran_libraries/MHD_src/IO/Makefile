#
#
#

MHD_IO_DIR = $$(MHD_SRCDIR)/IO
SOURCES = $(shell ls *.f90 *.F90)

MOD_MHD_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MHD_IO'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_IO_DIR = $(MHD_IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MHD_IO)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_IO_DIR)' $(SOURCES)
