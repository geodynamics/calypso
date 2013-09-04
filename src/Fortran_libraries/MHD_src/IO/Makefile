#
#
#

MHD_IO_DIR = $$(MHD_SRCDIR)/IO
SRC_MHD_IO = $(shell ls *.f90)
MOD_MHD_IO = $(addsuffix .o,$(basename $(SRC_MHD_IO)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_IO_DIR = $(MHD_IO_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_MHD_IO)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_MHD_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_MHD_IO) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_IO_DIR)' $(SRC_MHD_IO)
