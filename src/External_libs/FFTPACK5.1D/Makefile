#
#
#

FFTPACK_DIR = $$(EXT_DIR)/FFTPACK5.1D

LIB_FFTPACK =       -lfftpack.5d
LIB_FFTPACK_FILE = libfftpack.5d.a

FFTPACKFLAG = $(subst '-warn all','',$(F90OPTFLAGS))

SRC_FFTPACK = fftpack5.11d.f90
MOD_FFTPACK = fftpack5.11d.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo FFTPACKFLAG =  '$$(subst ''-warn all'','' '',$$(F90OPTFLAGS))' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'FFTPACK_DIR =  $(FFTPACK_DIR)'   >> $(MAKENAME)

lib_name:
	@echo  'LIB_FFTPACK =      $(LIB_FFTPACK)'      >> $(MAKENAME)
	@echo  'LIB_FFTPACK_FILE = $(LIB_FFTPACK_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_FFTPACK_FILE): $$(MOD_FFTPACK)'>> $(MAKENAME)

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FFTPACK)' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFTPACK=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFTPACK) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:

module:
	@echo '$(MOD_FFTPACK): $$(FFTPACK_DIR)/$(SRC_FFTPACK)' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(FFTPACKFLAG) $$<' >> $(MAKENAME)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
