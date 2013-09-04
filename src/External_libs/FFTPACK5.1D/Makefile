#
#
#

FFTPACK_DIR = $$(EXT_DIR)/FFTPACK5.1D

LIB_FFTPACK =       -lfftpack.5d
LIB_FFTPACK_FILE = libfftpack.5d.a

FFTPACKFLAG = $(subst '-warn all','',$(F90FLAGS))

MOD_FFTPACK = \
fftpack5.1d.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo FFTPACKFLAG =  '$$(subst ''-warn all'','' '',$$(F90FLAGS))' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'FFTPACK_DIR =  $(FFTPACK_DIR)'   >> $(MAKENAME)

lib_name:
	@echo  'LIB_FFTPACK =      $(LIB_FFTPACK)'      >> $(MAKENAME)
	@echo  'LIB_FFTPACK_FILE = $(LIB_FFTPACK_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_FFTPACK_FILE): $$(MOD_FFTPACK)'>> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FFTPACK)' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFTPACK=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFTPACK) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@echo 'fftpack5.1d.o: $$(FFTPACK_DIR)/fftpack5.1d.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(FFTPACKFLAG) $$<' >> $(MAKENAME)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
