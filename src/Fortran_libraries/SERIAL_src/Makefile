#
#
#

CORE_SRCDIR = $$(F_SRCDIR)/SERIAL_src

LIB_MESH =       -lcalypso_core
LIB_MESH_FILE = libcalypso_core.a

SUBDIRS = \
Fortran2003          \
\
BASE                 \
IO                   \
IO_ZLIB              \
Fields               \
spherical_harmonics  \
gauss_integration    \
FFT_wrapper          \
SPH_SPECTR_src


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CORE_SRCDIR = $(CORE_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list; \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_MESH =      $(LIB_MESH)'      >> $(MAKENAME)
	@echo  'LIB_MESH_FILE = $(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_MESH_FILE): $$(MOD_CORE)'>> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve; \
		) \
	done

mod_list:
	@echo  MOD_CORE= \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_F2003)'       \
	'$$(MOD_BASE)'     \
	'$$(MOD_IO)'       \
	'$$(MOD_BINARY_IO)'  \
	'$$(MOD_ZLIB_IO)'    \
	'$$(MOD_MPI_IO)'     \
	'$$(MOD_FIELDS)'     \
	'$$(MOD_GAUSS)'    \
	'$$(MOD_SPH)'      \
	'$$(MOD_FFT_WRAP)' \
	'$$(MOD_SPH_SPECTR)' \
	>> $(MAKENAME)
	@echo   >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate dependencies for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
