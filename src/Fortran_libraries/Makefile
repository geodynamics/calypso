#! /bin/csh
#
#

F_SRCDIR =  $$(MHDDIR)/Fortran_libraries

LIB_CALYPSO =       -lcalypso
LIB_CALYPSO_FILE = libcalypso.a

SUBDIRS =    \
\
SERIAL_src     \
SOLVER_src   \
PARALLEL_src \
MHD_src      \
UTILS_src    \
VIZ_src

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'F_SRCDIR =  $(F_SRCDIR)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_CALYPSO =      $(LIB_CALYPSO)'      >> $(MAKENAME)
	@echo  'LIB_CALYPSO_FILE = $(LIB_CALYPSO_FILE)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'create library file name list...'
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_name; \
		 ) \
	done

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_tasks \
		) \
	done

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_CALYPSO_FILE): $$(MOD_KEMO_F)'>> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done

mod_list:
	@echo  MOD_KEMO_F= \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_CORE)     $$(MOD_MHD)'        \
	'$$(MOD_PARALLEL) $$(MOD_SOLVERS)'    \
	'$$(MOD_VIZ)      $$(MOD_KEMO_UTILS)' >> $(MAKENAME)
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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

includes:


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
