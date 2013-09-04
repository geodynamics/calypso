#
#
#

SOLVER_SRCDIR = $$(F_SRCDIR)/SOLVER_src

LIB_SOLVER =       -lcalypso_solver
LIB_SOLVER_FILE = libcalypso_solver.a

SUBDIRS = \
direct_solver

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_SRCDIR =  $(SOLVER_SRCDIR)'       >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		dir_list; \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_SOLVER =      $(LIB_SOLVER)'      >> $(MAKENAME)
	@echo  'LIB_SOLVER_FILE = $(LIB_SOLVER_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_SOLVER_FILE): $$(MOD_SOLVERS)' >> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		lib_archve; \
		) \
	done


mod_list:
	@echo  MOD_SOLVERS=  \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_SR)         $$(MOD_DJDS_SOLVER)'  \
	'$$(MOD_CRS_SOLVER) $$(MOD_CRS_SOLVER_S) ' \
	'$$(MOD_SOLVER_LU)' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done

module:
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
