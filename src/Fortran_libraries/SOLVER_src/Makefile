#
#
#

SOLVER_SRCDIR = $$(F_SRCDIR)/SOLVER_src

LIB_SOLVER =       -lcalypso_solver
LIB_SOLVER_FILE = libcalypso_solver.a

MODULE_LIST=   'MOD_SOLVERS'
UPPER_MODULES=

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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
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
	@echo  '$$(LIB_SOLVER_FILE): $$('$(MODULE_LIST)')' >> $(MAKENAME)

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
	@echo '$(MODULE_LIST)= ' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MODULE_LIST)' \
		mod_list; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile list for ' $$dir ; \
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
