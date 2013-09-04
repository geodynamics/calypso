#
#
#

UTILS_SRCDIR =  $$(F_SRCDIR)/UTILS_src

LIB_UTILS =       -lcalypso_utils
LIB_UTILS_FILE = libcalypso_utils.a

SUBDIRS = \
spherical_grid \
MESH           \
MERGE


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'UTILS_SRCDIR =  $(UTILS_SRCDIR)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done

lib_name:
	@echo  'LIB_UTILS =       $(LIB_UTILS)'      >> $(MAKENAME)
	@echo  'LIB_UTILS_FILE =  $(LIB_UTILS_FILE)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in  $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		make lib_name \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)'; \
		 ) \
	done

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_UTILS_FILE): $$(MOD_KEMO_UTILS)'>> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		lib_archve; \
		) \
	done

mod_list:
	@echo '' >> $(MAKENAME)
	@echo MOD_KEMO_UTILS=  \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_MESH)'         \
	'$$(MOD_MERGE)'        \
	'$$(MOD_PART_COMMON)'  \
	'$$(MOD_SPH_GRID)'     >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
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
