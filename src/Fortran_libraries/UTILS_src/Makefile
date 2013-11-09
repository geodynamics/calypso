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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_name; \
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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done


module:
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
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
