#
#
#

LIB_VIZ =       -lkemo_viz
LIB_VIZ_FILE = libkemo_viz.a

MODULE_LIST=   'MOD_VIZ'
UPPER_MODULES=

SUBDIRS= \
surfacing

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'VIZ_SRCDIR =    $$(F_SRCDIR)/VIZ_src'      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		dir_list \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_VIZ =      $(LIB_VIZ)'      >> $(MAKENAME)
	@echo  'LIB_VIZ_FILE = $(LIB_VIZ_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_VIZ_FILE): $$('$(MODULE_LIST)')' >> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done;

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)='   >> $(MAKENAME)
	@echo '$$(MOD_SURFACING)' \\ >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MODULE_LIST)' \
		mod_list \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends \
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
