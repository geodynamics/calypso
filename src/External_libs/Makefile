#
#

SUBDIRS =  \
FFTPACK5.1D

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'EXT_DIR =  $$(MHDDIR)/External_libs'      >> $(MAKENAME)
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

lib_tasks:
	@for dir in $(SUBDIRS) ; do \
		( cd $${dir};  \
		echo 'generate library commands for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		OPENGL_LIBS='$(OPENGL_LIBS)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_tasks; \
		 ) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate compile commands for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		module; \
		) \
	done

includes:
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'copy C include files for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		includes; \
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
