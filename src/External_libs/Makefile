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
		make dir_list \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		make lib_name; \
		 ) \
	done

lib_tasks:
	@for dir in $(SUBDIRS) ; do \
		( cd $${dir};  \
		echo 'generate library commands for ' $$dir ; \
		make lib_tasks  \
		OPENGL_LIBS='$(OPENGL_LIBS)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)'; \
		 ) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate compile commands for ' $$dir ; \
		cd $${dir};  \
		make module \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' ;\
		) \
	done

includes:
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'copy C include files for ' $$dir ; \
		cd $${dir};  \
		make includes \
		MAKENAME='$(MAKENAME)' \
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
