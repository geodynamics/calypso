#
#
#

SUBDIRS = \
SPH_MHD \
\
mesh_utilities \
data_utilities

LIBS_SPH_MHD = \
'$$(LIB_CALYPSO)'  \
'$$(LIB_FFTPACK)'


LIB_FILES_SPH_MHD = \
'$$(LIB_CALYPSO_FILE)'   \
'$$(LIB_FFTPACK_FILE)'


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PROG_DIR = $$(MHDDIR)/programs'     >> $(MAKENAME)
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
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done

target_task:
	@echo 'parallels: sph_mhd'      \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'utilities: data_utils mesh_utils' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIBS_SPH_MHD=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SPH_MHD)            >> $(MAKENAME)
	@echo  LIB_FILES_SPH_MHD=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SPH_MHD)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		make \
		MAKENAME='$(MAKENAME)' \
		lib_name \
		) \
	done

lib_tasks:


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

includes:


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
