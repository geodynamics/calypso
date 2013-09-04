#
#
#

SUBDIRS = \
MERGE \
INITIAL_FIELD \
SNAPSHOT_MHD


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DATA_UTILS_DIR =    $$(PROG_DIR)/data_utilities'   >> $(MAKENAME)
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
	@echo data_utils: \
	assembles \
	sph_snapshots \
	sph_initial \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		lib_name \
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
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
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
