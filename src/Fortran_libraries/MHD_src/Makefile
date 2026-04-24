#
#
#

MHD_SRCDIR = $$(F_SRCDIR)/MHD_src

LIB_MHD=       -lcalypso_MHD
LIB_MHD_FILE= libcalypso_MHD.a

UPPER_MODULES= MOD_MHD

SUBDIRS = \
IO \
initial_data \
radial_FDM \
sph_MHD

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SRCDIR = $(MHD_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo LIB_MHD =      $(LIB_MHD)  >> $(MAKENAME)
	@echo LIB_MHD_FILE = $(LIB_MHD_FILE)  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB)' '$$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo '$$(LIB_MHD_FILE): $$(MOD_MHD)' >> $(MAKENAME)

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
	@echo '$(UPPER_MODULES)= '  >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(UPPER_MODULES)' \
		mod_list; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate compile list for ' $$dir ; \
		cd $${dir};  \
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
