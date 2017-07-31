#
#
#

C_SRCDIR = $$(MHDDIR)/C_libraries

LIB_CALYPSO_C =       -lcalypso_c
LIB_CALYPSO_C_FILE = libcalypso_c.a

OBJS_C= $$(OBJ_BASE_C)

SUBDIRS = \
BASE

LIBDIRS = $(SUBDIRS)

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'C_SRCDIR =     $(C_SRCDIR)'      >> $(MAKENAME)
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
	@echo  'LIB_CALYPSO_C =      $(LIB_CALYPSO_C)'       >> $(MAKENAME)
	@echo  'LIB_CALYPSO_C_FILE = $(LIB_CALYPSO_C_FILE)'  >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

libtarget:
	@echo  '$$(LIB_CALYPSO_C_FILE): $(OBJS_C)' >> $(MAKENAME)

lib_archve:
	@for dir in $(LIBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		ZLIB_LIB='$(ZLIB_LIB)'       \
		PNG_LIBS='$(PNG_LIBS)'       \
		FFTW3_C='$(FFTW3_C)'   \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile commands for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module \
		) \
	done

includes:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate symbolic link commands for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		includes \
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
