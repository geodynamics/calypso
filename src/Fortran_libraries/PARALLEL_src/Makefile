#
#
#

PARA_SRCDIR =   $$(F_SRCDIR)/PARALLEL_src

LIB_PARA =       -lcalypso_parallel
LIB_PARA_FILE = libcalypso_parallel.a

SUBDIRS = \
COMM_src        \
MPI_IO          \
MPI_ZLIB_IO     \
SPH_SHELL_src   \
CONST_SPH_GRID

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PARA_SRCDIR = $(PARA_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list; \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_PARA =      $(LIB_PARA)' >> $(MAKENAME)
	@echo  'LIB_PARA_FILE = $(LIB_PARA_FILE)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_PARA_FILE):  $$(MOD_PARALLEL) ' >> $(MAKENAME)

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
	@echo  MOD_PARALLEL= \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_COMM)'                          \
	'$$(MOD_MPI_IO)    $$(MOD_MPI_ZLIB_IO)' \
	'$$(MOD_COMM_SPH)  $$(MOD_SPH_GRID)'    >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done


module:
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
