#
#
#

C_BASE_DIR =     $$(C_SRCDIR)/BASE

OBJ_BASE_C = \
 calypso_param_c.o \
 numbers_to_bin_c.o \
 calypso_zlib_io_c.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'C_BASE_DIR =  $(C_BASE_DIR)'      >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_BASE_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_BASE_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_BASE_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	calypso_param_c.o: \
	'$$(C_BASE_DIR)'/calypso_param_c.c \
	calypso_param_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	numbers_to_bin_c.o: \
	'$$(C_BASE_DIR)'/numbers_to_bin_c.c \
	numbers_to_bin_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(C_BASE_DIR)'/numbers_to_bin_c.c \
	>> $(MAKENAME)
	
	@echo \
	calypso_zlib_io_c.o: \
	'$$(C_BASE_DIR)'/calypso_zlib_io_c.c \
	calypso_zlib_io_c.h \
	calypso_param_c.o \
	numbers_to_bin_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(C_BASE_DIR)'/calypso_zlib_io_c.c \
	>> $(MAKENAME)


includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(C_BASE_DIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(C_BASE_DIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o $(TARGET) *.mod *~
