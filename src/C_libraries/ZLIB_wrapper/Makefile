#
#
#

ZLIB_WRAP_DIR = $$(C_SRCDIR)/ZLIB_wrapper

CURRENTDIR = $(shell pwd)
SRC_ZLIB_WRAPPER = $(shell ls *.c)
INC_ZLIB_WRAPPER = $(addsuffix .h,$(basename $(SRC_ZLIB_WRAPPER)) )
OBJ_ZLIB_WRAPPER = $(addsuffix .o,$(basename $(SRC_ZLIB_WRAPPER)) )

OBJS_LIST= OBJS_C

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ZLIB_WRAP_DIR =  $(ZLIB_WRAP_DIR)' >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_ZLIB_WRAPPER)' >> $(MAKENAME)


mod_list:
	@echo  '$(OBJS_LIST)+= $$(OBJ_ZLIB_WRAPPER)' >> $(MAKENAME)
	@echo  OBJ_ZLIB_WRAPPER=  \\ >> $(MAKENAME)
	@echo  $(OBJ_ZLIB_WRAPPER)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


depends:
	@echo 'Construct compile list in C_src/ZLIB_wrapper'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_ZLIB_WRAPPER) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp >> $(MAKENAME).depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
