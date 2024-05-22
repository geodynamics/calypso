#
#
#

C_BASE_DIR =     $$(C_SRCDIR)/BASE

CURRENTDIR = $(shell pwd)
SRC_BASE_C = $(shell ls *.c)
INC_BASE_C = $(addsuffix .h,$(basename $(SRC_BASE_C)) )
OBJ_BASE_C = $(addsuffix .o,$(basename $(SRC_BASE_C)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'C_BASE_DIR =  $(C_BASE_DIR)'      >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_BASE_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_BASE_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_BASE_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo 'Construct dependencies in C_libraries/BASE'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_BASE_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_libraries'@'$$\(C_SRCDIR\)'@g' $(CURRENTDIR)/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
