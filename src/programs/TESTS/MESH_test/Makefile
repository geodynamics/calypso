#
#
#

MESH_TEST_DIR = $$(TESTS_DIR)/MESH_test

TARGET_COMM_TEST =   comm_test

SOURCES = $(shell ls *.f90)

MOD_COMM_TEST = \
t_ctl_data_comm_test.o \
send_recv_loop_tests.o \
mesh_send_recv_test.o \
\
t_control_param_comm_test.o \
analyzer_comm_test.o \
main_comm_test.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_TEST_DIR = $(MESH_TEST_DIR)'         >> $(MAKENAME)

target_list:
	@echo 'TARGET_COMM_TEST = $$(BUILDDIR)/$(TARGET_COMM_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo comm_tests: \
	'$$(TARGET_COMM_TEST)'     \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_COMM_TEST)': \
	'$$(LIB_CALYPSO_FILE)   $$(MOD_COMM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_COMM_TEST)' \
	'$$(MOD_COMM_TEST)' \
	'-L. $$(LIB_CALYPSO)  $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_COMM_TEST = $(MOD_COMM_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MESH_TEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
