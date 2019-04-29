!analyzer_test_sph.f90
!      module analyzer_test_sph
!..................................................
!
!      modified by H. Matsui on Aug., 2007
!
!      subroutine init_test_sph
!      subroutine analyze_test_sph
!
      module analyzer_test_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_SEND_RECV
!
      use t_SPH_mesh_field_data
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
!
      use t_ctl_data_const_sph_mesh
      use t_ctl_params_gen_sph_shell
!
      use calypso_mpi
!
      implicit none
!
      integer(kind = kint), parameter :: id_check = 44
      character(len=kchara), parameter :: check_header = 'comm_errors'
!
      character (len = kchara)                                          &
     &         :: control_file_name = 'control_sph_shell'
!
!>      Structure for file settings
      type(sph_mesh_generation_ctl), save :: SPH_TEST_ctl
!
      type(SPH_mesh_field_data), save :: SPH_TEST
!
      type(gen_sph_file_IO_params), save ::  test_sph_files
!
      private :: control_file_name, SPH_TEST_ctl
      private :: check_header, SPH_TEST
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_test_sph
!
      use set_control_platform_data
      use parallel_load_data_4_sph
      use cmp_trans_sph_tests
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_calypso_send_recv
      call elpsed_label_field_send_recv
!
!
!     --------------------- 
!
      call turn_off_debug_flag_by_ctl(my_rank, SPH_TEST_ctl%plt)
      call read_control_4_const_shell(control_file_name, SPH_TEST_ctl)
      call set_control_sph_mesh                                         &
     &   (SPH_TEST_ctl%plt, SPH_TEST_ctl%psph_ctl%Fmesh_ctl,            &
     &    test_sph_files%mesh_file_IO, test_sph_files%sph_file_IO,      &
     &    test_sph_files%FEM_mesh_flags)
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (SPH_TEST%sph, SPH_TEST%comms, SPH_TEST%groups)
!
       end subroutine init_test_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_test_sph
!
      use m_solver_SR
      use cmp_trans_sph_indices
      use cmp_trans_sph_tests
      use set_parallel_file_name
      use select_calypso_SR
      use select_copy_from_recv
!
      character(len=kchara) :: fname_tmp, file_name
      integer(kind = kint) :: itype
      integer(kind = kint), parameter :: NB = 8
!
!
      call allocate_idx_sph_recieve                                     &
     &   (SPH_TEST%sph%sph_rtp%nnod_rtp, SPH_TEST%sph%sph_rtm%nnod_rtm, &
     &    SPH_TEST%sph%sph_rlm%nnod_rlm, SPH_TEST%sph%sph_rj%nnod_rj)
      call allocate_real_sph_test(NB,                                   &
     &    SPH_TEST%sph%sph_rtp%nnod_rtp, SPH_TEST%sph%sph_rtm%nnod_rtm, &
     &    SPH_TEST%sph%sph_rlm%nnod_rlm, SPH_TEST%sph%sph_rj%nnod_rj)
!
      fname_tmp = add_process_id(my_rank, check_header)
      file_name = add_dat_extension(fname_tmp)
      write(*,*) 'error check result: ', trim(file_name)
      open(id_check, file=file_name)
!
      do itype = iflag_import_item, iflag_import_rev
!
       if(itype .eq. iflag_import_item) then
         write(id_check,*)  'USING IMPORT_ITEM'
       else if(itype .eq. iflag_import_rev) then
         write(id_check,*)  'USING IMPORT_REVERSE'
       end if
!
        call sph_type_indices_transfer                                  &
     &     (itype, SPH_TEST%sph, SPH_TEST%comms)
        call check_missing_sph_indices(id_check, SPH_TEST%sph)
        call compare_transfer_sph_indices(id_check, SPH_TEST%sph)
!
        call sph_transfer_test_N                                        &
     &     (NB, itype, SPH_TEST%sph, SPH_TEST%comms)
        call compare_transfer_sph_reals(NB, id_check, SPH_TEST%sph)
!
        call sph_transfer_test_6                                        &
     &     (itype, SPH_TEST%sph, SPH_TEST%comms)
        call compare_transfer_sph_reals(isix, id_check, SPH_TEST%sph)
!
        call sph_transfer_test_3                                        &
     &     (itype, SPH_TEST%sph, SPH_TEST%comms)
        call compare_transfer_sph_reals                                 &
     &     (ithree, id_check, SPH_TEST%sph)
!
        call sph_transfer_test_2                                        &
     &     (itype, SPH_TEST%sph, SPH_TEST%comms)
        call compare_transfer_sph_reals(itwo, id_check, SPH_TEST%sph)
!
        call sph_transfer_test_1                                        &
     &     (itype, SPH_TEST%sph, SPH_TEST%comms)
        call compare_transfer_sph_reals(ione, id_check, SPH_TEST%sph)
      end do
!
      close(id_check)
!
      call deallocate_real_sph_test
      call deallocate_idx_sph_recieve
!
      call output_elapsed_times
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze_test_sph'
!
      end subroutine analyze_test_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_sph
