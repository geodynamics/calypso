!analyzer_test_sph.f90
!      module analyzer_test_sph
!..................................................
!
!      modified by H. Matsui on Aug., 2007
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_test_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      integer(kind = kint), parameter :: id_check = 44
      character(len=kchara), parameter :: check_header = 'comm_errors'
      private :: check_header
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_read_ctl_gen_sph_shell
      use set_control_platform_data
      use parallel_load_data_4_sph
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call read_control_4_gen_shell_grids
      call set_control_sph_mesh
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use cmp_trans_sph_indices
      use set_parallel_file_name
      use cmp_trans_sph_tests
      use select_calypso_SR
!
      character(len=kchara) :: fname_tmp, file_name
      integer(kind = kint) :: itype
      integer(kind = kint), parameter :: NB = 8
!
!
      call allocate_idx_sph_recieve
      call allocate_real_sph_test(8)
!
      call add_int_suffix(my_rank, check_header, fname_tmp)
      call add_dat_extension(fname_tmp, file_name)
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
        call sph_indices_transfer(itype)
        call check_missing_sph_indices(id_check)
        call compare_transfer_sph_indices(id_check)
!
        call sph_transfer_test_N(NB, itype)
        call compare_transfer_sph_reals(NB, id_check)
        call sph_transfer_test_6(itype)
        call compare_transfer_sph_reals(isix, id_check)
        call sph_transfer_test_3(itype)
        call compare_transfer_sph_reals(ithree, id_check)
        call sph_transfer_test_2(itype)
        call compare_transfer_sph_reals(itwo, id_check)
        call sph_transfer_test_1(itype)
        call compare_transfer_sph_reals(ione, id_check)
      end do
!
      close(id_check)
!
      call deallocate_real_sph_test
      call deallocate_idx_sph_recieve
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_sph
