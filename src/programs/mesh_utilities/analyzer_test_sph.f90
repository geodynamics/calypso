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
      use m_machine_parameter
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
      use m_parallel_var_dof
      use m_read_ctl_gen_sph_shell
      use set_control_platform_data
      use load_data_for_sph_IO
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
      if (iflag_debug.gt.0) write(*,*) 'input_sph_trans_grids'
      call input_sph_trans_grids(my_rank)
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_parallel_var_dof
      use cmp_trans_sph_indices
      use set_parallel_file_name
!
      character(len=kchara) :: fname_tmp, file_name
!
!
      call allocate_idx_sph_recieve
!
      call sph_indices_transfer
!
      call add_int_suffix(my_rank, check_header, fname_tmp)
      call add_dat_extension(fname_tmp, file_name)
      write(*,*) 'error check result: ', trim(file_name)
      open(id_check, file=file_name)
!
      call check_missing_sph_indices(id_check)
      call compare_transfer_sph_indices(id_check)
      close(id_check)
!
      call deallocate_idx_sph_recieve
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_sph
