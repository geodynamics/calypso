!>@file   analyzer_check_sph_grids.f90
!!@brief  module analyzer_check_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_check_sph_grids
!!      subroutine analyze_check_sph_grids
!!@endverbatim
!
      module analyzer_check_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
!
      use t_mesh_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_const_sph_mesh
      use t_ctl_params_gen_sph_shell
!
      use para_const_kemoview_mesh
!
      implicit none
!
      character (len = kchara)                                          &
     &         :: control_file_name = 'control_sph_shell'
!
!
!>      Structure for file settings
      type(sph_mesh_generation_ctl), save :: SPH_MAKE_ctl
!
!>      Structure of spherical transform mesh information
      type(SPH_mesh_field_data), save :: SPH_GEN
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files1
!
!>      Structure to check and construct spherical shell mesh
      type(sph_grid_maker_in_sim), save :: sph_maker_G
!
      private :: control_file_name
      private :: SPH_GEN, SPH_MAKE_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_check_sph_grids
!
      use m_error_IDs
      use input_control_const_shell
!
      integer(kind = kint) :: ierr = 0
!
! 
      call init_elapse_time_by_TOTAL
      call elpsed_label_gen_sph_grid
!
      call start_elapsed_time(ied_total_elapsed)
      call s_input_control_const_shell(control_file_name, SPH_MAKE_ctl, &
     &                                 sph_files1, sph_maker_G)
      call end_elapsed_time(ied_total_elapsed)
!
      if(sph_maker_G%gen_sph%s3d_ranks%ndomain_sph .ne. nprocs) then
        if(my_rank .eq. 0) write(*,*) 'The number of MPI processes ',   &
     &      'must be equal to the number of subdomains.', char(10),     &
     &      'Current subdomains: ',                                     &
     &      sph_maker_G%gen_sph%s3d_ranks%ndomain_sph
        write(e_message,'(a)') 'Parallellization error'
        call calypso_mpi_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine init_check_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine analyze_check_sph_grids
!
      use calypso_mpi_int
      use m_error_IDs
      use mpi_gen_sph_grids_modes
      use compare_sph_with_IO
      use parallel_load_data_4_sph
!
      integer(kind = kint) :: iflag, iflag_gl
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'mpi_gen_sph_grids'
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call mpi_gen_sph_grids                                            &
     &   (sph_maker_G, SPH_GEN%sph, SPH_GEN%comms, SPH_GEN%groups)
      iflag = s_compare_sph_with_IO(sph_files1%sph_file_param,          &
     &    SPH_GEN%sph, SPH_GEN%comms, SPH_GEN%groups)
      call calypso_mpi_allreduce_one_int(iflag, iflag_gl, MPI_MAX)
!
!      write(*,*) 'indexing', my_rank, iflag_gl
      call calypso_mpi_barrier
      if(iflag .gt. 0) call calypso_mpi_abort(ierr_P_MPI, e_message)
      if(iflag_gl .eq. 0 .and. my_rank .eq. 0) then
        write(*,*) 'indexing is correct'
      end if
!
      call dealloc_sph_modes(SPH_GEN%sph, SPH_GEN%comms,                &
     &                       SPH_GEN%groups)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_check_sph_grids
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_sph_grids
