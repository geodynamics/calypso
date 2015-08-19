!>@file   analyzer_gen_sph_grids.f90
!!@brief  module analyzer_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_sph_grids
!!      subroutine analyze_gen_sph_grids
!!@endverbatim
!
      module analyzer_gen_sph_grids
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_work_time
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm(:)
!
      private :: comm_rlm, comm_rtm
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_sph_grids
!
      use m_spheric_parameter
      use m_read_ctl_gen_sph_shell
      use set_ctl_gen_shell_grids
      use const_sph_radial_grid
      use set_global_spherical_param
!
!
      num_elapsed = 4
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'Generation of spherical transform table'
      elapse_labels(3) = 'Generation of spherical mode and grid'
      elapse_labels(4) = 'Generation of FEM mesh data'
!
!
      call start_eleps_time(1)
      call read_control_4_gen_shell_grids
      call s_set_control_4_gen_shell_grids
!
      call set_global_sph_resolution
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter
        call output_set_radial_grid
      end if
!
      end subroutine init_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_sph_grids
!
      use m_spheric_global_ranks
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes
!
      call start_eleps_time(2)
      allocate(comm_rlm(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_grids'
      call para_gen_sph_rlm_grids(ndomain_sph, comm_rlm)
      call end_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rj_modes'
      call start_eleps_time(3)
      call para_gen_sph_rj_modes(ndomain_sph, comm_rlm)
      call dealloc_comm_stacks_sph(ndomain_sph, comm_rlm)
      deallocate(comm_rlm)
      call end_eleps_time(3)
!
      call start_eleps_time(2)
      allocate(comm_rtm(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_grids'
      call para_gen_sph_rtm_grids(ndomain_sph, comm_rtm)
      call end_eleps_time(2)
      call start_eleps_time(3)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtp_grids'
      call para_gen_sph_rtp_grids(ndomain_sph, comm_rtm)
      call dealloc_comm_stacks_sph(ndomain_sph, comm_rtm)
!
      deallocate(comm_rtm)
      call end_eleps_time(3)
!
      call start_eleps_time(4)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_fem_mesh_for_sph'
      call para_gen_fem_mesh_for_sph(ndomain_sph)
      call end_eleps_time(4)
!
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_sph_grids
