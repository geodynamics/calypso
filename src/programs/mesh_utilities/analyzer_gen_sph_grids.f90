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
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_gen_sph_shell
      use t_const_spherical_grid
!
      implicit none
!
      character (len = kchara)                                          &
     &         :: control_file_name = 'control_sph_shell'
!
!
!>      Structure for file settings
      type(platform_data_control), save :: psph_gen_plt
      type(parallel_sph_shell_control), save :: psph_gen_ctl
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!>      Structure of spectr index file name and formats
      type(field_IO_params), save :: sph_file_prm_const
!>      Structure of mesh file name and formats
      type(field_IO_params), save ::  fem_mesh_file
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_G
!
      private :: control_file_name, psph_gen_ctl
      private :: sph_const, sph_file_prm_const
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_sph_grids
!
      use set_ctl_gen_shell_grids
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
      call start_elapsed_time(1)
      call read_ctl_file_gen_shell_grids                                &
     &   (control_file_name, psph_gen_plt, psph_gen_ctl)
      call set_control_4_gen_shell_grids                                &
     &   (psph_gen_plt, psph_gen_ctl%spctl, psph_gen_ctl%sdctl,         &
     &    sph_const, fem_mesh_file, sph_file_prm_const,                 &
     &    gen_sph_G, ierr_MPI)
      if(ierr_MPI .gt. 0) call calypso_mpi_abort(ierr_MPI, e_message)
!
      end subroutine init_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_sph_grids
!
      use parallel_gen_sph_grids
      use para_gen_sph_grids_modes
      use mpi_gen_sph_grids_modes
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_grids'
      call para_gen_sph_grids(sph_const, gen_sph_G)
!
      call start_elapsed_time(4)
      if(gen_sph_G%s3d_ranks%ndomain_sph .eq. nprocs) then
        call mpi_gen_fem_mesh_for_sph(gen_sph_G,                        &
     &      sph_const%sph_params, sph_const%sph_rj, sph_const%sph_rtp,  &
     &      fem_mesh_file)
      else
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_fem_mesh_for_sph'
        call para_gen_fem_mesh_for_sph                                  &
     &     (gen_sph_G%s3d_ranks%ndomain_sph, gen_sph_G,                 &
     &      sph_const%sph_params, sph_const%sph_rj, sph_const%sph_rtp,  &
     &      fem_mesh_file)
      end if
      call end_elapsed_time(4)
      call end_elapsed_time(1)
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
