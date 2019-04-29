!>@file   mpi_gen_sph_grids_modes.f90
!!@brief  module mpi_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine mpi_gen_sph_rlm_grids                                &
!!     &         (gen_sph, sph_params, sph_rlm, comm_rlm_mul)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(nprocs)
!!      subroutine mpi_gen_sph_rtm_grids                                &
!!     &         (gen_sph, sph_params, sph_rtm, comm_rtm_mul)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(nprocs)
!!
!!      subroutine mpi_gen_sph_rj_modes(comm_rlm_mul, sph_params,       &
!!     &          gen_sph, sph_rlm, sph_rj)
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!      subroutine mpi_gen_sph_rtp_grids(comm_rtm_mul, sph_params,      &
!!     &          gen_sph, sph_rtp, sph_rtm)
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!@endverbatim
!
      module mpi_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use m_work_time
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_data_IO
      use t_file_IO_parameter
      use t_const_spherical_grid
      use t_sph_local_parameter
      use t_sph_local_index
!
      use set_local_sphere_by_global
!
      implicit none
!
      type(sph_file_data_type), save :: sph_file_m
      type(sph_local_1d_index), save :: sph_lcx_m
      private :: sph_file_m, sph_lcx_m
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rlm_grids                                  &
     &         (gen_sph, sph_params, sph_rlm, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(nprocs)
!
      type(sph_comm_tbl) :: comm_rlm_lc
      integer(kind = kint) :: ip
!
!
      ip = my_rank + 1
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rlm table generation for', my_rank
      call const_sph_rlm_modes                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph_rlm, comm_rlm_lc)
      call copy_sph_comm_neib(comm_rlm_lc, comm_rlm_mul(ip))
!
      call output_modes_rlm_sph_trans                                   &
     &   (sph_params, sph_rlm, comm_rlm_lc, sph_file_m)
!
      call dealloc_type_sph_comm_item(comm_rlm_lc)
      call dealloc_type_sph_1d_index_rlm(sph_rlm)
      call dealloc_type_spheric_param_rlm(sph_rlm)
!
      call sel_mpi_write_modes_rlm_file(nprocs, my_rank, sph_file_m)
      call dealloc_rlm_mode_IO(sph_file_m)
!
      write(*,'(a,i6,a)') 'Spherical transform table for domain',       &
     &          my_rank, ' is done.'
!
      end subroutine mpi_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rtm_grids                                  &
     &         (gen_sph, sph_params, sph_rtm, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(nprocs)
!
      type(sph_comm_tbl) :: comm_rtm_lc
      integer(kind = kint) :: ip
!
!
!
      ip = my_rank + 1
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rtm table generation for',  my_rank
      call const_sph_rtm_grids                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph_rtm, comm_rtm_lc)
      call copy_sph_comm_neib(comm_rtm_lc, comm_rtm_mul(ip))
!
      call output_geom_rtm_sph_trans                                    &
     &   (sph_params, sph_rtm, comm_rtm_lc, sph_file_m)
!
      call dealloc_type_sph_comm_item(comm_rtm_lc)
      call dealloc_type_sph_1d_index_rtm(sph_rtm)
      call dealloc_type_spheric_param_rtm(sph_rtm)
!
      call sel_mpi_write_geom_rtm_file(nprocs, my_rank, sph_file_m)
      call dealloc_rtm_grid_IO(sph_file_m)
!
      write(*,'(a,i6,a)') 'Legendre transform table rtm',               &
     &          my_rank, ' is done.'
!
      end subroutine mpi_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rj_modes(comm_rlm_mul, sph_params,         &
     &          gen_sph, sph_rlm, sph_rj)
!
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      call alloc_rj_1d_local_idx(sph_rj, sph_lcx_m)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call const_sph_rj_modes                                           &
     &   (my_rank, nprocs, comm_rlm_mul, gen_sph%added_radial_grp,      &
     &    gen_sph%s3d_ranks, gen_sph%s3d_radius,                        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph_params, sph_rj, sph_rlm, sph_file_m, sph_lcx_m)
!
      call sel_mpi_write_spectr_rj_file(nprocs, my_rank, sph_file_m)
      call dealloc_rj_mode_IO(sph_file_m)
!
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      end subroutine mpi_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rtp_grids(comm_rtm_mul, sph_params,        &
     &          gen_sph, sph_rtp, sph_rtm)
!
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      call alloc_rtp_1d_local_idx(sph_rtp, sph_lcx_m)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical grids for domain ',  my_rank
      call const_sph_rtp_grids(my_rank, nprocs, comm_rtm_mul,           &
     &    gen_sph%added_radial_grp, gen_sph%r_layer_grp,                &
     &    gen_sph%med_layer_grp, gen_sph%s3d_ranks, gen_sph%s3d_radius, &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph_params, sph_rtp, sph_rtm, sph_file_m, sph_lcx_m)
!
      call sel_mpi_write_geom_rtp_file(nprocs, my_rank, sph_file_m)
!
      call dealloc_rtp_grid_IO(sph_file_m)
!
      write(*,'(a,i6,a)') 'Spherical grids for domain',                 &
     &          my_rank, ' is done.'
!
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
!
      end subroutine mpi_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      end module mpi_gen_sph_grids_modes
