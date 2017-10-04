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
!!      subroutine mpi_gen_sph_rj_modes(comm_rlm_mul,                   &
!!     &          gen_sph, sph_params, sph_rlm, sph_rj)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine mpi_gen_sph_rtp_grids(comm_rtm_mul,                  &
!!     &          gen_sph, sph_params, sph_rtp, sph_rtm)
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine mpi_gen_fem_mesh_for_sph                             &
!!     &         (iflag_output_mesh, iflag_output_SURF,                 &
!!     &          gen_sph, sph_params, sph_rj, sph_rtp, mesh_file)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(field_IO_params), intent(inout) ::  mesh_file
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
      call sel_mpi_write_modes_rlm_file(nprocs, my_rank, sph_file_m)
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
      call sel_mpi_write_geom_rtm_file(nprocs, my_rank, sph_file_m)
      write(*,'(a,i6,a)') 'Legendre transform table rtm',               &
     &          my_rank, ' is done.'
!
      end subroutine mpi_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rj_modes(comm_rlm_mul,                     &
     &          gen_sph, sph_params, sph_rlm, sph_rj)
!
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
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
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      end subroutine mpi_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rtp_grids(comm_rtm_mul,                    &
     &          gen_sph, sph_params, sph_rtp, sph_rtm)
!
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
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
      write(*,'(a,i6,a)') 'Spherical grids for domain',                 &
     &          my_rank, ' is done.'
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
!
      end subroutine mpi_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_fem_mesh_for_sph                               &
     &         (iflag_output_mesh, iflag_output_SURF,                   &
     &          gen_sph, sph_params, sph_rj, sph_rtp, mesh_file)
!
      use t_mesh_data
      use t_gauss_points
      use t_sph_mesh_1d_connect
!
      use const_1d_ele_connect_4_sph
      use set_local_sphere_by_global
      use set_sph_groups
      use gen_sph_grids_modes
      use set_FEM_mesh_4_sph
      use mpi_load_mesh_data
      use sph_file_IO_select
      use parallel_FEM_mesh_init
      use set_nnod_4_ele_by_type
!
      integer(kind = kint), intent(in) :: iflag_output_mesh
      integer(kind = kint), intent(in) :: iflag_output_SURF
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(mesh_data) :: femmesh
      type(group_data) :: radial_rj_grp_lc
      type(gauss_points) :: gauss_s
      type(comm_table_make_sph) :: stbl_s
      type(element_geometry) :: ele_mesh
!
!
      if(iflag_output_mesh .eq. 0) return
!
      call const_gauss_colatitude(sph_rtp%nidx_global_rtp(2), gauss_s)
!
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp,   &
     &    gen_sph%s3d_ranks, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &    stbl_s)
      call set_rj_radial_grp(sph_params, sph_rj,                        &
     &    gen_sph%added_radial_grp, radial_rj_grp_lc)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct FEM mesh for domain ', my_rank
!
      call copy_gl_2_local_rtp_param (my_rank, gen_sph%s3d_ranks,       &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, sph_rtp)
      call s_const_FEM_mesh_for_sph                                     &
     &   (my_rank, sph_rtp%nidx_rtp, gen_sph%s3d_radius%radius_1d_gl,   &
     &    gauss_s, gen_sph%s3d_ranks, gen_sph%stk_lc1d,                 &
     &    gen_sph%sph_gl1d, sph_params, sph_rtp, radial_rj_grp_lc,      &
     &    femmesh%mesh, femmesh%group, stbl_s)
!
! Output mesh data
      if(iflag_output_mesh .gt. 0) then
        mesh_file%file_prefix = sph_file_head
        call mpi_output_mesh(mesh_file, femmesh%mesh, femmesh%group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
      end if
!
      if(iflag_output_SURF .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_init_with_IO'
        call set_3d_nnod_4_sfed_by_ele(femmesh%mesh%ele%nnod_4_ele,     &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
        call FEM_mesh_init_with_IO(iflag_output_SURF, mesh_file,        &
     &      femmesh%mesh, femmesh%group, ele_mesh)
      end if
!
      call dealloc_groups_data(femmesh%group)
      call dealloc_mesh_type(femmesh%mesh)
      call deallocate_grp_type(radial_rj_grp_lc)
!
      call dealloc_nnod_nele_sph_mesh(stbl_s)
      call dealloc_gauss_colatitude(gauss_s)
!
      end subroutine mpi_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module mpi_gen_sph_grids_modes
