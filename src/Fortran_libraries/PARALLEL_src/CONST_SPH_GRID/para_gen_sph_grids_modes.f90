!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_grids(ndomain_sph,                  &
!!     &          gen_sph, sph_params, sph_rlm, comm_rlm_mul)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
!!      subroutine para_gen_sph_rtm_grids(ndomain_sph,                  &
!!     &          gen_sph, sph_params, sph_rtm, comm_rtm_mul)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
!!
!!      subroutine para_gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,     &
!!     &          gen_sph, sph_params, sph_rlm, sph_rj)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine para_gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,    &
!!     &          gen_sph, sph_params, sph_rtp, sph_rtm)
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine para_gen_fem_mesh_for_sph                            &
!!     &         (iflag_output_mesh, ndomain_sph,                       &
!!     &          gen_sph, sph_params, sph_rj, sph_rtp, mesh_file)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!@endverbatim
!
      module para_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use m_work_time
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
      use t_spheric_mesh
      use t_spheric_data_IO
      use t_file_IO_parameter
      use t_const_spherical_grid
      use t_sph_1d_global_index
      use t_sph_local_index
!
      use set_local_sphere_by_global
!
      implicit none
!
      type(sph_file_data_type), save :: sph_file_p
      type(sph_local_1d_index), save :: sph_lcx_p
      private :: sph_file_p, sph_lcx_p
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_grids(ndomain_sph,                    &
     &          gen_sph, sph_params, sph_rlm, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
!
      type(sph_comm_tbl) :: comm_rlm_lc
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rlm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rlm_modes                                        &
     &    (ip_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,              &
     &     gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,         &
     &     sph_rlm, comm_rlm_lc)
        if(iflag_debug .gt. 0) write(*,*) 'copy_sph_comm_neib'
        call copy_sph_comm_neib(comm_rlm_lc, comm_rlm_mul(ip_rank+1))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans                                 &
     &     (sph_params, sph_rlm, comm_rlm_lc, sph_file_p)
!
        call sel_write_modes_rlm_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Spherical transform table for domain',  &
     &          ip_rank, ' is done on process ', my_rank
      end do
!
      end subroutine para_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_grids(ndomain_sph,                    &
     &          gen_sph, sph_params, sph_rtm, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
!
      type(sph_comm_tbl) :: comm_rtm_lc
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rtm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rtm_grids                                        &
     &    (ip_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,              &
     &     gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,         &
     &     sph_rtm, comm_rtm_lc)
        call copy_sph_comm_neib(comm_rtm_lc, comm_rtm_mul(ip_rank+1))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans                                  &
     &     (sph_params, sph_rtm, comm_rtm_lc, sph_file_p)
!
        call sel_write_geom_rtm_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Legendre transform table rtm',          &
     &          ip_rank, ' is done on process ', my_rank
      end do
!
      end subroutine para_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,       &
     &          gen_sph, sph_params, sph_rlm, sph_rj)
!
      use set_comm_table_rtp_rj
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: ip_rank, ip
!
!
      call alloc_rj_1d_local_idx(sph_rj, sph_lcx_p)
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical modes for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rj_modes(ip_rank,                                &
     &      ndomain_sph, comm_rlm_mul, gen_sph%added_radial_grp,        &
     &      gen_sph%s3d_ranks, gen_sph%s3d_radius,                      &
     &      gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &      sph_params, sph_rj, sph_rlm, sph_file_p, sph_lcx_p)
!
        call sel_write_spectr_modes_rj_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Spherical modes for domain',            &
     &          ip_rank, ' is done on process ', my_rank
      end do
      call dealloc_rj_1d_local_idx(sph_lcx_p)
!
      end subroutine para_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,      &
     &          gen_sph, sph_params, sph_rtp, sph_rtm)
!
      use set_comm_table_rtp_rj
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      integer(kind = kint) :: ip_rank, ip
!
!
      call alloc_rtp_1d_local_idx(sph_rtp, sph_lcx_p)
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm_mul,    &
     &    gen_sph%added_radial_grp, gen_sph%r_layer_grp,                &
     &    gen_sph%med_layer_grp, gen_sph%s3d_ranks, gen_sph%s3d_radius, &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph_params, sph_rtp, sph_rtm, sph_file_p, sph_lcx_p)
!
        call sel_write_geom_rtp_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Spherical grids for domain',            &
     &          ip_rank, ' is done on process ', my_rank
      end do
      call dealloc_rtp_1d_local_idx(sph_lcx_p)
!
      end subroutine para_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_fem_mesh_for_sph                              &
     &         (iflag_output_mesh, ndomain_sph,                         &
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
      use load_mesh_data
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: iflag_output_mesh
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(field_IO_params), intent(inout) ::  mesh_file
!
      integer(kind = kint) :: ip_rank, ip
      type(mesh_data) :: femmesh
      type(group_data) :: radial_rj_grp_lc
      type(gauss_points) :: gauss_s
      type(comm_table_make_sph) :: stbl_s
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
     &   gen_sph%added_radial_grp, radial_rj_grp_lc)
!
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct FEM mesh for domain ', ip_rank,           &
     &             ' on ', my_rank
!
        call copy_gl_2_local_rtp_param                                  &
     &     (ip_rank, gen_sph%s3d_ranks, gen_sph%sph_lcp,                &
     &      gen_sph%stk_lc1d, sph_rtp)
!
!
        call s_const_FEM_mesh_for_sph(ip_rank,                          &
     &      sph_rtp%nidx_rtp, gen_sph%s3d_radius%radius_1d_gl, gauss_s, &
     &      gen_sph%s3d_ranks, gen_sph%stk_lc1d,                        &
     &      gen_sph%sph_gl1d, sph_params, sph_rtp,                      &
     &      radial_rj_grp_lc, femmesh%mesh, femmesh%group, stbl_s)
!
! Output mesh data
        if(iflag_output_mesh .gt. 0) then
          mesh_file%file_prefix = sph_file_head
          call output_mesh(mesh_file, ip_rank,                          &
     &                     femmesh%mesh, femmesh%group)
          write(*,'(a,i6,a,i6)')                                        &
     &         'Domain', ip_rank, ' is done on process ', my_rank
        end if
      end do
!
      call dealloc_groups_data(femmesh%group)
      call dealloc_mesh_type(femmesh%mesh)
      call deallocate_grp_type(radial_rj_grp_lc)
!
      call dealloc_nnod_nele_sph_mesh(stbl_s)
      call dealloc_gauss_colatitude(gauss_s)
!
      end subroutine para_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
