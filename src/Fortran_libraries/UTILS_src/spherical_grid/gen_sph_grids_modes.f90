!>@file   gen_sph_grids_modes.f90
!!@brief  module gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Serial version)
!!
!!@verbatim
!!      subroutine gen_sph_transfer_grids
!!      subroutine gen_sph_modes_grids
!!      subroutine gen_fem_mesh_for_sph
!!
!!      subroutine const_transform_grids_modes(ip_rank)
!!      subroutine const_sph_modes_grids(ip_rank)
!!      subroutine const_fem_mesh_for_sph(ip_rank)
!!@endverbatim
!
      module gen_sph_grids_modes
!
      use m_precision
!
      use m_machine_parameter
      use set_local_sphere_by_global
!
      implicit none
!
!>      Integer flag to excluding FEM mesh
      integer(kind = kint) :: iflag_excluding_FEM_mesh = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gen_sph_transfer_grids
!
      use m_parallel_sph_grids
      use load_data_for_sph_IO
!
      integer(kind = kint) :: ip_rank
!
!
      call alloc_parallel_sph_grids(ndomain_sph)
!
      do ip_rank = 0, ndomain_sph-1
        call const_transform_grids_modes(ip_rank)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans(ip_rank)
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans(ip_rank)
!
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &          ip_rank, ' is done.'
      end do
      write(*,*)
!
      end subroutine gen_sph_transfer_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_sph_modes_grids
!
      use m_spheric_parameter
      use m_parallel_sph_grids
      use set_local_index_table_sph
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx
      call allocate_rj_1d_local_idx
!
      do ip_rank = 0, ndomain_sph-1
        call const_sph_modes_grids(ip_rank)
      end do
!
      call deallocate_rtp_1d_local_idx
      call deallocate_rj_1d_local_idx
!
      end subroutine gen_sph_modes_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_fem_mesh_for_sph
!
      use m_gauss_points
      use m_group_data_sph_specr
      use m_sph_mesh_1d_connect
      use set_local_index_table_sph
      use set_sph_groups
!
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_excluding_FEM_mesh .gt. 0) return
!
      n_point = nidx_global_rtp(2)
      call allocate_gauss_points
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call set_rj_radial_grp
!
      do ip_rank = 0, ndomain_sph-1
        call const_fem_mesh_for_sph(ip_rank)
      end do
!
      call deallocate_rj_r_grp_item
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_transform_grids_modes(ip_rank)
!
      use m_spheric_parameter
      use m_parallel_sph_grids
      use const_comm_table_sph
      use copy_sph_1d_global_index
      use set_local_sphere_param
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint) :: ip
!
!
      ip = ip_rank + 1
!
      call copy_gl_2_local_rtm_param(ip_rank)
      call copy_gl_2_local_rlm_param(ip_rank)
!
      call allocate_spheric_param_rtm
      call allocate_spheric_param_rlm
      call allocate_sph_1d_index_rtm
      call allocate_sph_1d_index_rlm
!
      call copy_sph_1d_gl_idx_rtm
      call copy_sph_1d_gl_idx_rlm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_global_sph_4_rtm', ip_rank
      call set_global_sph_4_rtm
      call set_global_sph_4_rlm
!
      if(iflag_debug .gt. 0) then
        call check_spheric_param_rtm(ip_rank)
        call check_spheric_param_rlm(ip_rank)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rlm', ip_rank
      call const_comm_table_4_rlm(ip_rank, nnod_rlm)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rtm', ip_rank
      call const_comm_table_4_rtm(ip_rank, nnod_rtm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'copy_sph_rlm_grid_to_mem', ip_rank
      call copy_sph_rlm_grid_to_mem(ip)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'copy_sph_rtm_grid_to_mem', ip_rank
      call copy_sph_rtm_grid_to_mem(ip)
!
      end subroutine const_transform_grids_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_modes_grids(ip_rank)
!
      use m_spheric_parameter
      use m_parallel_sph_grids
      use const_comm_table_sph
      use load_data_for_sph_IO
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_index_table_sph
!
      integer(kind = kint), intent(in) :: ip_rank
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'copy_gl_2_local_rtp_param', ip_rank
      call copy_gl_2_local_rtp_param(ip_rank)
      call copy_gl_2_local_rj_param(ip_rank)
!
      call add_center_mode_rj
!
      call allocate_spheric_param_rtp
      call allocate_spheric_param_rj
      call allocate_sph_1d_index_rtp
      call allocate_sph_1d_index_rj
!
      call copy_sph_1d_gl_idx_rtp
      call copy_sph_1d_gl_idx_rj
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'set_global_sph_rtp_id', ip_rank
      call set_global_sph_rtp_id
      call set_global_sph_rj_id
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'check_spheric_param_rtp', ip_rank
        call check_spheric_param_rtp(ip_rank)
        call check_spheric_param_rj(ip_rank)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rtp', ip_rank
      call const_comm_table_4_rtp(ip_rank, nnod_rtp)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rj', ip_rank
      call const_comm_table_4_rj(ip_rank, nnod_rj)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                  's_set_sph_groups', ip_rank
      call s_set_sph_groups
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'output_geom_rtp_sph_trans', ip_rank
      call output_geom_rtp_sph_trans(ip_rank)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'output_modes_rj_sph_trans', ip_rank
      call output_modes_rj_sph_trans(ip_rank)
!
      write(*,'(a,i6,a)') 'Spherical modes and grids for domain',       &
     &          ip_rank, ' is done.'
!
      end subroutine const_sph_modes_grids
!
! ----------------------------------------------------------------------
!
      subroutine const_fem_mesh_for_sph(ip_rank)
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use m_gauss_points
      use m_group_data_sph_specr
      use set_local_index_table_sph
      use set_FEM_mesh_4_sph
      use m_sph_mesh_1d_connect
!
      use m_node_id_spherical_IO
      use m_read_mesh_data
      use set_comm_tbl_type_4_IO
      use set_node_types_4_IO
      use set_element_types_4_IO
      use set_group_types_4_IO
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: ip_rank
!
      type(mesh_geometry) :: mesh
      type(mesh_groups) ::  group
!
!
      call copy_gl_2_local_rtp_param(ip_rank)
!
      call s_const_FEM_mesh_for_sph(ip_rank, mesh, group)
!
      call copy_comm_tbl_type_to_IO(ip_rank, mesh%nod_comm)
      call copy_node_type_to_IO(mesh%node)
      call copy_ele_connect_type_to_IO(mesh%ele)
      call set_grp_data_type_to_IO(group)
!
      call dealloc_groups_data(group)
      call deallocate_node_geometry_type(mesh%node)
      call deallocate_type_comm_tbl(mesh%nod_comm)
!
      mesh_file_head = sph_head
      call sel_write_mesh_file(ip_rank)
!
      write(*,'(a,i6,a)')                                               &
     &          'FEM mesh for domain', ip_rank, ' is done.'
!
      end subroutine const_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module gen_sph_grids_modes
