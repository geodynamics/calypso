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
!!      subroutine const_sph_rlm_modes(ip_rank)
!!      subroutine const_sph_rtm_grids(ip_rank)
!!
!!      subroutine const_fem_mesh_for_sph(ip_rank)
!!@endverbatim
!
      module gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>      Integer flag to excluding FEM mesh
      integer(kind = kint) :: iflag_excluding_FEM_mesh = 0
!
      private :: const_comm_table_4_rlm, const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rlm_modes(ip_rank)
!
      use m_spheric_parameter
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer(kind = kint), intent(in) :: ip_rank
!
!
      call copy_gl_2_local_rlm_param(ip_rank)
!
      call allocate_spheric_param_rlm
      call allocate_sph_1d_index_rlm
!
      call copy_sph_1d_gl_idx_rlm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_global_sph_4_rlm', ip_rank
      call set_global_sph_4_rlm
!
      if(iflag_debug .gt. 0) call check_spheric_param_rlm(ip_rank)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rlm', ip_rank
      call const_comm_table_4_rlm(ip_rank, nnod_rlm)
!
      end subroutine const_sph_rlm_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_rtm_grids(ip_rank)
!
      use m_spheric_parameter
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer(kind = kint), intent(in) :: ip_rank
!
!
      call copy_gl_2_local_rtm_param(ip_rank)
!
      call allocate_spheric_param_rtm
      call allocate_sph_1d_index_rtm
!
      call copy_sph_1d_gl_idx_rtm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_global_sph_4_rtm', ip_rank
      call set_global_sph_4_rtm
!
      if(iflag_debug .gt. 0)  call check_spheric_param_rtm(ip_rank)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rtm', ip_rank
      call const_comm_table_4_rtm(ip_rank, nnod_rtm)
!
      end subroutine const_sph_rtm_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_fem_mesh_for_sph(ip_rank)
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use m_spheric_parameter
      use m_gauss_points
      use m_group_data_sph_specr
      use set_local_index_table_sph
      use set_local_sphere_by_global
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
      nidx_local_fem(1:3) = nidx_rtp(1:3)
      nidx_local_fem(3) =   m_folding * nidx_local_fem(3)
!
      call s_const_FEM_mesh_for_sph(ip_rank, radius_1d_gl, mesh, group)
!
      call copy_comm_tbl_type_to_IO(ip_rank, mesh%nod_comm)
      call copy_node_type_to_IO(mesh%node)
      call copy_ele_connect_type_to_IO(mesh%ele)
      call set_grp_data_type_to_IO(group)
!
      call dealloc_groups_data(group)
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
      call deallocate_type_comm_tbl(mesh%nod_comm)
!
      mesh_file_head = sph_file_head
      call sel_write_mesh_file(ip_rank)
!
      write(*,'(a,i6,a)')                                               &
     &          'FEM mesh for domain', ip_rank, ' is done.'
!
      end subroutine const_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rlm(ip_rank, nnod_rlm)
!
      use m_sph_trans_comm_table
      use set_comm_table_rtm_rlm
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nnod_rlm
!
!
      call allocate_ncomm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'count_comm_table_4_rlm', ip_rank
      call count_comm_table_4_rlm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'count_num_domain_rtm_rlm', ip_rank
      call count_num_domain_rtm_rlm(nneib_domain_rlm)
!
      call allocate_sph_comm_stack_rlm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_comm_stack_rtm_rlm', ip_rank
      call set_comm_stack_rtm_rlm(ip_rank, nneib_domain_rlm,            &
     &    id_domain_rlm, istack_sr_rlm, ntot_item_sr_rlm)
!
      call allocate_sph_comm_item_rlm(nnod_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_comm_table_4_rlm', ip_rank
      call set_comm_table_4_rlm
      call deallocate_ncomm
!
!      call allocate_idx_gl_rlm_out
!      call set_global_id_4_comm_rlm
!
      end subroutine const_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtm(ip_rank, nnod_rtm)
!
      use m_sph_trans_comm_table
      use set_comm_table_rtm_rlm
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nnod_rtm
!
!      write(*,*) 'allocate_ncomm'
      call allocate_ncomm
!
!      write(*,*) 'count_comm_table_4_rtm'
      call count_comm_table_4_rtm
!
!      write(*,*) 'count_num_domain_rtm_rlm'
      call count_num_domain_rtm_rlm(nneib_domain_rtm)
!
!      write(*,*) 'allocate_sph_comm_stack_rtm'
      call allocate_sph_comm_stack_rtm
!
!      write(*,*) 'set_comm_stack_rtm_rlm'
      call set_comm_stack_rtm_rlm(ip_rank, nneib_domain_rtm,            &
     &    id_domain_rtm, istack_sr_rtm, ntot_item_sr_rtm)
!
      call allocate_sph_comm_item_rtm(nnod_rtm)
!
!      write(*,*) 'set_comm_table_4_rtm'
      call set_comm_table_4_rtm
      call deallocate_ncomm
!
!      call allocate_idx_gl_rtm_out
!      write(*,*) 'set_global_id_4_comm_rtm'
!      call set_global_id_4_comm_rtm
!
      end subroutine const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      end module gen_sph_grids_modes
