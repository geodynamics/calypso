!>@file   nod_and_ele_derived_info.f90
!!@brief  module nod_and_ele_derived_info
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct node and element derived informations
!!
!!@verbatim
!!      subroutine set_nod_and_ele_infos(node, ele)
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!      subroutine dealloc_nod_and_ele_infos(mesh)
!!      subroutine empty_nod_and_ele_infos(mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!
!!      subroutine dup_nod_and_ele_infos(org_mesh, new_mesh)
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!@endverbatim
!!
      module nod_and_ele_derived_info
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
!
      private :: copy_sph_node_position, copy_center_of_element
      private :: copy_node_param_smp, copy_ele_param_smp
      private :: copy_ele_overlap_flag, copy_subdomain_position_range
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_nod_and_ele_infos(node, ele)
!
      use set_size_4_smp_types
      use cal_mesh_position
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      logical :: read_element
!
!
      read_element = allocated(ele%x_ele)
!
      if(read_element .eqv. .false.) then
        call alloc_overlapped_ele(ele)
        call alloc_ele_geometry(ele)
        if (iflag_debug.eq.1) write(*,*) 'set_center_of_element'
        call set_center_of_element(node, ele)
      end if
!
       if (iflag_debug.eq.1) write(*,*) 'count_size_4_smp_mesh'
      call count_size_4_smp_mesh(node, ele)
!
       if (iflag_debug.eq.1) write(*,*) 'set_spherical_position'
      call set_spherical_position(node)
!
      call find_subdomain_position_range(node)
!
       if (iflag_debug.eq.1) write(*,*) 'count_overlap_ele'
      call count_overlap_ele(node, ele)
!
      end subroutine set_nod_and_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine empty_nod_and_ele_infos(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      mesh%ele%numele = 0
      call alloc_overlapped_ele(mesh%ele)
      call alloc_ele_geometry(mesh%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_node_param_smp'
      call alloc_node_param_smp(mesh%node)
      call alloc_ele_param_smp(mesh%ele)
!
      end subroutine empty_nod_and_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_nod_and_ele_infos(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_overlapped_ele(mesh%ele)
      call dealloc_ele_geometry(mesh%ele)
      call dealloc_ele_param_smp(mesh%ele)
      call dealloc_node_param_smp(mesh%node)
!
      end subroutine dealloc_nod_and_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine dup_nod_and_ele_infos(org_mesh, new_mesh)
!
      type(mesh_geometry), intent(in) :: org_mesh
      type(mesh_geometry), intent(inout) :: new_mesh
!
!
      call alloc_node_param_smp(new_mesh%node)
      call copy_node_param_smp(np_smp, org_mesh%node, new_mesh%node)
      call alloc_ele_param_smp(new_mesh%ele)
      call copy_ele_param_smp(np_smp, org_mesh%ele, new_mesh%ele)
!
      call alloc_overlapped_ele(new_mesh%ele)
      call copy_ele_overlap_flag(org_mesh%ele, new_mesh%ele)
!
      call alloc_ele_geometry(new_mesh%ele)
      call copy_center_of_element(org_mesh%ele, new_mesh%ele)
!
      call copy_sph_node_position(org_mesh%node, new_mesh%node)
      call copy_subdomain_position_range(org_mesh%node, new_mesh%node)
!
      end subroutine dup_nod_and_ele_infos
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_ele_overlap_flag(org_ele, new_ele)
!
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: iele
!
!
      new_ele%internal_ele = org_ele%internal_ele
!$omp parallel do
      do iele = 1, org_ele%numele
        new_ele%interior_ele(iele) = org_ele%interior_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_overlap_flag
!
! ----------------------------------------------------------------------
!
      subroutine copy_center_of_element(org_ele, new_ele)
!
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: iele
!
!$omp parallel do
      do iele = 1, org_ele%numele
        new_ele%x_ele(iele,1) = org_ele%x_ele(iele,1)
        new_ele%x_ele(iele,2) = org_ele%x_ele(iele,2)
        new_ele%x_ele(iele,3) = org_ele%x_ele(iele,3)
!
        new_ele%r_ele(iele) = org_ele%r_ele(iele)
        new_ele%ar_ele(iele) = org_ele%ar_ele(iele)
        new_ele%phi_ele(iele) = org_ele%phi_ele(iele)
        new_ele%theta_ele(iele) = org_ele%theta_ele(iele)
        new_ele%s_ele(iele) = org_ele%s_ele(iele)
        new_ele%as_ele(iele) = org_ele%as_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_center_of_element
!
! ----------------------------------------------------------------------
!
      subroutine copy_node_param_smp(np_smp, org_node, new_node)
!
      integer(kind = kint), intent(in) :: np_smp
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 0, np_smp
        new_node%istack_nod_smp(ip) = org_node%istack_nod_smp(ip)
        new_node%istack_internal_smp(ip)                                &
     &      = org_node%istack_internal_smp(ip)
      end do
!$omp end parallel do
!
      end subroutine copy_node_param_smp
!
! ----------------------------------------------------------------------
!
      subroutine copy_ele_param_smp(np_smp, org_ele, new_ele)
!
      integer(kind = kint), intent(in) :: np_smp
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 0, np_smp
        new_ele%istack_ele_smp(ip) = org_ele%istack_ele_smp(ip)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_param_smp
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_position(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, new_node%numnod
        new_node%rr(inod) =    org_node%rr(inod)
        new_node%theta(inod) = org_node%theta(inod)
        new_node%phi(inod) =   org_node%phi(inod)
        new_node%a_r(inod) =   org_node%a_r(inod)
        new_node%ss(inod) =    org_node%ss(inod)
        new_node%a_s(inod) =   org_node%a_s(inod)
      end do
!$omp end parallel do
!
      end subroutine copy_sph_node_position
!
! ----------------------------------------------------------------------
!
      subroutine copy_subdomain_position_range(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      new_node%xyz_max_lc(1:3) = org_node%xyz_max_lc(1:3)
      new_node%xyz_min_lc(1:3) = org_node%xyz_min_lc(1:3)
!
      end subroutine copy_subdomain_position_range
!
! ----------------------------------------------------------------------
!
      end module nod_and_ele_derived_info
