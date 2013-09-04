!const_surface_data.f90
!      module const_surface_data
!
!     Written by H. Matsui on Apr., 2006
!
!
!      subroutine construct_surface_data(my_rank)
!      subroutine const_element_list_4_surface
!
      module const_surface_data
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      implicit none
!
      private :: const_all_surface_data
      private :: const_external_surface_data
      private :: const_isolate_surface_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_surface_data(my_rank)
!
      use m_surface_hash
!
      use set_surface_hash
      use check_geometries
!
      integer(kind = kint), intent(in) :: my_rank
!
!   set hash data for suface elements using sum of local node ID
!
!
      call allocate_surface_hash(numnod, numele, nnod_4_surf)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_surface_hash'
      call count_surface_hash(numnod, numele, nnod_4_ele,               &
     &          nnod_4_surf, ie)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_surf_hash'
      call set_surf_hash(numele, nnod_4_ele, ie)
!
!
      call const_all_surface_data
!      call check_surface_data(my_rank)
!
!      call const_external_surface_data
!      call check_external_surface(my_rank)
!
!      call const_isolate_surface_data
!      call check_iso_surface(my_rank)
!
!
      if (iflag_debug.eq.1) write(*,*) 'deallocate_surface_hash'
      call deallocate_surface_hash
!
      end subroutine construct_surface_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_element_list_4_surface
!
      use set_element_list_4_surface
!
!
      call allocate_ele_4_surf
      call set_ele_list_4_surf(numele, numsurf, nsurf_4_ele,            &
     &    isf_4_ele, iele_4_surf)
!
      end subroutine const_element_list_4_surface
!
!------------------------------------------------------------------
!
      subroutine const_all_surface_data
!
      use mark_surf_hash
      use set_surface_data
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_all_surfaces'
      call mark_all_surfaces(numele, nnod_4_ele, ie)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_all_surfaces'
      call count_all_surfaces(numele, numsurf)
!
      call allocate_surface_connect
!
      if (iflag_debug.eq.1) write(*,*) 'set_all_surfaces'
      call set_all_surfaces(numele, numsurf, nnod_4_ele,                &
     &          nnod_4_surf, ie, node_on_sf, ie_surf, isf_4_ele)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_surf_rotation_flag'
      call set_surf_rotation_flag(numele, numsurf, nnod_4_ele,          &
     &    nnod_4_surf, ie, ie_surf, isf_4_ele, isf_rot_ele)
!
      end subroutine const_all_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_external_surface_data
!
      use mark_surf_hash
      use set_surface_data
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(numele, nnod_4_ele, ie)
!
      if (iflag_debug.eq.1) write(*,*) 'mark_external_surface'
      call mark_external_surface(internal_node, numele, nnod_4_ele, ie)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface(numele, numsurf_ext)
!
      call allocate_ext_surface
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface(numele, numele, numsurf_ext,                &
     &          isf_4_ele, isf_external)
!
      end subroutine const_external_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_isolate_surface_data
!
      use mark_surf_hash
      use set_surface_data
!
!
!   mark independent surface
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(numele, nnod_4_ele, ie)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface(numele, numsurf_iso)
!
      call allocate_iso_surface
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface(numele, numele, numsurf_iso,                &
     &          isf_4_ele, isf_isolate)
!
      end subroutine const_isolate_surface_data
!
!------------------------------------------------------------------
!
      end module const_surface_data
