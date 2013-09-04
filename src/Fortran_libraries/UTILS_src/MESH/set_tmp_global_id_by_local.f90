!set_tmp_global_id_by_local.f90
!      module set_tmp_global_id_by_local
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine set_global_nod_id_by_local
!      subroutine set_global_ele_id_by_local
!      subroutine set_global_surf_id_by_local
!      subroutine set_global_edge_id_by_local
!
      module set_tmp_global_id_by_local
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_global_nod_id_by_local
!
      use m_geometry_parameter
      use m_geometry_data
      use set_global_id_by_local
!
      integer(kind = kint) :: inod
!
      do inod = 1, numnod
        globalnodid(inod) = inod
      end do
!
      end subroutine set_global_nod_id_by_local
!
! ----------------------------------------------------------------------
!
      subroutine set_global_ele_id_by_local
!
      use m_geometry_parameter
      use m_geometry_data
      use set_global_id_by_local
!
      call s_set_global_id_by_local(numele, internal_ele,               &
     &    interior_ele, globalelmid)
!
      end subroutine set_global_ele_id_by_local
!
! ----------------------------------------------------------------------
!
      subroutine set_global_surf_id_by_local
!
      use m_geometry_parameter
      use m_geometry_data
      use set_global_id_by_local
!
      call s_set_global_id_by_local(numsurf, internal_surf,             &
     &    interior_surf, globalsurfid)
!
      end subroutine set_global_surf_id_by_local
!
! ----------------------------------------------------------------------
!
      subroutine set_global_edge_id_by_local
!
      use m_geometry_parameter
      use m_geometry_data
      use set_global_id_by_local
!
      call s_set_global_id_by_local(numedge, internal_edge,             &
     &    interior_edge, globaledgeid)
!
      end subroutine set_global_edge_id_by_local
!
! ----------------------------------------------------------------------
!
      end module set_tmp_global_id_by_local
