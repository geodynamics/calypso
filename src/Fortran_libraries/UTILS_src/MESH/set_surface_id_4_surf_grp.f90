!set_surface_id_4_surf_grp.f90
!      module set_surface_id_4_surf_grp
!
!     Writteg by H.Matsui on Aug., 2006
!
!> @brief set surface ID for surface group items
!
!      subroutine set_surface_id_4_surf_group(numele, isf_4_ele,        &
!     &          num_surf, num_surf_bc, surf_istack, surf_item,         &
!     &          isurf_grp, isurf_grp_n)
!
      module set_surface_id_4_surf_grp
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_surface_id_4_surf_group(numele, isf_4_ele,         &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          isurf_grp, isurf_grp_n)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(inout) :: isurf_grp(num_surf_bc)
      integer(kind = kint), intent(inout) :: isurf_grp_n(num_surf_bc)
!
      integer(kind = kint) :: i_grp
      integer(kind = kint) :: inum, ist, ied, iele, isf, isf_n
!
!
      do i_grp = 1, num_surf
        ist = surf_istack(i_grp-1) + 1
        ied = surf_istack(i_grp)
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          isf_n = (isf - mod(isf-1,2)) + mod(isf,2)
!
          isurf_grp(inum) = isf_4_ele(iele,isf)
          isurf_grp_n(inum) = isf_4_ele(iele,isf_n)
        end do
      end do
!
      end subroutine set_surface_id_4_surf_group
!
!-----------------------------------------------------------------------
!
      end module set_surface_id_4_surf_grp
