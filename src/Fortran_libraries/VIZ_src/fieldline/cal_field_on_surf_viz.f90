!>@file  cal_field_on_surf_viz.f90
!!       module cal_field_on_surf_viz
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief easy field evaluation on surface by linear interpolation
!!
!!@verbatim
!!      subroutine element_ave_4_viz(nnod, nele, ie, v_nod, s_nod,      &
!!     &          iele, v_ave, s_ave)
!!
!!      subroutine cal_field_on_surf_vect4(nnod, nsurf, nnod_sf,        &
!!     &          ie_surf, isurf, xi, v_nod, v4_tgt)
!!      subroutine cal_field_on_surf_vector(nnod, nsurf, nnod_sf,       &
!!     &          ie_surf, isurf, xi, v_nod, v_tgt)
!!      subroutine cal_field_on_surf_scalar(nnod, nsurf, nnod_sf,       &
!!     &           ie_surf, isurf, xi, s_nod, s_tgt)
!!
!!      subroutine cal_surf_field_value_2d(nd, xi, fd, ft)
!!        integer(kind = kint), intent(in) :: nd
!!        real(kind = kreal), intent(in) :: xi(2)
!!        real(kind = kreal), intent(in) :: fd(nd,4)
!!        real(kind = kreal), intent(inout) :: ft(nd)
!!@endverbatim
!
      module cal_field_on_surf_viz
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine element_ave_4_viz(nnod, nele, ie, v_nod, s_nod,        &
     &          iele, v_ave, s_ave)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nnod
      integer(kind = kint), intent(in) :: ie(nele,num_t_linear)
      real(kind = kreal), intent(in) :: v_nod(nnod,3), s_nod(nnod)
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: v_ave(3), s_ave
!
      integer(kind = kint) :: k1,  inod
      real(kind = kreal) :: v_ele(3,num_t_linear),  s_ele(num_t_linear)
!
!
      do k1 = 1, num_t_linear
        inod = ie(iele,k1)
        v_ele(1,k1) = v_nod(inod,1)
        v_ele(2,k1) = v_nod(inod,2)
        v_ele(3,k1) = v_nod(inod,3)
        s_ele(k1) =   s_nod(inod)
      end do
!
      v_ave(1) = (v_ele(1,1) + v_ele(1,2) + v_ele(1,3) + v_ele(1,4)     &
     &          + v_ele(1,5) + v_ele(1,6) + v_ele(1,7) + v_ele(1,8))    &
     &           * r125
      v_ave(2) = (v_ele(2,1) + v_ele(2,2) + v_ele(2,3) + v_ele(2,4)     &
     &          + v_ele(2,5) + v_ele(2,6) + v_ele(2,7) + v_ele(2,8))    &
     &           * r125
      v_ave(3) = (v_ele(3,1) + v_ele(3,2) + v_ele(3,3) + v_ele(3,4)     &
     &          + v_ele(3,5) + v_ele(3,6) + v_ele(3,7) + v_ele(3,8))    &
     &           * r125
      s_ave =    (s_ele(1) + s_ele(2) + s_ele(3) + s_ele(4)             &
     &          + s_ele(5) + s_ele(6) + s_ele(7) + s_ele(8)) * r125
!
      end subroutine element_ave_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine cal_field_on_surf_vect4(nnod, nsurf, nnod_sf,          &
     &          ie_surf, isurf, xi, v_nod, v4_tgt)
!
      integer(kind = kint), intent(in) :: isurf
      integer(kind = kint), intent(in) :: nnod, nsurf, nnod_sf
      integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_sf)
      real(kind = kreal), intent(in) :: xi(2)
      real(kind = kreal), intent(in) :: v_nod(nnod,3)
      real(kind = kreal), intent(inout) :: v4_tgt(4)
!
      real(kind = kreal) :: fd(4,4)
      integer(kind = kint) :: k1, inod
!
!
      do k1 = 1, 4
        inod = ie_surf(isurf,k1)
        fd(1:3,k1) = v_nod(inod,1:3)
        fd(4,k1) =   one
      end do
!
      call cal_surf_field_value_2d(ifour, xi, fd, v4_tgt(1))
!
      end subroutine cal_field_on_surf_vect4
!
!  ---------------------------------------------------------------------
!
      subroutine cal_field_on_surf_vector(nnod, nsurf, nnod_sf,         &
     &          ie_surf, isurf, xi, v_nod, v_tgt)
!
      integer(kind = kint), intent(in) :: isurf
      integer(kind = kint), intent(in) :: nnod, nsurf, nnod_sf
      integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_sf)
      real(kind = kreal), intent(in) :: xi(2)
      real(kind = kreal), intent(in) :: v_nod(nnod,3)
      real(kind = kreal), intent(inout) :: v_tgt(3)
!
      real(kind = kreal) :: fd(3,4)
      integer(kind = kint) :: k1, inod
!
!
      do k1 = 1, 4
        inod = ie_surf(isurf,k1)
        fd(1:3,k1) = v_nod(inod,1:3)
      end do
!
      call cal_surf_field_value_2d(ithree, xi, fd, v_tgt)
!
      end subroutine cal_field_on_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine cal_field_on_surf_scalar(nnod, nsurf, nnod_sf,         &
     &           ie_surf, isurf, xi, s_nod, s_tgt)
!
      integer(kind = kint), intent(in) :: isurf
      integer(kind = kint), intent(in) :: nnod, nsurf, nnod_sf
      integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_sf)
      real(kind = kreal), intent(in) :: s_nod(nnod)
      real(kind = kreal), intent(in) :: xi(2)
      real(kind = kreal), intent(inout) :: s_tgt(1)
!
      real(kind = kreal) :: fd(4)
      integer(kind = kint) :: k1, inod
!
!
      do k1 = 1, 4
        inod = ie_surf(isurf,k1)
        fd(k1) =   s_nod(inod)
      end do
!
      call cal_surf_field_value_2d(ione, xi, fd(1), s_tgt(1))
!
      end subroutine cal_field_on_surf_scalar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_surf_field_value_2d(nd, xi, fd, ft)
!
      integer(kind = kint), intent(in) :: nd
      real(kind = kreal), intent(in) :: xi(2)
      real(kind = kreal), intent(in) :: fd(nd,4)
      real(kind = kreal), intent(inout) :: ft(nd)
!
      real(kind = kreal) :: an(4)
!
      an(1) = half*half * (one-xi(1)) * (one-xi(2))
      an(2) = half*half * (one+xi(1)) * (one-xi(2))
      an(3) = half*half * (one+xi(1)) * (one+xi(2))
      an(4) = half*half * (one-xi(1)) * (one+xi(2))
!
      ft(1:nd) = (an(1)*fd(1:nd,1) + an(2)*fd(1:nd,2)                   &
     &          + an(3)*fd(1:nd,3) + an(4)*fd(1:nd,4))
!
      end subroutine cal_surf_field_value_2d
!
!  ---------------------------------------------------------------------
!
      end module cal_field_on_surf_viz
