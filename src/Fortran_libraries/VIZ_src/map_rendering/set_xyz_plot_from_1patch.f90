!>@file   set_xyz_plot_from_1patch.f90
!!@brief  module set_xyz_plot_from_1patch
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Subroutines for plane projection
!!@verbatim
!!      subroutine set_xy_plot_from_1patch(psf_nod, psf_ele, d_scalar,  &
!!     &                                   iele, xy_patch, d_patch)
!!      subroutine set_xz_plot_from_1patch(psf_nod, psf_ele, d_scalar,  &
!!     &                                   iele, xy_patch, d_patch)
!!      subroutine set_yz_plot_from_1patch(psf_nod, psf_ele, d_scalar,  &
!!     &                                   iele, xy_patch, d_patch)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        integer(kind = kint), intent(in) :: iele
!!        real(kind = kreal), intent(inout) :: xy_patch(2,3)
!!        real(kind = kreal), intent(inout) :: d_patch(3)
!!@endverbatim
      module set_xyz_plot_from_1patch
!
      use m_precision
      use m_constants
!
      use t_geometry_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_xy_plot_from_1patch(psf_nod, psf_ele, d_scalar,    &
     &                                   iele, xy_patch, d_patch)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: xy_patch(2,3)
      real(kind = kreal), intent(inout) :: d_patch(3)
!
      integer(kind = kint) :: k1, inod
!
!
      do k1 = 1, 3
        inod = psf_ele%ie(iele,k1)
!
        xy_patch(1,k1) = psf_nod%xx(inod,1)
        xy_patch(2,k1) = psf_nod%xx(inod,2)
        d_patch(k1) =    d_scalar(inod)
      end do
!
      end subroutine set_xy_plot_from_1patch
!
!  ---------------------------------------------------------------------
!
      subroutine set_xz_plot_from_1patch(psf_nod, psf_ele, d_scalar,    &
     &                                   iele, xy_patch, d_patch)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: xy_patch(2,3)
      real(kind = kreal), intent(inout) :: d_patch(3)
!
      integer(kind = kint) :: k1, inod
!
      do k1 = 1, 3
        inod = psf_ele%ie(iele,k1)
        xy_patch(1,k1) = psf_nod%xx(inod,1)
        xy_patch(2,k1) = psf_nod%xx(inod,3)
        d_patch(k1) =    d_scalar(inod)
      end do
!
      end subroutine set_xz_plot_from_1patch
!
!  ---------------------------------------------------------------------
!
      subroutine set_yz_plot_from_1patch(psf_nod, psf_ele, d_scalar,    &
     &                                   iele, xy_patch, d_patch)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: xy_patch(2,3)
      real(kind = kreal), intent(inout) :: d_patch(3)
!
      integer(kind = kint) :: k1, inod
!
!
      do k1 = 1, 3
        inod = psf_ele%ie(iele,k1)
!
        xy_patch(1,k1) = psf_nod%xx(inod,1)
        xy_patch(2,k1) = psf_nod%xx(inod,2)
        d_patch(k1) =    d_scalar(inod)
      end do
!
      end subroutine set_yz_plot_from_1patch
!
!  ---------------------------------------------------------------------
!
      end module set_xyz_plot_from_1patch
