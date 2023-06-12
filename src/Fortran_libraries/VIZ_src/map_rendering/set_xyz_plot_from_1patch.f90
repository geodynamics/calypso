!>@file   set_xyz_plot_from_1patch.f90
!!@brief  module set_xyz_plot_from_1patch
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Subroutines for plane projection
!!@verbatim
!!      subroutine set_xy_plot_from_1patch(psf_nod, psf_ele, psf_phys,  &
!!     &                                   iele, xy_patch, d_patch)
!!      subroutine set_xz_plot_from_1patch(psf_nod, psf_ele, psf_phys,  &
!!     &                                   iele, xy_patch, d_patch)
!!      subroutine set_yz_plot_from_1patch(psf_nod, psf_ele, psf_phys,  &
!!     &                                   iele, xy_patch, d_patch)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(phys_data), intent(in) :: psf_phys
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
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_xy_plot_from_1patch(psf_nod, psf_ele, psf_phys,    &
     &                                   iele, xy_patch, d_patch)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: xy_patch(2,3)
      real(kind = kreal), intent(inout) :: d_patch(3)
!
      integer(kind = kint) :: k1, j, inod
!
!
      do k1 = 1, 3
        inod = psf_ele%ie(iele,k1)
!
        do j = 1, 3
          xy_patch(1,j) = psf_nod%xx(inod,1)
          xy_patch(2,j) = psf_nod%xx(inod,2)
          d_patch(j) =   psf_phys%d_fld(inod,1)
        end do
      end do
!
      end subroutine set_xy_plot_from_1patch
!
!  ---------------------------------------------------------------------
!
      subroutine set_xz_plot_from_1patch(psf_nod, psf_ele, psf_phys,    &
     &                                   iele, xy_patch, d_patch)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: xy_patch(2,3)
      real(kind = kreal), intent(inout) :: d_patch(3)
!
      integer(kind = kint) :: k1, j, inod
!
      do k1 = 1, 3
        inod = psf_ele%ie(iele,k1)
!
        do j = 1, 3
          xy_patch(1,j) = psf_nod%xx(inod,1)
          xy_patch(2,j) = psf_nod%xx(inod,3)
          d_patch(j) =   psf_phys%d_fld(inod,1)
        end do
      end do
!
      end subroutine set_xz_plot_from_1patch
!
!  ---------------------------------------------------------------------
!
      subroutine set_yz_plot_from_1patch(psf_nod, psf_ele, psf_phys,    &
     &                                   iele, xy_patch, d_patch)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: xy_patch(2,3)
      real(kind = kreal), intent(inout) :: d_patch(3)
!
      integer(kind = kint) :: k1, j, inod
!
!
      do k1 = 1, 3
        inod = psf_ele%ie(iele,k1)
!
        do j = 1, 3
          xy_patch(1,j) = psf_nod%xx(inod,1)
          xy_patch(2,j) = psf_nod%xx(inod,2)
          d_patch(j) =   psf_phys%d_fld(inod,1)
        end do
      end do
!
      end subroutine set_yz_plot_from_1patch
!
!  ---------------------------------------------------------------------
!
      end module set_xyz_plot_from_1patch
