!>@file   set_radial_grid_sph_shell.f90
!!@brief  module set_radial_grid_sph_shell
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in Sep., 2009
!
!> @brief Set radial grid data
!!
!!@verbatim
!!      subroutine set_equi_distance_shell(num_layer, nlayer_ICB,       &
!!     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!!      subroutine count_equi_ext_layers(nri, r_ICB, r_CMB,             &
!!     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!!
!!      subroutine set_radial_distance_flag(num_layer, nlayer_ICB,      &
!!     &          nlayer_CMB, r_ICB, r_CMB, r_grid, iflag_rgrid)
!!@endverbatim
!
      module set_radial_grid_sph_shell
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_equi_distance_shell(num_layer, nlayer_ICB,         &
     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: k, nri
!
!
      nri = nlayer_CMB - nlayer_ICB
!
      do k = 1, num_layer
        r_grid(k) = r_ICB + (r_CMB - r_ICB) * dble(k - nlayer_ICB)      &
     &             / dble(nri) 
      end do
!
      end subroutine set_equi_distance_shell
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine count_equi_ext_layers(nri, r_ICB, r_CMB,               &
     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_min, r_max
!
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(inout) :: ntot_shell
!
      real(kind = kreal) :: dr
      integer(kind = kint) :: ngrid_icore, ngrid_ext
!
!
      dr = (r_CMB - r_ICB) / dble(nri)
!
      if(r_min .ge. r_ICB) then
        ngrid_icore = 0
      else
        ngrid_icore = int(aint((r_ICB - r_min)/dr), KIND(ngrid_icore))
      end if
      if(ngrid_icore .lt. 0) ngrid_icore = 0
!      r_min = r_ICB - dr * dble(ngrid_icore)
!
      if(r_max .le. r_CMB) then
        ngrid_ext = 0
      else
        ngrid_ext = int(aint((r_max - r_CMB)/dr), KIND(ngrid_ext)) + 1
      end if
      if(ngrid_ext .lt. 0) ngrid_ext = 0
!      r_max =  r_CMB + dr * dble(ngrid_ext)
!
      nlayer_ICB = ngrid_icore + 1
      nlayer_CMB = nlayer_ICB +  nri
      ntot_shell = nlayer_CMB + ngrid_ext
!
      end subroutine count_equi_ext_layers
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_radial_distance_flag(num_layer, nlayer_ICB,        &
     &          nlayer_CMB, r_ICB, r_CMB, r_grid, iflag_rgrid)
!
      use chebyshev_radial_grid
      use half_chebyshev_radial_grid
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_grid(num_layer)
!
      integer(kind = kint), intent(inout) :: iflag_rgrid
!
      integer(kind = kint) :: k
      real(kind = kreal) :: diff
      real(kind = kreal) :: diff_ch_max, diff_eq_max, diff_hch_max
!
      real(kind = kreal), allocatable :: r_eq(:), r_ch(:), r_hch(:)
!
!
      allocate( r_eq(num_layer) )
      allocate( r_ch(num_layer) )
      allocate( r_hch(num_layer) )
!
      call set_equi_distance_shell(num_layer, nlayer_ICB, nlayer_CMB,   &
     &    r_ICB, r_CMB, r_eq)
      call set_chebyshev_distance_shell(num_layer, nlayer_ICB,          &
     &    nlayer_CMB, r_ICB, r_CMB, r_ch)
      call half_chebyshev_distance_shell(num_layer, nlayer_CMB,         &
     &    r_CMB, r_hch)
!
!
      diff_eq_max =  abs( r_grid(1) - r_eq(1)) /  r_eq(1)
      diff_ch_max =  abs( r_grid(1) - r_ch(1)) /  r_ch(1)
      diff_hch_max = abs( r_grid(1) - r_hch(1)) / r_hch(1)
!
      do k = 2, num_layer
        diff = abs( r_grid(k) - r_eq(k)) / r_eq(k)
        diff_eq_max = max(diff_eq_max,diff)
!
        diff = abs( r_grid(k) - r_ch(k)) / r_ch(k)
        diff_ch_max = max(diff_ch_max,diff)
!
        diff = abs( r_grid(k) - r_hch(k)) / r_hch(k)
        diff_hch_max = max(diff_hch_max,diff)
      end do
!
      if      (diff_ch_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_Chebyshev
      else if (diff_eq_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_equidistance
      else if (diff_hch_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_half_Chebyshev
      else
        iflag_rgrid = igrid_non_equidist
      end if
!
      deallocate( r_eq, r_ch, r_hch)
!
      end subroutine set_radial_distance_flag
!
!  -------------------------------------------------------------------
!
      end module set_radial_grid_sph_shell
