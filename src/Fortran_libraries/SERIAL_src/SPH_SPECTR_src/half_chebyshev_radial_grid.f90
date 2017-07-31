!>@file   half_chebyshev_radial_grid.f90
!!@brief  module half_chebyshev_radial_grid
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in Sep., 2009
!
!> @brief Set radial grid data
!!
!!@verbatim
!!      subroutine half_chebyshev_distance_shell                        &
!!     &         (num_layer, nlayer_CMB, r_CMB, r_grid)
!!      subroutine count_half_chebyshev_external(nri, r_CMB, r_max,     &
!!     &          ntot_shell, nlayer_ICB, nlayer_CMB)
!!@endverbatim
!
      module half_chebyshev_radial_grid
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
      subroutine half_chebyshev_distance_shell                          &
     &         (num_layer, nlayer_CMB, r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_CMB
      real(kind = kreal), intent(in) :: r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: kst, ked, k
      real(kind = kreal) :: pi, dr
!
!
      pi = four * atan(one)
!
      kst = 1
      ked = nlayer_CMB
      do k = kst, ked
        r_grid(k) = r_CMB * sin(half * pi * dble(k)/dble(nlayer_CMB))
      end do
!
      kst = nlayer_CMB + 1
      ked = min(num_layer, nlayer_CMB + nlayer_CMB/2)
      do k = kst, ked
        r_grid(k) = r_CMB + r_CMB * (one - sin(half*pi                  &
     &                       * dble(k-nlayer_CMB)/dble(nlayer_CMB)) )
      end do
      dr = r_grid(ked) - r_grid(ked-1)
!
      kst = nlayer_CMB + nlayer_CMB/2 + 1
      ked = num_layer
      do k = kst, ked
        r_grid(k) = r_grid(k-1) + dr
      end do
!
      end subroutine half_chebyshev_distance_shell
!
!  -------------------------------------------------------------------
!
      subroutine count_half_chebyshev_external(nri, r_CMB, r_max,       &
     &          ntot_shell, nlayer_ICB, nlayer_CMB)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_CMB
      real(kind = kreal), intent(in) :: r_max
!
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(inout) :: ntot_shell
!
      real(kind = kreal) :: pi, dr, r
      integer(kind = kint) :: ngrid_ext
      integer(kind = kint) :: k
!
!
      pi = four * atan(one)
      dr =   half * r_CMB * ( one - cos( pi/dble(nri)) )
!
      r = r_CMB
      k = 0
      do
        if(r .ge. r_max) exit
        if(k .ge. nri/2) exit
!
        k = k + 1
        r = r_CMB + r_CMB * (one - cos(half*pi*dble(k)/dble(nri)) )
        dr =  r_CMB * (-cos(half*pi*dble(k  )/dble(nri))                &
     &                 +cos(half*pi*dble(k-1)/dble(nri)) )
!        write(*,*) k, r, dr
      end do
!
!
      do
        if(r .ge. r_max) exit
        k = k + 1
        r = r + dr
!        write(*,*) k, r, dr
      end do
!
      if(k .le. 1) then
        ngrid_ext = 0
      else
        ngrid_ext = k
      end if
!
      nlayer_ICB = 0
      nlayer_CMB = nri
      ntot_shell = nlayer_CMB + ngrid_ext
!
      end subroutine count_half_chebyshev_external
!
!  -------------------------------------------------------------------
!
      end module half_chebyshev_radial_grid
