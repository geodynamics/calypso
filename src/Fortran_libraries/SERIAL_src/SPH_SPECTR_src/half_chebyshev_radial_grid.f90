!>@file   half_chebyshev_radial_grid.f90
!!@brief  module half_chebyshev_radial_grid
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in Sep., 2009
!
!> @brief Set radial grid data
!!
!!@verbatim
!!      subroutine half_chebyshev_distance_shell(num_layer,             &
!!     &          nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, r_grid)
!!      subroutine half_chebyshev_distance_sphere                       &
!!     &         (num_layer, nlayer_CMB, r_CMB, r_grid)
!!        integer(kind = kint), intent(in) :: num_layer
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(inout) :: r_grid(num_layer)
!!
!!      subroutine count_half_chebyshev_ext_layers(nri, r_ICB, r_CMB,   &
!!     &          r_min, r_max, ngrid_icore, ngrid_external)
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: r_min, r_max
!!        integer(kind = kint), intent(inout) :: ngrid_icore
!!        integer(kind = kint), intent(inout) :: ngrid_external
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
      subroutine half_chebyshev_distance_shell(num_layer,               &
     &          nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: kst, ked, k, nri
      real(kind = kreal), parameter:: pi = four * atan(one)
      real(kind = kreal) :: dr, shell
!
!
      nri = nlayer_CMB - nlayer_ICB
      shell = r_CMB - r_ICB
!
      do k = nlayer_ICB, nlayer_CMB
        r_grid(k) = r_ICB                                               &
     &           + shell * cos(half*pi * dble(k-nlayer_CMB)/dble(nri))
      end do
!
      kst = nlayer_CMB + 1
      ked = min(num_layer, nlayer_CMB + nri/2)
      do k = kst, ked
        r_grid(k) = r_ICB + shell * (two                                &
     &            - cos(half*pi * dble(k-nlayer_CMB)/dble(nri)))
      end do
      dr = r_grid(ked) - r_grid(ked-1)
!
      kst = nlayer_CMB + nri/2 + 1
      ked = num_layer
      do k = kst, ked
        r_grid(k) = r_grid(k-1) + dr
      end do
!
      dr = r_grid(nlayer_ICB+1) - r_grid(nlayer_ICB)
      do k = 1, nlayer_ICB-1
        r_grid(k) = r_ICB - (nlayer_ICB - k) * dr
      end do
!
      end subroutine half_chebyshev_distance_shell
!
!  -------------------------------------------------------------------
!
      subroutine half_chebyshev_distance_sphere                         &
     &         (num_layer, nlayer_CMB, r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_CMB
      real(kind = kreal), intent(in) :: r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: kst, ked, k
      real(kind = kreal) :: dr
      real(kind = kreal), parameter :: pi = four * atan(one)
!
!
      do k = 1, nlayer_CMB
        r_grid(k) = r_CMB                                               &
     &         * cos(half * pi * dble(nlayer_CMB - k)/dble(nlayer_CMB))
      end do
!
      ked = min(num_layer, nlayer_CMB + nlayer_CMB/2)
      do k = nlayer_CMB+1, ked
        r_grid(k) = r_CMB * (two                                        &
     &            - cos(half*pi * dble(k-nlayer_CMB)/dble(nlayer_CMB)))
      end do
      dr = r_grid(ked) - r_grid(ked-1)
!
      kst = nlayer_CMB + nlayer_CMB/2 + 1
      ked = num_layer
      do k = kst, ked
        r_grid(k) = r_grid(k-1) + dr
      end do
!
      end subroutine half_chebyshev_distance_sphere
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine count_half_chebyshev_ext_layers(nri, r_ICB, r_CMB,     &
     &          r_min, r_max, ngrid_icore, ngrid_external)
!
      use set_radial_grid_sph_shell
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_min, r_max
!
      integer(kind = kint), intent(inout) :: ngrid_icore
      integer(kind = kint), intent(inout) :: ngrid_external
!
      real(kind = kreal), parameter :: pi = four * atan(one)
      real(kind = kreal) :: shell, dr
!
!
      shell = r_CMB - r_ICB
      dr = shell * (cos(half*pi)                                        &
     &    - cos(half*pi * dble(nri+1)/dble(nri)))
      ngrid_icore =    count_equi_inner_sphere(dr, r_ICB, r_min)
      ngrid_external = count_half_chebyshev_external(shell, nri,        &
     &                                               r_CMB, r_max)
!
      end subroutine count_half_chebyshev_ext_layers
!
!  -------------------------------------------------------------------
!
      integer(kind = kint) function count_half_chebyshev_external       &
     &                                     (shell, nri, r_CMB, r_max)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: shell
      real(kind = kreal), intent(in) :: r_CMB
      real(kind = kreal), intent(in) :: r_max
!
      real(kind = kreal), parameter :: pi = four * atan(one)
      real(kind = kreal) :: dr, r
      integer(kind = kint) :: ngrid_icore, ngrid_ext
      integer(kind = kint) :: k
!
!
      r = r_CMB
      dr = shell * (one - cos(half*pi/dble(nri)))
      k = 0
      do
        if(r .ge. r_max) exit
        if(k .ge. nri/2) exit
!
        k = k + 1
        r =  r_CMB + half * shell * (one - cos( pi*dble(k)/dble(nri)))
        dr = shell * ( -cos(half*pi * dble(k  )/dble(nri))              &
                      + cos(half*pi * dble(k-1)/dble(nri)))
!        write(*,*) k, r, dr
      end do
!
      do
        if(r .ge. r_max) exit
        k = k + 1
        r = r + dr
!        write(*,*) k, r, dr
      end do
!
      count_half_chebyshev_external = k
      if(k .le. 1) count_half_chebyshev_external = 0
!
      end function count_half_chebyshev_external
!
!  -------------------------------------------------------------------
!
      end module half_chebyshev_radial_grid
