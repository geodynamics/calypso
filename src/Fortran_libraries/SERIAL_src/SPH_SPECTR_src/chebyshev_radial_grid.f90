!>@file   chebyshev_radial_grid.f90
!!@brief  module chebyshev_radial_grid
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in Sep., 2009
!
!> @brief Set radial grid data
!!
!!@verbatim
!!      subroutine set_chebyshev_distance_shell(num_layer, nlayer_ICB,  &
!!     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!!      subroutine count_chebyshev_ext_layers(nri, r_ICB, r_CMB,        &
!!     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!!@endverbatim
!
      module chebyshev_radial_grid
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
      subroutine set_chebyshev_distance_shell(num_layer, nlayer_ICB,    &
     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: kst, ked, k, nri
      real(kind = kreal) :: pi, dr, shell
!
!
      pi = four * atan(one)
      nri = nlayer_CMB - nlayer_ICB
      shell = r_CMB - r_ICB
!
      kst = 1
      ked = nlayer_ICB-nri/2 - 1
      do k = kst, ked
        r_grid(k) = (r_ICB - half * shell) * dble(k) / dble(ked+1)
      end do
!
      kst = max(ione,nlayer_ICB-nri/itwo)
      ked = nlayer_ICB-1
      do k = kst, ked
        r_grid(k) = r_ICB - half * shell * (one - cos( pi               &
     &              * dble(k-nlayer_ICB)/dble(nri)) )
      end do
!
      kst = nlayer_ICB
      ked = nlayer_CMB
      do k = kst, ked
        r_grid(k) = r_ICB + half * shell * (one - cos( pi               &
     &              * dble(k-nlayer_ICB)/dble(nri)) )
      end do
!
      kst = nlayer_CMB + 1
      ked = min(num_layer, nlayer_CMB + nri/2)
      do k = kst, ked
        r_grid(k) = r_CMB + half * shell * (one - cos( pi               &
     &              * dble(k-nlayer_CMB)/dble(nri)) )
      end do
      dr = r_grid(ked) - r_grid(ked-1)
!
      kst = nlayer_CMB + nri/2 + 1
      ked = num_layer
      do k = kst, ked
        r_grid(k) = r_grid(k-1) + dr
      end do
!
      end subroutine set_chebyshev_distance_shell
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine count_chebyshev_ext_layers(nri, r_ICB, r_CMB,          &
     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_min, r_max
!
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(inout) :: ntot_shell
!
      real(kind = kreal) :: pi, dr, r, shell
      integer(kind = kint) :: ngrid_icore, ngrid_ext
      integer(kind = kint) :: k
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
      dr =   half * shell * ( one - cos( pi/dble(nri)) )
!
!      Set grid by Chebyshev grid
      r = r_ICB
      k = 0
      do
        if(r .le. r_min) exit
        if(k .ge. nri/2) exit
!
        k = k + 1
        r = r_ICB - half * shell * (one - cos( pi*dble(k)/dble(nri)) )
        dr =   half * shell * ( cos( pi*dble(k-1)/dble(nri))            &
     &                        - cos( pi*dble(k  )/dble(nri)) )
!        write(*,*) k, r, dr
      end do
!
!
!      Set grid by equidistance grid
      if(k .eq. nri/2) k = int(aint((r - r_min) /  dr)) + nri/2
      ngrid_icore = k-1
      if(ngrid_icore .lt. 0) ngrid_icore = 0
!
!
      r = r_CMB
      k = 0
      do
        if(r .ge. r_max) exit
        if(k .ge. nri/2) exit
!
        k = k + 1
        r = r_CMB + half * shell * (one - cos( pi*dble(k)/dble(nri)) )
        dr =  half * shell * ( -cos( pi*dble(k  )/dble(nri))            &
     &                        + cos( pi*dble(k-1)/dble(nri)) )
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
      nlayer_ICB = ngrid_icore + 1
      nlayer_CMB = nlayer_ICB +  nri
      ntot_shell = nlayer_CMB + ngrid_ext
!
      end subroutine count_chebyshev_ext_layers
!
!  -------------------------------------------------------------------
!
      end module chebyshev_radial_grid
