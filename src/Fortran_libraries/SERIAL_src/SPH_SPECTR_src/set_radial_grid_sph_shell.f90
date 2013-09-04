!set_radial_grid_sph_shell.f90
!      module set_radial_grid_sph_shell
!
!      Written by H. Matsui on Sep., 2009
!
!      subroutine set_equi_distance_shell(num_layer, nlayer_ICB,        &
!     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!      subroutine set_chebyshev_distance_shell(num_layer, nlayer_ICB,   &
!     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!
!      subroutine count_equi_ext_layers(nri, r_ICB, r_CMB,              &
!     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!      subroutine count_chebyshev_ext_layers(nri, r_ICB, r_CMB,         &
!     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!
!      subroutine set_radial_distance_flag(num_layer, nlayer_ICB,       &
!     &          nlayer_CMB, r_ICB, r_CMB, r_grid, iflag_rgrid)
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
        ngrid_icore = int(aint((r_ICB - r_min) /  dr))
      end if
      if(ngrid_icore .lt. 0) ngrid_icore = 0
!      r_min = r_ICB - dr * dble(ngrid_icore)
!
      if(r_max .le. r_CMB) then
        ngrid_ext = 0
      else
        ngrid_ext =   int(aint((r_max - r_CMB) /  dr)) + 1
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
!
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
      do
        if(r .le. r_min) exit
        k = k + 1
        r = r - dr
!        write(*,*) k, r, dr
      end do
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
!  -------------------------------------------------------------------
!
      subroutine set_radial_distance_flag(num_layer, nlayer_ICB,        &
     &          nlayer_CMB, r_ICB, r_CMB, r_grid, iflag_rgrid)
!
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
      real(kind = kreal) :: diff, diff_ch_max, diff_eq_max
!
      real(kind = kreal), allocatable :: r_eq(:), r_ch(:)
!
!
      allocate( r_eq(num_layer) )
      allocate( r_ch(num_layer) )
!
      call set_equi_distance_shell(num_layer, nlayer_ICB, nlayer_CMB,   &
     &    r_ICB, r_CMB, r_eq)
      call set_chebyshev_distance_shell(num_layer, nlayer_ICB,          &
     &    nlayer_CMB, r_ICB, r_CMB, r_ch)
!
!
      diff_eq_max = abs( r_grid(1) - r_eq(1)) / r_eq(1)
      diff_ch_max = abs( r_grid(1) - r_ch(1)) / r_ch(1)
!
      do k = 2, num_layer
        diff = abs( r_grid(k) - r_eq(k)) / r_eq(k)
        diff_eq_max = max(diff_eq_max,diff)
!
        diff = abs( r_grid(k) - r_ch(k)) / r_ch(k)
        diff_ch_max = max(diff_ch_max,diff)
      end do
!
      if      (diff_ch_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_Chebyshev
      else if (diff_eq_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_euqidistance
      else
        iflag_rgrid = igrid_non_euqidist
      end if
!
      deallocate( r_eq, r_ch )
!
      end subroutine set_radial_distance_flag
!
!  -------------------------------------------------------------------
!
      end module set_radial_grid_sph_shell
