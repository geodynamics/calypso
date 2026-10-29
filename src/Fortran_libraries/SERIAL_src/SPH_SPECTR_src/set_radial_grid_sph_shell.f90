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
!!      subroutine set_equi_distance_sphere(num_layer, nlayer_CMB,      &
!!     &                                    r_CMB, r_grid)
!!        integer(kind = kint), intent(in) :: num_layer
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(inout) :: r_grid(num_layer)
!!      subroutine count_equi_ext_layers(nri, r_ICB, r_CMB,             &
!!     &          r_min, r_max, ngrid_icore, ngrid_external)
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: r_min, r_max
!!        integer(kind = kint), intent(inout) :: ngrid_icore
!!        integer(kind = kint), intent(inout) :: ngrid_external
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
      integer(kind = kint) :: k
!
!$omp parallel do
      do k = 1, num_layer
        r_grid(k) = r_ICB + (r_CMB - r_ICB) * dble(k - nlayer_ICB)      &
     &                     / dble(nlayer_CMB - nlayer_ICB) 
      end do
!$omp end parallel do
!
      end subroutine set_equi_distance_shell
!
!  -------------------------------------------------------------------
!
      subroutine set_equi_distance_sphere(num_layer, nlayer_CMB,        &
     &                                    r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_CMB
      real(kind = kreal), intent(in) :: r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: k
!
!$omp parallel do
      do k = 1, num_layer
        r_grid(k) =  r_CMB * dble(k) / dble(nlayer_CMB) 
      end do
!$omp end parallel do
!
      end subroutine set_equi_distance_sphere
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine count_equi_ext_layers(nri, r_ICB, r_CMB,               &
     &          r_min, r_max, ngrid_icore, ngrid_external)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_min, r_max
!
      integer(kind = kint), intent(inout) :: ngrid_icore
      integer(kind = kint), intent(inout) :: ngrid_external
!
      real(kind = kreal) :: dr
!
!
      dr = (r_CMB - r_ICB) / dble(nri)
      ngrid_icore =    count_equi_inner_sphere(dr, r_ICB, r_min)
      ngrid_external = count_equi_external(dr, r_CMB, r_max)
!
      end subroutine count_equi_ext_layers
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      integer(kind = kint) function count_equi_inner_sphere             &
     &                                            (dr, r_ICB, r_min)
!
      real(kind = kreal), intent(in) :: dr
      real(kind = kreal), intent(in) :: r_ICB
      real(kind = kreal), intent(in) :: r_min
!
      integer(kind = kint) :: ngrid_icore
!
!
!      r_min = r_ICB - dr * dble(ngrid_icore)
!
      if(r_min .ge. r_ICB .or. r_ICB .eq. zero) then
        ngrid_icore = 0
      else
        ngrid_icore = int(aint((r_ICB - r_min)/dr), KIND(ngrid_icore))
      end if
      count_equi_inner_sphere = max(ngrid_icore, 0)
!
      end function count_equi_inner_sphere
!
!  -------------------------------------------------------------------
!
      integer(kind = kint) function count_equi_external                 &
     &                                            (dr, r_CMB, r_max)
!
      real(kind = kreal), intent(in) :: dr
      real(kind = kreal), intent(in) :: r_CMB
      real(kind = kreal), intent(in) :: r_max
!
      integer(kind = kint) :: ngrid_ext
!
!
!      r_max =  r_CMB + dr * dble(ngrid_ext)
      if(r_max .le. r_CMB) then
        ngrid_ext = 0
      else
        ngrid_ext = int(aint((r_max - r_CMB)/dr), KIND(ngrid_ext)) + 1
      end if
      count_equi_external = max(ngrid_ext, 0)
!
      end function count_equi_external
!
!  -------------------------------------------------------------------
!
      end module set_radial_grid_sph_shell
