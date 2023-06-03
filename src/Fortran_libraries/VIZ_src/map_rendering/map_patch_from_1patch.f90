!>@file   map_patch_from_1patch.f90
!!@brief  module map_patch_from_1patch
!!
!!@author  H. Matsui
!!@date Programmed in May, 2023
!
!>@brief Divided triangle patch for map projection
!!
!!@verbatim
!!      subroutine s_set_map_patch_from_1patch(iele, nnod_psf, nele_psf,&
!!     &          xx_psf, ie_psf, ntot_comp, field_psf,                 &
!!     &          n_map_patch, x_map_patch, d_map_patch)
!!        integer(kind = kint), intent(in) :: iele
!!        integer(kind = kint), intent(in) :: nnod_psf, nele_psf
!!        integer(kind = kint), intent(in) :: ie_psf(nele_psf,3)
!!        real(kind = kreal), intent(in) :: xx_psf(nnod_psf,3)
!!        real(kind = kreal), intent(in) :: field_psf(nnod_psf,ntot_comp)
!!        integer(kind = kint), intent(inout) :: n_map_patch
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: x_map_patch(num_triangle,n_vector,n_map_patch)
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: d_map_patch(num_triangle,ntot_comp,n_map_patch)
!!@endverbatim
      module map_patch_from_1patch
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_geometry_constants
!
      implicit none
!
      integer(kind = kint), parameter, private :: ipatch_map_12_23(9)   &
     &       = (/ 1, 4, 5,  1, 5, 3,  4, 2, 3/)
      integer(kind = kint), parameter, private :: ipatch_map_12_31(9)   &
     &       = (/ 5, 2, 3,  5, 3, 4,  1, 5, 4/)
      integer(kind = kint), parameter, private :: ipatch_map_23_31(9)   &
     &       = (/ 1, 2, 4,  1, 4, 5,  5, 4, 3/)
      integer(kind = kint), parameter, private :: ipatch_map_12_3(6)    &
     &       = (/ 1, 4, 3,  4, 2, 3/)
      integer(kind = kint), parameter, private :: ipatch_map_23_1(6)    &
     &       = (/ 4, 1, 2,  1, 4, 3/)
      integer(kind = kint), parameter, private :: ipatch_map_31_2(6)    &
     &       = (/ 1, 2, 4,  4, 2, 3/)
!
      real(kind = kreal), parameter, private :: EPSILON = 1.0d-9
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_map_patch_from_1patch(iele, nnod_psf, nele_psf,  &
     &          xx_psf, ie_psf, ntot_comp, field_psf,                   &
     &          n_map_patch, x_map_patch, d_map_patch)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: nnod_psf, nele_psf
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ie_psf(nele_psf,3)
      real(kind = kreal), intent(in) :: xx_psf(nnod_psf,3)
      real(kind = kreal), intent(in) :: field_psf(nnod_psf,ntot_comp)
!
      integer(kind = kint), intent(inout) :: n_map_patch
      real(kind = kreal), intent(inout)                                 &
     &          :: x_map_patch(num_triangle,n_vector,n_map_patch)
      real(kind = kreal), intent(inout)                                 &
     &          :: d_map_patch(num_triangle,ntot_comp,n_map_patch)
!
      real(kind = kreal) :: x_map(5,3)
      real(kind = kreal), allocatable :: d_map(:,:)
!
      integer(kind = kint) :: inod, i, j, jj, k
      real(kind = kreal) :: y1, y2, y3
!
!
      allocate(d_map(9,ntot_comp))
      d_map(1:9,1:ntot_comp) = 0.0d0
!
      do i = 1, 3
        inod = ie_psf(iele,i)
        d_map(i,1:ntot_comp) =   real(field_psf(inod,1:ntot_comp))
!
        do j = 1, 3
          if(abs(xx_psf(inod,j)) .lt. EPSILON) then
            x_map(i,j) = 0.0d0
          else
            x_map(i,j) = xx_psf(inod,j)
          end if
        end do
      end do
      y1 = x_map(1,2)
      y2 = x_map(2,2)
      y3 = x_map(3,2)
!
!
      if     ( ((y1*y2).lt.zero) .and. ((y2*y3).lt.zero) )then
        x_map(4,1) = (y2*x_map(1,1) - y1*x_map(2,1)) / (y2- y1)
        x_map(4,2) = zero
        x_map(4,3) = (y2*x_map(1,3) - y1*x_map(2,3)) / (y2- y1)
        d_map(4,1:ntot_comp)  = (y2*d_map(1,1:ntot_comp)                &
     &                         - y1*d_map(2,1:ntot_comp)) / (y2- y1)
!
        x_map(5,1) = (y3*x_map(2,1) - y2*x_map(3,1)) / (y3- y2)
        x_map(5,2) = zero
        x_map(5,3) = (y3*x_map(2,3) - y2*x_map(3,3)) / (y3- y2)
        d_map(5,1:ntot_comp) = (y3*d_map(2,1:ntot_comp)                 &
     &                        - y2*d_map(3,1:ntot_comp)) /   (y3- y2)
!
        n_map_patch = 3
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_12_23(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,1:ntot_comp,i) =   d_map(k,1:ntot_comp)
          end do
        end do
!
      else if( ((y1*y2).lt.zero) .and. (y3.eq.zero) )then
        x_map(4,1) = (y2*x_map(1,1) - y1*x_map(2,1)) / (y2- y1)
        x_map(4,2) = zero
        x_map(4,3) = (y2*x_map(1,3) - y1*x_map(2,3)) / (y2- y1)
        d_map(4,1:ntot_comp) = (y2*d_map(1,1:ntot_comp)                 &
     &                        - y1*d_map(2,1:ntot_comp)) / (y2- y1)
!
        n_map_patch = 2
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_12_3(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,1:ntot_comp,i) =   d_map(k,1:ntot_comp)
          end do
        end do
!
!
      else if( ((y1*y2).lt.zero) .and. ((y3*y1).lt.zero) ) then
        x_map(4,1) = (y2*x_map(1,1) - y1*x_map(2,1)) / (y2- y1)
        x_map(4,2) = zero
        x_map(4,3) = (y2*x_map(1,3) - y1*x_map(2,3)) / (y2- y1)
        d_map(4,1:ntot_comp) = (y2*d_map(1,1:ntot_comp)                 &
     &                        - y1*d_map(2,1:ntot_comp)) / (y2- y1)
!
        x_map(5,1) = (y1*x_map(3,1) - y3*x_map(1,1)) / (y1- y3)
        x_map(5,2) = zero
        x_map(5,3) = (y1*x_map(3,3) - y3*x_map(1,3)) / (y1- y3)
        d_map(5,1:ntot_comp) = (y1*d_map(3,1:ntot_comp)                 &
     &                        - y3*d_map(1,1:ntot_comp)) / (y1- y3)
!
        n_map_patch = 3
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_12_31(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,1:ntot_comp,i) =   d_map(k,1:ntot_comp)
          end do
        end do
!
      else if( ((y2*y3).lt.zero) .and. (y1.eq.zero) )then
        x_map(4,1) = (y3*x_map(2,1) - y2*x_map(3,1)) / (y3- y2)
        x_map(4,2) = zero
        x_map(4,3) = (y3*x_map(2,3) - y2*x_map(3,3)) / (y3- y2)
        d_map(4,1:ntot_comp) = (y3*d_map(2,1:ntot_comp)                 &
     &                        - y2*d_map(3,1:ntot_comp)) / (y3- y2)
!
        n_map_patch = 2
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_23_1(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,1:ntot_comp,i) =   d_map(k,1:ntot_comp)
          end do
        end do
!
      else if( ((y2*y3).lt.zero) .and. ((y3*y1).lt.zero) ) then
        x_map(4,1) = (y3*x_map(2,1) - y2*x_map(3,1)) / (y3- y2)
        x_map(4,2) = zero
        x_map(4,3) = (y3*x_map(2,3) - y2*x_map(3,3)) / (y3- y2)
        d_map(4,1:ntot_comp) = (y3*d_map(2,1:ntot_comp)                 &
     &                        - y2*d_map(3,1:ntot_comp)) /   (y3- y2)
!
        x_map(5,1) = (y1*x_map(3,1) - y3*x_map(1,1)) / (y1- y3)
        x_map(5,2) = zero
        x_map(5,3) = (y1*x_map(3,3) - y3*x_map(1,3)) / (y1- y3)
        d_map(5,1:ntot_comp) = (y1*d_map(3,1:ntot_comp)                 &
     &                        - y3*d_map(1,1:ntot_comp)) / (y1- y3)
!
        n_map_patch = 3
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_23_31(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,1:ntot_comp,i) =   d_map(k,1:ntot_comp)
          end do
        end do
!
      else if( ((y3*y1).lt.zero) .and. (y2.eq.zero) )then
        x_map(4,1) = (y1*x_map(3,1) - y3*x_map(1,1)) / (y1- y3)
        x_map(4,2) = zero
        x_map(4,3) = (y1*x_map(3,3) - y3*x_map(1,3)) / (y1- y3)
        d_map(4,1:ntot_comp) = (y1*d_map(3,1:ntot_comp)                 &
     &                        - y3*d_map(1,1:ntot_comp)) / (y1- y3)
!
        n_map_patch = 2
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_31_2(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,1:ntot_comp,i) =   d_map(k,1:ntot_comp)
          end do
        end do
!
      else
        n_map_patch = 1
        do j = 1, 3
          x_map_patch(j,1,1) = x_map(j,1)
          x_map_patch(j,2,1) = x_map(j,2)
          x_map_patch(j,3,1) = x_map(j,3)
          d_map_patch(j,1:ntot_comp,1) =    d_map(j,1:ntot_comp)
        end do
      end if
      deallocate(d_map)
!
      end subroutine s_set_map_patch_from_1patch
!
!-----------------------------------------------------------------------
!
      end module map_patch_from_1patch
