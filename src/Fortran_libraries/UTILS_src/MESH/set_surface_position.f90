!
!      module set_surface_position
!
!      Written by H. Matsui on May, 2006
!
!      subroutine set_linear_surf_position(numnod, numsurf, ie_surf, xx,&
!     &          x_surf)
!      subroutine set_quad_surf_position(numnod, numsurf, ie_surf, xx,  &
!     &          x_surf)
!      subroutine set_lag_surf_position(numnod, numsurf, ie_surf, xx,   &
!     &          x_surf)
!
!      subroutine set_triangle_surf_position(numnod, numsurf, ie_surf,  &
!     &          xx, x_surf)
!
      module set_surface_position
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
      subroutine set_linear_surf_position(numnod, numsurf, ie_surf, xx, &
     &          x_surf)
!
      integer(kind = kint), intent(in) :: numnod, numsurf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,4)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_surf(numsurf,3)
!
      integer(kind = kint) :: isurf
      integer(kind = kint) ::  i1,  i2,  i3,  i4
!
!
!$omp parallel do private(i1,i2,i3,i4)
        do isurf = 1, numsurf
          i1 = ie_surf(isurf,1)
          i2 = ie_surf(isurf,2)
          i3 = ie_surf(isurf,3)
          i4 = ie_surf(isurf,4)
!
          x_surf(isurf,1)                                               &
     &          = quad * (xx(i1,1) + xx(i2,1) + xx(i3,1) + xx(i4,1))
          x_surf(isurf,2)                                               &
     &          = quad * (xx(i1,2) + xx(i2,2) + xx(i3,2) + xx(i4,2))
          x_surf(isurf,3)                                               &
     &          = quad * (xx(i1,3) + xx(i2,3) + xx(i3,3) + xx(i4,3))
        end do
!$omp end parallel do
!
      end subroutine set_linear_surf_position
!
!  ---------------------------------------------------------------------
!
      subroutine set_quad_surf_position(numnod, numsurf, ie_surf, xx,   &
     &          x_surf)
!
      integer(kind = kint), intent(in) :: numnod, numsurf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,8)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_surf(numsurf,3)
!
      integer(kind = kint) :: isurf
      integer(kind = kint) ::  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
!$omp parallel do private(i1,i2,i3,i4,i5,i6,i7,i8)
        do isurf = 1, numsurf
          i1 = ie_surf(isurf,1)
          i2 = ie_surf(isurf,2)
          i3 = ie_surf(isurf,3)
          i4 = ie_surf(isurf,4)
          i5 = ie_surf(isurf,5)
          i6 = ie_surf(isurf,6)
          i7 = ie_surf(isurf,7)
          i8 = ie_surf(isurf,8)
!
          x_surf(isurf,1)                                               &
     &          = - quad * (xx(i1,1) + xx(i2,1) + xx(i3,1) + xx(i4,1))  &
     &            + half * (xx(i5,1) + xx(i6,1) + xx(i7,1) + xx(i8,1))
          x_surf(isurf,2)                                               &
     &          = - quad * (xx(i1,2) + xx(i2,2) + xx(i3,2) + xx(i4,2))  &
     &            + half * (xx(i5,2) + xx(i6,2) + xx(i7,2) + xx(i8,2))
          x_surf(isurf,3)                                               &
     &          = - quad * (xx(i1,3) + xx(i2,3) + xx(i3,3) + xx(i4,3))  &
     &            + half * (xx(i5,3) + xx(i6,3) + xx(i7,3) + xx(i8,3))
        end do
!$omp end parallel do
!
      end subroutine set_quad_surf_position
!
!  ---------------------------------------------------------------------
!
      subroutine set_lag_surf_position(numnod, numsurf, ie_surf, xx,    &
     &          x_surf)
!
      integer(kind = kint), intent(in) :: numnod, numsurf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,9)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_surf(numsurf,3)
!
      integer(kind = kint) :: isurf, i9
!
!
!$omp parallel do private(i9)
        do isurf = 1, numsurf
          i9 = ie_surf(isurf,9)
!
          x_surf(isurf,1) = xx(i9,1)
          x_surf(isurf,2) = xx(i9,2)
          x_surf(isurf,3) = xx(i9,3)
        end do
!$omp end parallel do
!
      end subroutine set_lag_surf_position
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_triangle_surf_position(numnod, numsurf, ie_surf,   &
     &          xx, x_surf)
!
      integer(kind = kint), intent(in) :: numnod, numsurf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,3)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_surf(numsurf,3)
!
      integer(kind = kint) :: isurf
      integer(kind = kint) ::  i1,  i2,  i3
!
!
!$omp parallel do private(i1,i2,i3)
        do isurf = 1, numsurf
          i1 = ie_surf(isurf,1)
          i2 = ie_surf(isurf,2)
          i3 = ie_surf(isurf,3)
!
          x_surf(isurf,1)                                               &
     &          = third * (xx(i1,1) + xx(i2,1) + xx(i3,1) )
          x_surf(isurf,2)                                               &
     &          = third * (xx(i1,2) + xx(i2,2) + xx(i3,2) )
          x_surf(isurf,3)                                               &
     &          = third * (xx(i1,3) + xx(i2,3) + xx(i3,3) )
        end do
!$omp end parallel do
!
      end subroutine set_triangle_surf_position
!
!  ---------------------------------------------------------------------
!
      end module set_surface_position
