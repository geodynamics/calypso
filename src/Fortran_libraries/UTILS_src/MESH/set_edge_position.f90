!set_edge_position.f90
!      module set_edge_position
!
!      Written by H. Matsui on May, 2006
!
!      subroutine set_linear_edge_position(numnod, numedge, ie_edge, xx,&
!     &          x_edge)
!      subroutine set_quad_edge_position(numnod, numedge, ie_edge, xx,  &
!     &          x_edge)
!
      module set_edge_position
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_edge_position(numnod, numedge, ie_edge, xx, &
     &          x_edge)
!
      integer(kind = kint), intent(in) :: numnod, numedge
      integer(kind = kint), intent(in) :: ie_edge(numedge,2)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_edge(numedge,3)
!
      integer(kind = kint) :: iedge
      integer(kind = kint) ::  i1,  i2
      real(kind = kreal), parameter :: one = 1.0d0, two = 2.0d0
      real(kind = kreal), parameter :: half = one/two
!
!
!$omp parallel do private(i1,i2)
        do iedge = 1, numedge
          i1 = ie_edge(iedge,1)
          i2 = ie_edge(iedge,2)
!
          x_edge(iedge,1) = half * (xx(i1,1) + xx(i2,1))
          x_edge(iedge,2) = half * (xx(i1,2) + xx(i2,2))
          x_edge(iedge,3) = half * (xx(i1,3) + xx(i2,3))
        end do
!$omp end parallel do
!
      end subroutine set_linear_edge_position
!
!  ---------------------------------------------------------------------
!
      subroutine set_quad_edge_position(numnod, numedge, ie_edge, xx,   &
     &          x_edge)
!
      integer(kind = kint), intent(in) :: numnod, numedge
      integer(kind = kint), intent(in) :: ie_edge(numedge,3)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_edge(numedge,3)
!
      integer(kind = kint) :: iedge, i2
!
!
!$omp parallel do private(i2)
        do iedge = 1, numedge
          i2 = ie_edge(iedge,2)
!
          x_edge(iedge,1) = xx(i2,1)
          x_edge(iedge,2) = xx(i2,2)
          x_edge(iedge,3) = xx(i2,3)
        end do
!$omp end parallel do
!
      end subroutine set_quad_edge_position
!
!  ---------------------------------------------------------------------
!
      end module set_edge_position
