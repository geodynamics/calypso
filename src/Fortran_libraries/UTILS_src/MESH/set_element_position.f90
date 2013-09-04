!set_element_position.f90
!      module set_element_position
!
!      Written by H. Matsui on May, 2006
!
!      subroutine set_linear_ele_position(numnod, numele, ie, xx, x_ele)
!      subroutine set_quad_ele_position(numnod, numele, ie, xx, x_ele)
!      subroutine set_lag_ele_position(numnod, numele, ie, xx, x_ele)
!
      module set_element_position
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
      subroutine set_linear_ele_position(numnod, numele, ie, xx, x_ele)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele,8)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_ele(numele,3)
!
      integer(kind = kint) :: iele
      integer(kind = kint) ::  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      real(kind = kreal), parameter :: one = 1.0d0, octa = 8.0d0
      real(kind = kreal), parameter :: r125 = one/octa
!
!
!$omp parallel do private(i1,i2,i3,i4,i5,i6,i7,i8)
        do iele = 1, numele
          i1 =  ie(iele, 1)
          i2 =  ie(iele, 2)
          i3 =  ie(iele, 3)
          i4 =  ie(iele, 4)
          i5 =  ie(iele, 5)
          i6 =  ie(iele, 6)
          i7 =  ie(iele, 7)
          i8 =  ie(iele, 8)
!
          x_ele(iele,1)                                                 &
     &      = r125 * (xx( i1,1) + xx( i2,1) + xx( i3,1) + xx( i4,1)     &
     &              + xx( i5,1) + xx( i6,1) + xx( i7,1) + xx( i8,1) )
          x_ele(iele,2)                                                 &
     &      = r125 * (xx( i1,2) + xx( i2,2) + xx( i3,2) + xx( i4,2)     &
     &              + xx( i5,2) + xx( i6,2) + xx( i7,2) + xx( i8,2) )
          x_ele(iele,3)                                                 &
     &      = r125 * (xx( i1,3) + xx( i2,3) + xx( i3,3) + xx( i4,3)     &
     &              + xx( i5,3) + xx( i6,3) + xx( i7,3) + xx( i8,3) )
        end do
!$omp end parallel do
!
      end subroutine set_linear_ele_position
!
!  ---------------------------------------------------------------------
!
      subroutine set_quad_ele_position(numnod, numele, ie, xx, x_ele)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele,20)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_ele(numele,3)
!
      integer(kind = kint) :: iele
      integer(kind = kint) ::  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer(kind = kint) ::  i9, i10, i11, i12, i13, i14, i15, i16
      integer(kind = kint) :: i17, i18, i19, i20
      real(kind = kreal), parameter :: one = 1.0d0, four = 4.0d0
      real(kind = kreal), parameter :: quata = one/four
!
!
!$omp parallel do private(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,          &
!$omp&                    i11,i12,i13,i14,i15,i16,i17,i18,i19,i20)
        do iele = 1, numele
          i1 =  ie(iele, 1)
          i2 =  ie(iele, 2)
          i3 =  ie(iele, 3)
          i4 =  ie(iele, 4)
          i5 =  ie(iele, 5)
          i6 =  ie(iele, 6)
          i7 =  ie(iele, 7)
          i8 =  ie(iele, 8)
          i9 =  ie(iele, 9)
          i10 = ie(iele,10)
          i11 = ie(iele,11)
          i12 = ie(iele,12)
          i13 = ie(iele,13)
          i14 = ie(iele,14)
          i15 = ie(iele,15)
          i16 = ie(iele,16)
          i17 = ie(iele,17)
          i18 = ie(iele,18)
          i19 = ie(iele,19)
          i20 = ie(iele,20)
!
          x_ele(iele,1)                                                 &
     &      = - quata * (xx( i1,1) + xx( i2,1) + xx( i3,1) + xx( i4,1)  &
     &                 + xx( i5,1) + xx( i6,1) + xx( i7,1) + xx( i8,1)) &
     &        + quata * (xx( i9,1) + xx(i10,1) + xx(i11,1) + xx(i12,1)  &
     &                 + xx(i13,1) + xx(i14,1) + xx(i15,1) + xx(i16,1)  &
     &                 + xx(i17,1) + xx(i18,1) + xx(i19,1) + xx(i20,1))
          x_ele(iele,2)                                                 &
     &      = - quata * (xx( i1,2) + xx( i2,2) + xx( i3,2) + xx( i4,2)  &
     &                 + xx( i5,2) + xx( i6,2) + xx( i7,2) + xx( i8,2)) &
     &        + quata * (xx( i9,2) + xx(i10,2) + xx(i11,2) + xx(i12,2)  &
     &                 + xx(i13,2) + xx(i14,2) + xx(i15,2) + xx(i16,2)  &
     &                 + xx(i17,2) + xx(i18,2) + xx(i19,2) + xx(i20,2))
          x_ele(iele,3)                                                 &
     &      = - quata * (xx( i1,3) + xx( i2,3) + xx( i3,3) + xx( i4,3)  &
     &                 + xx( i5,3) + xx( i6,3) + xx( i7,3) + xx( i8,3)) &
     &        + quata * (xx( i9,3) + xx(i10,3) + xx(i11,3) + xx(i12,3)  &
     &                 + xx(i13,3) + xx(i14,3) + xx(i15,3) + xx(i16,3)  &
     &                 + xx(i17,3) + xx(i18,3) + xx(i19,3) + xx(i20,3))
        end do
!$omp end parallel do
!
      end subroutine set_quad_ele_position
!
!  ---------------------------------------------------------------------
!
      subroutine set_lag_ele_position(numnod, numele, ie, xx, x_ele)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele,27)
      real(kind=kreal), intent(in) :: xx(numnod,3)
!
      real(kind=kreal), intent(inout) :: x_ele(numele,3)
!
      integer(kind = kint) :: iele, i27
!
!
!$omp parallel do private(i27)
        do iele = 1, numele
          i27 = ie(iele,27)
!
          x_ele(iele,1) = xx(i27,1)
          x_ele(iele,2) = xx(i27,2)
          x_ele(iele,3) = xx(i27,3)
        end do
!$omp end parallel do
!
      end subroutine set_lag_ele_position
!
!  ---------------------------------------------------------------------
!
      end module set_element_position
