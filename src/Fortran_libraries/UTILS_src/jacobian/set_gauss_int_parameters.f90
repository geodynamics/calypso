!
!      module set_gauss_int_parameters
!
!       Written by H. Matsui on March. 2006
!
!!      subroutine set_gauss_coefs_4_3d                                 &
!!     &        (max_int_point, maxtot_int_1d, int_start1, xi1, owe,    &
!!     &         maxtot_int_3d, int_start3, l_int, xi3, ei3, zi3, owe3d)
!!      subroutine set_gauss_coefs_4_2d                                 &
!!     &        (max_int_point, maxtot_int_1d, int_start1, xi1, owe,    &
!!     &         maxtot_int_2d, int_start2, l_int2d, xi2, ei2, owe2d)
!!      subroutine set_gauss_coefs_4_1d                                 &
!!     &         (max_int_point, maxtot_int_1d, int_start1, xi1, owe)
!
      module set_gauss_int_parameters
!
      use m_precision
!
      implicit none
!
      private :: set_gauss_coefs_1d_n
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_4_3d                                   &
     &        (max_int_point, maxtot_int_1d, int_start1, xi1, owe,      &
     &         maxtot_int_3d, int_start3, l_int, xi3, ei3, zi3, owe3d)
!
      integer(kind = kint), intent(in) :: maxtot_int_1d
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
!
      integer(kind = kint), intent(in)                                  &
     &                     :: l_int(3,maxtot_int_3d,max_int_point)
      real(kind = kreal), intent(in) :: xi1(maxtot_int_1d)
      real(kind = kreal), intent(in) :: owe(maxtot_int_1d)
!
      real(kind = kreal), intent(inout) :: xi3(maxtot_int_3d)
      real(kind = kreal), intent(inout) :: ei3(maxtot_int_3d)
      real(kind = kreal), intent(inout) :: zi3(maxtot_int_3d)
      real(kind = kreal), intent(inout) :: owe3d(maxtot_int_3d)
!
      integer(kind = kint) :: n, ix, ii, i1, i2, i3
!
!
      do n = 1, max_int_point
        do ii = 1, n*n*n
          ix = ii + int_start3(n)
          i1 = l_int(1,ii,n) + int_start1(n)
          i2 = l_int(2,ii,n) + int_start1(n)
          i3 = l_int(3,ii,n) + int_start1(n)
          xi3(ix) = xi1(i1)
          ei3(ix) = xi1(i2)
          zi3(ix) = xi1(i3)
          owe3d(ix) = owe(i1) * owe(i2) * owe(i3)
        end do
      end do
!
      end subroutine set_gauss_coefs_4_3d
!
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_4_2d                                   &
     &        (max_int_point, maxtot_int_1d, int_start1, xi1, owe,      &
     &         maxtot_int_2d, int_start2, l_int2d, xi2, ei2, owe2d)
!
      integer(kind = kint), intent(in) :: maxtot_int_1d
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
!
      integer(kind = kint), intent(in)                                  &
     &                     :: l_int2d(2,maxtot_int_2d,max_int_point)
      real(kind= kreal), intent(in) :: xi1(maxtot_int_1d)
      real(kind = kreal), intent(in) :: owe(maxtot_int_1d)
!
      real(kind = kreal), intent(inout) :: xi2(maxtot_int_2d)
      real(kind = kreal), intent(inout) :: ei2(maxtot_int_2d)
      real(kind = kreal), intent(inout) :: owe2d(maxtot_int_2d)
!
      integer(kind = kint) :: n, ii, ix, i1, i2
!
      do n = 1, max_int_point
        do ii = 1, n*n
          ix = ii + int_start2(n)
          i1 = l_int2d(1,ii,n) + int_start1(n)
          i2 = l_int2d(2,ii,n) + int_start1(n)
          xi2(ix) = xi1(i1)
          ei2(ix) = xi1(i2)
          owe2d(ix) = owe(i1) * owe(i2)
        end do
      end do
!
      end subroutine set_gauss_coefs_4_2d
!
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_4_1d                                   &
     &         (max_int_point, maxtot_int_1d, int_start1, xi1, owe)
!
      use m_constants
      use m_gauss_int_parameters
      use t_gauss_points
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_1d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
!
      real(kind = kreal), intent(inout) :: xi1(maxtot_int_1d)
      real(kind = kreal), intent(inout) :: owe(maxtot_int_1d)
!
      integer(kind = kint) :: n
      type(gauss_points) :: gauss_1d
!
!
      if (max_int_point .ge. ione) then
        call set_gauss_coefs_1d_n(ione,   pt1d_1g, wt1d_1g,             &
     &      max_int_point, maxtot_int_1d, int_start1, xi1, owe)
      end if
!
      if (max_int_point .ge. itwo) then
        call set_gauss_coefs_1d_n(itwo,   pt1d_2g, wt1d_2g,             &
     &      max_int_point, maxtot_int_1d, int_start1, xi1, owe)
      end if
!
      if (max_int_point .ge. ithree) then
        call set_gauss_coefs_1d_n(ithree, pt1d_3g, wt1d_3g,             &
     &      max_int_point, maxtot_int_1d, int_start1, xi1, owe)
      end if
!
      if (max_int_point .ge. ifour) then
        call set_gauss_coefs_1d_n(ifour,  pt1d_4g, wt1d_4g,             &
     &      max_int_point, maxtot_int_1d, int_start1, xi1, owe)
      end if
!
!
!
      if (max_int_point .ge. 5) then
        do n = 5, max_int_point
          call construct_gauss_coefs(n, gauss_1d)
!
          call set_gauss_coefs_1d_n(n, gauss_1d%point, gauss_1d%weight, &
     &        max_int_point, maxtot_int_1d, int_start1, xi1, owe)
!
          call dealloc_gauss_points(gauss_1d)
        end do
      end if
!
      end subroutine set_gauss_coefs_4_1d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_1d_n(n, pt1d, wt1d,                    &
     &          max_int_point, maxtot_int_1d, int_start1, xi1, owe)
!
      integer(kind = kint), intent(in) :: n
      real(kind = kreal), intent(in) :: pt1d(n), wt1d(n)
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_1d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
!
      real(kind = kreal), intent(inout) :: xi1(maxtot_int_1d)
      real(kind = kreal), intent(inout) :: owe(maxtot_int_1d)
!
      integer(kind = kint) :: ix, ii
!
!
      do ii = 1, n
        ix = ii + int_start1(n)
        xi1(ix) = pt1d(ii)
        owe(ix) = wt1d(ii)
      end do
!
      end subroutine set_gauss_coefs_1d_n
!
!  ---------------------------------------------------------------------
!
      end module set_gauss_int_parameters
