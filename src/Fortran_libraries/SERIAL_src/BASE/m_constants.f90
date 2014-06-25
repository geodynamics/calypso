!> @file m_constants.f90
!!      module m_constants
!!
!! @author H. Matsui
!! @date Written on Oct., 2009
!!
!> @brief Constants for Fortran sources
!
      module m_constants
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), parameter :: zero =  0.0d0
      real(kind = kreal), parameter :: one =   1.0d0
      real(kind = kreal), parameter :: two =   2.0d0
      real(kind = kreal), parameter :: three = 3.0d0
      real(kind = kreal), parameter :: four =  4.0d0
      real(kind = kreal), parameter :: five =  5.0d0
      real(kind = kreal), parameter :: six =   6.0d0
      real(kind = kreal), parameter :: seven = 7.0d0
      real(kind = kreal), parameter :: eight = 8.0d0
      real(kind = kreal), parameter :: dnine = 9.0d0
      real(kind = kreal), parameter :: ten =  10.0d0
      real(kind = kreal), parameter :: eleven = 11.0d0
      real(kind = kreal), parameter :: twenty = 20.0d0
      real(kind = kreal), parameter :: thirty = 30.0d0
!
      real(kind = kreal), parameter :: half =  one /  two
      real(kind = kreal), parameter :: third = one / three
      real(kind = kreal), parameter :: quad =  one / four
      real(kind = kreal), parameter :: r125 =  one / eight
      real(kind = kreal), parameter :: deci =  one / ten
!
      real(kind = kreal), parameter :: dminus =  -one
!
      integer(kind = kint), parameter :: izero =   0
      integer(kind = kint), parameter :: ione =    1
      integer(kind = kint), parameter :: itwo =    2
      integer(kind = kint), parameter :: ithree =  3
      integer(kind = kint), parameter :: ifour =   4
      integer(kind = kint), parameter :: ifive =   5
      integer(kind = kint), parameter :: isix =    6
      integer(kind = kint), parameter :: iseven =  7
      integer(kind = kint), parameter :: ieight =  8
      integer(kind = kint), parameter :: inine =   9
      integer(kind = kint), parameter :: iten =    10
!
      integer(kind = kint), parameter :: iminus =  -1
!
      real, parameter :: rzero =  0.0e0
      real, parameter :: rone =   1.0e0
      real, parameter :: rtwo =   2.0e0
      real, parameter :: rhalf =  0.5e0
!
      end module m_constants
