!>@file   t_schmidt_polynomial.f90
!!@brief  module t_schmidt_polynomial
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in 2009
!
!> @brief Data array for Legendre polyonomials with
!!        Schmidt normalization
!!
!!@verbatim
!!      subroutine alloc_schmidt_polynomial(ltr, leg)
!!       subroutine dealloc_schmidt_polynomial(leg)
!!
!!      subroutine dlad(theta, leg)
!!      subroutine dschmidt(theta, leg)
!!
!!      subroutine full_norm_legendre(theta, leg)
!!@endverbatim
!
      module t_schmidt_polynomial
!
      use m_precision
!
      implicit  none
! 
!
!>      Structure for LEgendre polynomial for single point
      type legendre_polynomials
!>        Truncation level
        integer(kind = kint) :: nth = 10
!
!>        Schmidt quasi-normalized Legendre polynomials
        real(kind = kreal), allocatable :: p(:,:)
!>        diffrence of Schmidt quasi-normalized Legendre polynomials
        real(kind = kreal), allocatable :: dp(:,:)
!
!>        Legendre polynomials without normalization
        real(kind = kreal), allocatable :: dplm(:,:)
!
!>        work area
        real(kind = kreal), allocatable :: df(:)
      end type legendre_polynomials
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_schmidt_polynomial(ltr, leg)
!
      integer(kind = kint), intent(in) :: ltr
      type(legendre_polynomials), intent(inout) :: leg
!
!
      if(ltr .lt. 0) leg%nth = ltr
!
      allocate ( leg%p(0:leg%nth,0:leg%nth) )
      allocate ( leg%dp(0:leg%nth,0:leg%nth) )
      allocate ( leg%dplm(0:leg%nth+2,0:leg%nth+2) )
      allocate ( leg%df(0:leg%nth+2) )
!
      leg%p = 0.0d0
      leg%dp = 0.0d0
      leg%dplm = 0.0d0
      leg%df = 0.0d0
!
      end subroutine alloc_schmidt_polynomial
!
! -----------------------------------------------------------------------
!
       subroutine dealloc_schmidt_polynomial(leg)
!
      type(legendre_polynomials), intent(inout) :: leg
!
        deallocate (leg%p, leg%dp, leg%dplm, leg%df)
!
       end subroutine dealloc_schmidt_polynomial
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dlad(theta, leg)
!
      use legendre
!
      type(legendre_polynomials), intent(inout) :: leg
!
      real(kind = kreal), intent(in) :: theta
      real(kind = kreal) :: x
!
!*  ++++++++  set x ++++++++++++++++
!*
      x = cos(theta)
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call dledendre(leg%nth, x, leg%dplm, leg%df)
!
      end subroutine dlad
!
! -----------------------------------------------------------------------
!
      subroutine dschmidt(theta, leg)
!
      use schmidt
!
!
      real(kind = kreal), intent(in) :: theta
      type(legendre_polynomials), intent(inout) :: leg
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call schmidt_legendre(leg%nth, theta, leg%p, leg%df)
!
!*   ++++++++++  lead difference of Legendre Polynomial  ++++++
!*
      call diff_schmidt_legendre(leg%nth, leg%p, leg%dp)
!
      end subroutine dschmidt
!
! -----------------------------------------------------------------------
!
      subroutine full_norm_legendre(theta, leg)
!
      use schmidt
!
      real(kind = kreal), intent(in) :: theta
      type(legendre_polynomials), intent(inout) :: leg
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call dschmidt(theta, leg)
!
!*   ++++++++++  Full normalizationl  ++++++
!*
      call full_normalize_from_smdt(leg%nth, leg%p, leg%dp)
!
      end subroutine full_norm_legendre
!
! -----------------------------------------------------------------------
!
      end module t_schmidt_polynomial
