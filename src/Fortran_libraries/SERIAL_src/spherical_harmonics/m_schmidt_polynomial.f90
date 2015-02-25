!>@file   m_schmidt_polynomial.f90
!!@brief  module m_schmidt_polynomial
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in 2009
!
!> @brief Data array for Legendre polyonomials with
!!        Schmidt normalization
!!
!!@verbatim
!!      subroutine allocate_schmidt_polynomial
!!      subroutine deallocate_schmidt_polynomial
!!
!!      subroutine dlad(theta)
!!      subroutine dschmidt(theta)
!!
!!      subroutine full_norm_legendre(theta)
!!@endverbatim
!
      module m_schmidt_polynomial
!
      use m_precision
!
      implicit  none
! 
!
!
!>      Truncation level
      integer(kind = kint) :: nth = 10
!
!>      Schmidt quasi-normalized Legendre polynomials
      real(kind = kreal), allocatable :: p(:,:)
!>      diffrence of Schmidt quasi-normalized Legendre polynomials
      real(kind = kreal), allocatable :: dp(:,:)
!
!>      Legendre polynomials without normalization
      real(kind = kreal), allocatable :: dplm(:,:)
!
!>      work area
      real(kind = kreal), allocatable :: df(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_schmidt_polynomial
!
        allocate ( p(0:nth,0:nth) )
        allocate ( dp(0:nth,0:nth) )
        allocate ( dplm(0:nth+2,0:nth+2) )
        allocate ( df(0:nth+2) )
!
        p = 0.0d0
        dp = 0.0d0
        dplm = 0.0d0
        df = 0.0d0
!
       end subroutine allocate_schmidt_polynomial
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_schmidt_polynomial
!
        deallocate (p, dp, dplm, df)
!
       end subroutine deallocate_schmidt_polynomial
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dlad(theta)
!
      use legendre
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
      call dledendre(nth, x, dplm, df)
!
      end subroutine dlad
!
! -----------------------------------------------------------------------
!
      subroutine dschmidt(theta)
!
      use schmidt
!
      real(kind = kreal), intent(in) :: theta
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call schmidt_legendre(nth, theta, p, df)
!
!*   ++++++++++  lead difference of Legendre Polynomial  ++++++
!*
      call diff_schmidt_legendre(nth, p, dp)
!
      end subroutine dschmidt
!
! -----------------------------------------------------------------------
!
      subroutine full_norm_legendre(theta)
!
      use schmidt
!
      real(kind = kreal), intent(in) :: theta
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call dschmidt(theta)
!
!*   ++++++++++  Full normalizationl  ++++++
!*
      call full_normalize_from_smdt(nth, p, dp)
!
      end subroutine full_norm_legendre
!
! -----------------------------------------------------------------------
!
      end module m_schmidt_polynomial
