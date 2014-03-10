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
!!      subroutine dlad
!!      subroutine dschmidt
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
      integer(kind = kint) :: nth = 10
      real(kind = kreal) :: dth
!
      real(kind = kreal), allocatable :: p(:,:)
      real(kind = kreal), allocatable :: dp(:,:)
!
      real(kind = kreal), allocatable :: dplm(:,:)
      real(kind = kreal), allocatable :: dc(:,:)
      real(kind = kreal), allocatable :: df(:,:)
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
        allocate ( dc(0:nth,0:nth+2) )
        allocate ( df(0:nth+2,0:nth+2) )
!
        p = 0.0d0
        dp = 0.0d0
        dplm = 0.0d0
        dc = 0.0d0
        df = 0.0d0
!
       end subroutine allocate_schmidt_polynomial
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_schmidt_polynomial
!
        deallocate ( p, dp, dplm, dc, df )
!
       end subroutine deallocate_schmidt_polynomial
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dlad
!
      use legendre
!
      real(kind = kreal) :: x
!
!*  ++++++++  set x ++++++++++++++++
!*
      x = cos(dth)
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call dladendre(nth, x, dplm, df)
!
      end subroutine dlad
!
! -----------------------------------------------------------------------
!
      subroutine dschmidt
!
      use schmidt
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call schmidt_polynomial(nth, dth, p, df)
!
!*   ++++++++++  lead difference of Legendre Polynomial  ++++++
!*
      call diff_schmidt_polynomial(nth, p, dp)
!
      end subroutine dschmidt
!
! -----------------------------------------------------------------------
!
      subroutine full_norm_legendre
!
      use schmidt
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call schmidt_polynomial(nth, dth, p, df)
!
!*   ++++++++++  lead difference of Legendre Polynomial  ++++++
!*
      call diff_schmidt_polynomial(nth, p, dp)
!
!*   ++++++++++  Full normalizationl  ++++++
!*
      call full_normalize_by_smdt(nth, p, dp)
!
      end subroutine full_norm_legendre
!
! -----------------------------------------------------------------------
!
      end module m_schmidt_polynomial
