!> @file  schmidt_fix_m.f90
!>      module schmidt_fix_m
!!
!! @author  H. Matsui
!! @date Programmed on Jan., 1998
!
!> @brief module for Legendre polynomials with Schmidt normalization
!!
!!      subroutine schmidt_legendres_m(ltr, m, theta, p, dp,            &
!!@n     &          pmp1, pmp1, df)
!!@n
!!@n      subroutine schmidt_legendre_m(ltr, m, theta,  p, df)
!!@n*************************************************************
!!@n*     Legendre and adjoint Legendle Polynomial for order m
!!@n*
!!@n*      p(l)  : Schmidt Polynomial
!!@n*         Normalization...
!!@n*           P_{l}^{0} = P_{l,0}
!!@n*           P_{l}^{m} = sqrt( 2(l-m)! / (l+m)! ) * P_{l,m}
!!@n*        theta :  input colatitude ( 0 =< theta <= pi )
!!@n*
!!@n*        df(l)  :   work area
!!@n*
!!@n*************************************************************
!!@n
!!@n      subroutine diff_schmidt_legendre_m(ltr, m, pmn1, pmp1, dp)
!!@n*************************************************************
!!@n*     lead difference of Schmidt Polynomial
!!@n*
!!@n*      pmp1 = p(m+1,l)  : Schmidt Polynomial (input)
!!@n*      pmn1 = p(m-1,l)  : Schmidt Polynomial (input)
!!@n*         Normalization...
!!@n*           P_{l}^{0} = P_{l,0}
!!@n*           P_{l}^{m} = sqrt( 2(l-m)! / (l+m)! ) * P_{l,m}
!!@n*      dp(l) : diffrential of Schmidt Polynomial  dp/dtheta
!!@n*
!!@n*************************************************************
!!@n
!!@n      subroutine full_normalize_from_smdt_m(ltr, m, p, dp)
!!@n
!!@n @param ltr       Truncation level for the polynomial
!!@n @param theta     Input degree ( \f$ 0 \le \theta \le \pi \f$)
!!@n @param p(l)      Schmidt Polynomial for order m \f$ P_{l}^{m} \f$
!!@n @param dp(l)     diffrence of Schmidt Polynomial for order m
!!                     \f$ d P_{l}^{m} / d\theta \f$
!!@n @param df(l)     work area
!!
      module schmidt_fix_m
!
      use m_precision
      use m_constants
!
      implicit none
!*
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine schmidt_legendres_m(ltr, m, theta, p, dp,              &
     &          pmn1, pmp1, df)
!
      integer(kind = kint), intent(in) :: m, ltr
      real(kind = kreal), intent(in) :: theta
!
      real(kind = kreal), intent(inout) :: p(0:ltr)
      real(kind = kreal), intent(inout) :: dp(0:ltr)
      real(kind = kreal), intent(inout) :: pmp1(0:ltr), pmn1(0:ltr)
      real(kind = kreal), intent(inout) :: df(0:ltr+2)
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call schmidt_legendre_m(ltr, (m-1), theta, pmn1, df)
      call schmidt_legendre_m(ltr, m,     theta, p,    df)
      call schmidt_legendre_m(ltr, (m+1), theta, pmp1, df)
!
!*   ++++++++++  lead difference of Legendre Polynomial  ++++++
!*
      call diff_schmidt_legendre_m(ltr, m, pmn1, pmp1, dp)
!
      end subroutine schmidt_legendres_m
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine schmidt_legendre_m(ltr, m, theta, p, df)
!*
      integer(kind = kint), intent(in) :: ltr, m
      real(kind = kreal), intent(in) :: theta
!
      real(kind = kreal), intent(inout) :: p(0:ltr)
      real(kind = kreal), intent(inout) :: df(0:ltr+2)
!
      integer(kind = kint) :: l, k, m1
!
!
      if(m .lt. 0) then
        p(0:ltr) = zero
!
!* +++++++  Legendre Polynomial  ++++++++++++
!*
      else if(m .eq. 0) then
        p(0) = one
        p(1) = cos(theta)
!*
        do l = 2, ltr
          p(l) =  p(l-1) * dble(2*l-1)/dble(l) * cos(theta)             &
     &          - p(l-2) * dble(l-1)/dble(l)
        end do
!*
!* +++++++  adjoint Legendre Polynomial  ++++++++++++
!*
      else
        df(m) = one
        do k = 1, m
          df(m) =  df(m) * dble(2*k-1) / dble(2*k)
        end do
        df(m+1) = sqrt( two * df(m) * dble(2*m+1) ) * cos(theta)
        df(m  ) =   sqrt( two * df(m) )
!*
!*
        if ( m .lt. ltr-1 ) then
          do l = m+2, ltr
            df(l) = ( cos(theta) * dble(2*l-1) * df(l-1)                &
     &               - sqrt( dble( (l-1)*(l-1) - m*m )) * df(l-2) )     &
     &               / sqrt( dble( l*l - m*m ))
          end do
        end if
!*
        do l = m, ltr
          p(l) =  df(l)
          do m1 = 1, m
            p(l) =  p(l) * sin(theta)
          end do
        end do
      end if
!*
      end subroutine schmidt_legendre_m
!
!  ---------------------------------------------------------------------
!
      subroutine diff_schmidt_legendre_m(ltr, m, pmn1, pmp1, dp)
!*
      integer(kind = kint), intent(in) :: ltr, m
      real(kind = kreal), intent(in) :: pmp1(0:ltr), pmn1(0:ltr)
      real(kind = kreal), intent(inout) :: dp(0:ltr)
!
      integer(kind = kint) :: l
!
!
      if(m .eq. 0) then
        dp(0) = zero
        do l = 1, ltr
          dp(l) = - sqrt( dble(l*(l+1)/2) ) * pmp1(l)
        end do
!
      else if(m .eq. 1) then
        dp(1) = pmn1(1)
        do l = 2, ltr
          dp(l) = half * ( sqrt( dble( 2*l*(l+1) ) ) * pmn1(l)          &
     &                   - sqrt( dble((l-1)*(l+2)) ) * pmp1(l) )
        end do
!
      else
        dp(m) = half * sqrt( dble(2*m) ) * pmn1(m)
        do l = m+1, ltr
          dp(l) = half * ( sqrt( dble( (l+m)*(l-m+1) ) ) * pmn1(l)      &
     &                   - sqrt( dble( (l-m)*(l+m+1) ) ) * pmp1(l) )
        end do
      end if
!*
      end subroutine diff_schmidt_legendre_m
!
!  ---------------------------------------------------------------------
!
      subroutine full_normalize_from_smdt_m(ltr, m, p, dp)
!*
      integer(kind = kint), intent(in) :: ltr, m
      real(kind = kreal), intent(inout) :: p(0:ltr)
      real(kind = kreal), intent(inout) :: dp(0:ltr)
!
      integer(kind = kint) :: l
      real(kind = kreal) :: pi, asqrt2pi, cl
!
!
      if(m .eq. 0) then
        p(1:ltr) =  sqrt(2.0d0) * p(1:ltr)
        dp(1:ltr) = sqrt(2.0d0) * dp(1:ltr)
!
      else
        pi = 4.0d0*atan(1.0d0)
        asqrt2pi = 1.0d0 / sqrt(2.0d0*pi)
        do l = m, ltr
          cl = sqrt(dble(2*l+1)) / 2.0d0
          p(l) =  (-1.0d0)**m * (asqrt2pi*cl) * p(l)
          dp(l) = (-1.0d0)**m * (asqrt2pi*cl) * dp(l)
        end do
      end if
!
      end subroutine full_normalize_from_smdt_m
!
!  ---------------------------------------------------------------------
!
      end module schmidt_fix_m
