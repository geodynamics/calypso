!> @file  schmidt.f90
!>      module schmidt
!!
!! @author  H. Matsui
!! @date Programmed on Jan., 1998
!
!> @brief module for Legendre polynomials with Schmidt normalization
!!
!!@n      subroutine schmidt_polynomial(nth, theta,  p, df)
!!@n*************************************************************
!!@n*     lead legendre and adjoint Legendle Polynomial
!!@n*
!!@n*      p(m,l)  : Schmidt Polynomial
!!@n*         Normalization...
!!@n*           P_{l}^{0} = P_{l,0}
!!@n*           P_{l}^{m} = sqrt( 2(l-m)! / (l+m)! ) * P_{l,m}
!!@n*        dth      :  input degree theta ( 0 =< dth <= pi )
!!@n*
!!@n*        df(m,l)  :   work area
!!@n*
!!@n*************************************************************
!!@n
!!@n      subroutine diff_schmidt_polynomial(nth, p, dp)
!!@n*************************************************************
!!@n*     lead difference of Schmidt Polynomial
!!@n*
!!@n*      p(m,l)  : Schmidt Polynomial (input)
!!@n*         Normalization...
!!@n*           P_{l}^{0} = P_{l,0}
!!@n*           P_{l}^{m} = sqrt( 2(l-m)! / (l+m)! ) * P_{l,m}
!!@n*      dp(m,l) : diffrential of Schmidt Polynomial  dp/dtheta
!!@n*
!!@n*************************************************************
!!@n
!!@n      subroutine full_normalize_by_smdt(nth, p, dp)
!!@n
!!@n @param nth       Truncation level for the polynomial
!!@n @param theta     Input degree ( \f$ 0 \le \theta \le \pi \f$)
!!@n @param p(m,l)    Schmidt Polynomial  \f$ P_{l}^{m} \f$
!!@n @param dp(m,l)   diffrence of Schmidt Polynomial
!!                     \f$ d P_{l}^{m} / d\theta \f$
!!@n @param df(m)     work area
!!
      module schmidt
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
      subroutine schmidt_polynomial(nth, theta,  p, df)
!*
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(in) :: theta
!
      real(kind = kreal), intent(inout) :: p(0:nth,0:nth)
      real(kind = kreal), intent(inout) :: df(0:nth+2)
!
      integer(kind = kint) :: l, m, k, m1
!
!
!* +++++++  Legendre Polynomial  ++++++++++++
!*
      p(0,0) = one
      p(0,1) = cos(theta)
!*
      do l = 2, nth
        p(0,l) =  p(0,l-1) * dble(2*l-1)/dble(l) * cos(theta)           &
     &          - p(0,l-2) * dble(l-1)/dble(l)
      end do
!*
!* +++++++  adjoint Legendre Polynomial  ++++++++++++
!*
!*
      do m = 1, nth
!*
        df(m) = one
        do k = 1, m
          df(m) =  df(m) * dble(2*k-1) / dble(2*k)
        end do
        df(m+1) = sqrt( two * df(m) * dble(2*m+1) ) * cos(theta)
        df(m  ) =   sqrt( two * df(m) )
!*
!*
        if ( m .lt. nth-1 ) then
          do l = m+2, nth
            df(l) = ( cos(theta) * dble(2*l-1) * df(l-1)                &
     &               - sqrt( dble( (l-1)*(l-1) - m*m )) * df(l-2) )     &
     &               / sqrt( dble( l*l - m*m ))
          end do
        end if
!*
        do l = m, nth
          p(m,l) =  df(l)
          do m1 = 1, m
            p(m,l) =  p(m,l) * sin(theta)
          end do
        end do
!*
      end do
!*
      end subroutine schmidt_polynomial
!
!  ---------------------------------------------------------------------
!
      subroutine diff_schmidt_polynomial(nth, p, dp)
!*
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(in) :: p(0:nth,0:nth)
      real(kind = kreal), intent(inout) :: dp(0:nth,0:nth)
!
      integer(kind = kint) :: l, m
!
!
      dp(0,0) = zero
      do l = 1, nth
        dp(0,l) = - sqrt( dble(l*(l+1)/2) ) * p(1,l)
      end do
      dp(1,1) = p(0,1)
!
      if (nth .lt. 2) return
!
      do l = 2, nth
        dp(1,l) = half * ( sqrt( dble( 2*l*(l+1) ) ) * p(0,l)           &
     &                   - sqrt( dble((l-1)*(l+2)) ) * p(2,l) )
      end do
!
      do l = 2, nth
        dp(l,l) = half * sqrt( dble(2*l) ) * p(l-1,l)
      end do
!
      if (nth .lt. 3) return
!
      do l = 3, nth
        do m = 2 ,l-1
          dp(m,l) = half * ( sqrt( dble( (l+m)*(l-m+1) ) ) * p(m-1,l)   &
     &                     - sqrt( dble( (l-m)*(l+m+1) ) ) * p(m+1,l) )
        end do
      end do
!*
      end subroutine diff_schmidt_polynomial
!
!  ---------------------------------------------------------------------
!
      subroutine full_normalize_by_smdt(nth, p, dp)
!*
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(inout) :: p(0:nth,0:nth)
      real(kind = kreal), intent(inout) :: dp(0:nth,0:nth)
!
      integer(kind = kint) :: l, m
      real(kind = kreal) :: pi, asqrt2pi, cl
!
!
      pi = 4.0d0*atan(1.0d0)
      asqrt2pi = 1.0d0 / sqrt(2.0d0*pi)
!
!
      do l = 0, nth
        p(0,l) =  sqrt(2.0d0) * p(0,l)
        dp(0,l) = sqrt(2.0d0) * dp(0,l)
      end do
!
      do l = 0, nth
        cl = sqrt(dble(2*l+1)) / 2.0d0
        do m = 0 ,l
          p(m,l) =  (-1.0d0)**m * (asqrt2pi*cl) * p(m,l)
          dp(m,l) = (-1.0d0)**m * (asqrt2pi*cl) * dp(m,l)
        end do
      end do
!
      end subroutine full_normalize_by_smdt
!
!  ---------------------------------------------------------------------
!
      end module schmidt
