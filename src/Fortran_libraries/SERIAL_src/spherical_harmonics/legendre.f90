!>@file   legendre.f90
!!@brief  module legendre
!!
!!@author H. Matsui
!!@date Programmed in 1995
!!@n    Modified in June, 2006
!
!>@brief module for Legendre polynomials
!!
!!@verbatim
!!      subroutine dledendre(ltr, x, dplm, df)
!!      subroutine diff_legendre(ltr, dplm, p, dp)
!!*************************************************************
!!     lead legendre and adjoint Legendle Polynomial
!!
!!      plm(m,l) : adjoint Legendre Polynomial P_l^m (x)
!!         x        :  input value x ( -1 =< x =<1 )
!!        df(m,l)  :   work area
!!
!!*************************************************************
!!@endverbatim
!!
!!@n @param ltr       Truncation level for the polynomial
!!@n @param x         Input value  ( -1 =< x =<1 )
!!@n @param plm(m,l) adjoint Legendre Polynomial P_l^m (x)
!!@n @param p(m,l)    Schmidt Polynomial
!!@n @param dp(m,l)   diffrence of Schmidt Polynomial  dp/dtheta
!!@n @param df(m)     work area
!
      module legendre
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dledendre(ltr, x, plm, df)
!*
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: x
!
      real(kind = kreal), intent(inout) :: plm(0:ltr+2,0:ltr+2)
      real(kind = kreal), intent(inout) :: df(0:ltr+2)
!
      integer(kind = kint) :: l, m, mm, n
!
!* +++++++  Legendre Polynomial  ++++++++++++
!*
      plm(0,0) = 1.0d0
      plm(0,1) = x
!*
      do 10 l = 2 ,ltr+1
!*
        plm(0,l) = x * plm(0,l-1) * dble(2*l-1)/dble(l)                 &
     &               - plm(0,l-2) * dble(l-1)/dble(l)
!*
  10  continue
!*
!* +++++++  adjoint Legendre Polynomial  ++++++++++++
!*
      do 20 m = 1 ,ltr+1
!*
        df(m) = 1.0
        df(m+1) = x 
        do 30 n = 1 ,2*m-1 ,2
          df(m) = df(m)*dble(n)
          df(m+1) = df(m+1)*dble(n)
  30    continue
        df(m+1) = df(m+1)*dble(2*m+1)
!*
!*
        if ( m .lt. ltr-1 ) then
          do 40 mm = m+2 ,ltr
            df(mm) = x * df(mm-1) * dble(2*mm-1)/dble(mm-m)             &
     &                 - df(mm-2) * dble(mm+m-1)/dble(mm-m)
  40      continue
        endif
!*
        do 50 l = m ,ltr
!            write(*,*) 'l,m,df', l,m,df(l)
          plm(m,l) = ( abs(1-x**2) )**(dble(m)/2) * df(l)
  50    continue
!*
  20  continue
!*
!*
      return
      end subroutine dledendre
!
!  ---------------------------------------------------------------------
!
      subroutine diff_legendre(ltr, plm, dp)
!*
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: plm(0:ltr+2,0:ltr+2)
!
      real(kind = kreal), intent(inout) :: dp(0:ltr,0:ltr)
!
      integer(kind = kint) :: l, m
!
!*   ++++++++++  lead difference of the Polynomial  ++++++
!*
        dp(0,0) = 0.0d0
        do 20 l = 1 ,ltr
          do 21 m = 1 ,l-1
!*
            dp(m,l) = ( dble(l+m) * dble(l-m+1) * plm(m-1,l)            &
     &                 - plm(m+1,l) ) / 2.0d0
!*
  21      continue
          dp(0,l) = - plm(1,l)
          dp(l,l) = dble(l) * plm(l-1,l)
!*
  20    continue
!*
      return
      end subroutine diff_legendre
!
!  ---------------------------------------------------------------------
!
      end module legendre
