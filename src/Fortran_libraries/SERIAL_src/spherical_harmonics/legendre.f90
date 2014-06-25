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
!!    subroutine dladendre(nth, x, dplm, df)
!!*************************************************************
!!     lead legendre and adjoint Legendle Polynomial
!!
!!      dplm(m,l) : adjoint Legendre Polynomial P_l^m (x)
!!         x        :  input value x ( -1 =< x =<1 )
!!        df(m,l)  :   work area
!!
!!*************************************************************
!!
!!      subroutine schmidt_normalization(nth, dplm, dc, p, dp)
!!*************************************************************
!!     lead Schmidt quasi-normalization
!!
!!      p(m,l)  : Schmidt Polynomial
!!      dp(m,l) : diffrence of Schmidt Polynomial dp/dtheta
!!      dplm(m,l)  : adjoint Legendre Polynomial P_l^m (x)
!!      dc(m,l)  :   work area
!!
!!*************************************************************
!!@endverbatim
!!
!!@n @param nth       Truncation level for the polynomial
!!@n @param x         Input value  ( -1 =< x =<1 )
!!@n @param dplm(m,l) adjoint Legendre Polynomial P_l^m (x)
!!@n @param p(m,l)    Schmidt Polynomial
!!@n @param dp(m,l)   diffrence of Schmidt Polynomial  dp/dtheta
!!@n @param df(m)     work area
!!@n @param dc(m,l)   work area
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
      subroutine dladendre(nth, x, dplm, df)
!*
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(in) :: x
!
      real(kind = kreal), intent(inout) :: dplm(0:nth+2,0:nth+2)
      real(kind = kreal), intent(inout) :: df(0:nth+2)
!
      integer(kind = kint) :: l, m, mm, n
!
!* +++++++  Legendre Polynomial  ++++++++++++
!*
      dplm(0,0) = 1.0d0
      dplm(0,1) = x
!*
      do 10 l = 2 ,nth+1
!*
        dplm(0,l) = x * dplm(0,l-1) * dble(2*l-1)/dble(l)               &
     &             - dplm(0,l-2) * dble(l-1)/dble(l)
!*
  10  continue
!*
!* +++++++  adjoint Legendre Polynomial  ++++++++++++
!*
      do 20 m = 1 ,nth+1
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
        if ( m .lt. nth-1 ) then
          do 40 mm = m+2 ,nth
            df(mm) = x * df(mm-1) * dble(2*mm-1)/dble(mm-m)         &
     &                - df(mm-2) * dble(mm+m-1)/dble(mm-m)
  40      continue
        endif
!*
        do 50 l = m ,nth
!            write(*,*) 'l,m,df', l,m,df(l)
          dplm(m,l) = ( abs(1-x**2) )**(dble(m)/2) * df(l)
  50    continue
!*
  20  continue
!*
!*
      return
      end subroutine dladendre
!
!  ---------------------------------------------------------------------
!
      subroutine schmidt_normalization(nth, dplm, dc, p, dp)
!*
      use factorials
!
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(in) :: dplm(0:nth+2,0:nth+2)
!
      real(kind = kreal), intent(inout) :: dc(0:nth,0:nth+2)
      real(kind = kreal), intent(inout) :: p(0:nth,0:nth)
      real(kind = kreal), intent(inout) :: dp(0:nth,0:nth)
!
      integer(kind = kint) :: l, m
!
!*  +++++++   set normalized cpnstant ++++++++++++
!*
      do 10 l = 0 ,nth
        dc(0,l) = 1.0
        do 11 m = 1 ,l
          dc(m,l) = ( 2.0d0 / factorial(l-m,l+m,1) )**0.5d0
  11    continue
  10  continue
!*
!*   ++++++++++  lead difference of the Polynomial  ++++++
!*
        dp(0,0) = 0.0d0
        do 20 l = 1 ,nth
          do 21 m = 1 ,l-1
!*
            dp(m,l) = ( dble(l+m) * dble(l-m+1) * dplm(m-1,l)           &
     &                 - dplm(m+1,l) ) / 2.0d0
!*
  21      continue
          dp(0,l) = - dplm(1,l)
          dp(l,l) = dble(l) * dplm(l-1,l)
!*
  20    continue
!*
!*   ++++++++++ normalize  +++++++++++
!*
        do 31 l = 0 ,nth
          do 32 m = 0 ,l
!*
            p(m,l) =  dplm(m,l) * dc(m,l)
            dp(m,l) = dp(m,l) * dc(m,l)
!            write(*,*) 'l,m',l,m, p(m,l),dp(m,l)
!*
  32      continue
  31    continue
!*
      return
      end subroutine schmidt_normalization
!
!  ---------------------------------------------------------------------
!
      end module legendre
