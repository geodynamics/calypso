!>@file   normgi.f90
!!@brief  module normgi
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in Apr. 2009
!
!>@brief Select Evaluation of Adams-Gaunt integrals
!!
!!@verbatim
!!      double precision function gs_select(m1,l1,m2,l2,m3,l3)
!!
!!**********************************************************************
!!*                                                                    *
!!*     normalized Gaunt integral                                      *
!!*                                                                    *
!!*      (m1,m2,m3)   / 1  (m1)   (m2)   (m3)                          *
!!*    gs           = |   P    * P    * P     dx                       *
!!*      (l1,l2,l3)   /-1  (l1)   (l2)   (l3)                          *
!!*                                                                    *
!!*      2*{(-1)**(is-l2-m3)}*(2*is-2*l3)!*is!                         *
!!*  =  ---------------------------------------                        *
!!*      (is-l1)!*(is-l2)!*(is-l3)!*(2*is+1)!                          *
!!*                                                                    *
!!*       (l1-m1)!*(l2+m2)!*(l3+m3)!*(l3-m3)!     1                    *
!!*    * -------------------------------------]**---                   *
!!*                (l1+m1)!*(l2-m2)!              2                    *
!!*                                                                    *
!!*              {(-1)**it}*(l1+m1+it)!*(l2+l3-m1-it)!                 *
!!*   * SIGMA[--------------------------------------------]            *
!!*       it   (l1-m1-it)!*(l2-l3+m1+it)!*(l3-m3-it)!*it!              *
!!*                                                                    *
!!*                                                                    *
!!*                                                                    *
!!*                                                                    *
!!*                                                                    *
!!*  Where   m1 = m2+m3           (m1 >= m2 >= m3)                     *
!!*          is = (l1+l2+l3)/2                                         *
!!*          it : all the factors of factrials are nonnegative         *
!!*                                                                    *
!!*                                                                    *
!!*           (m)        (l-m)!      1                                 *
!!*          P   =[ 2 * -------- ]**--- * P                            *
!!*           (l)        (l+m)!      2     (l,m)                       *
!!*                                                                    *
!!*                                                                    *
!!**********************************************************************
!!@endverbatim
!
!
      module normgi
!
      use m_precision
!
      implicit none
!
!>     Integer flag to evaluate Adams-Gaunt integrals by prime factors
      integer(kind = kint), parameter :: iflag_gaunt_primes =    0
!>     Integer flag to evaluate Adams-Gaunt integrals by factorials
      integer(kind = kint), parameter :: iflag_gaunt_factorial = 1
!>     Integer flag to evaluate Adams-Gaunt integrals
!!      by multiple factorials
      integer(kind = kint), parameter :: iflag_gaunt_mul_fact =  2
!
!>     Integer flag for evaluation of Adams-Gaunt integrals
      integer(kind = kint), parameter :: iflag_gaunt_evaluation         &
     &                                  = iflag_gaunt_primes
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      double precision function gs_select(m1,l1,m2,l2,m3,l3)
!
      use normgi_by_fact
      use normgi_by_primes
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      if(iflag_gaunt_evaluation .eq. iflag_gaunt_mul_fact) then
        gs_select = gs_mul_fact(m1,l1,m2,l2,m3,l3)
      else if(iflag_gaunt_evaluation .eq. iflag_gaunt_factorial) then
        gs_select = gs_fact(m1,l1,m2,l2,m3,l3)
      else
        gs_select = gs_prime(m1,l1,m2,l2,m3,l3)
      end if
!
      end function gs_select
!
!  ---------------------------------------------------------------------
!
      end module normgi
