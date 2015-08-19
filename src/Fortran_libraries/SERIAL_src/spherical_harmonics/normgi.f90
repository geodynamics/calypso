!> @file  normgi.f90
!!      module normgi
!!
!! @author  H. Matsui
!! @date Programmed in 1994
!
!> @brief module for GAunt integrals with Schmidt normalization
!!
!!@verbatim
!!      double precision function gs_fact(m1,l1,m2,l2,m3,l3)
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!*
      double precision function gs(m1,l1,m2,l2,m3,l3)
!*
!      use normgi_by_fact
      use normgi_by_primes
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
!      gs = gs_mul_fact(m1,l1,m2,l2,m3,l3)
!      gs = gs_fact(m1,l1,m2,l2,m3,l3)
      gs = gs_prime(m1,l1,m2,l2,m3,l3)
!*
!      write(*,*) m1,l1,m2,l2,m3,l3,                                    &
!     &   (gs-gs_mul_fact(m1,l1,m2,l2,m3,l3)),                          &
!     &   (gs-gs_fact(m1,l1,m2,l2,m3,l3))
!
      end function gs
!
!  ---------------------------------------------------------------------
!
      end module normgi
