!
!      double precision function gs_fact(m1,l1,m2,l2,m3,l3)
!
!**********************************************************************
!*                                                                    *
!*     normalized Gaunt integral                                      *
!*                                                                    *
!*      (m1,m2,m3)   / 1  (m1)   (m2)   (m3)                          *
!*    gs           = |   P    * P    * P     dx                       *
!*      (l1,l2,l3)   /-1  (l1)   (l2)   (l3)                          *
!*                                                                    *
!*      2*{(-1)**(is-l2-m3)}*(2*is-2*l3)!*is!                         *
!*  =  ---------------------------------------                        *
!*      (is-l1)!*(is-l2)!*(is-l3)!*(2*is+1)!                          *
!*                                                                    *
!*       (l1-m1)!*(l2+m2)!*(l3+m3)!*(l3-m3)!     1                    *
!*    * -------------------------------------]**---                   *
!*                (l1+m1)!*(l2-m2)!              2                    *
!*                                                                    *
!*              {(-1)**it}*(l1+m1+it)!*(l2+l3-m1-it)!                 *
!*   * SIGMA[--------------------------------------------]            *
!*       it   (l1-m1-it)!*(l2-l3+m1+it)!*(l3-m3-it)!*it!              *
!*                                                                    *
!*                                                                    *
!*                                                                    *
!*                                                                    *
!*                                                                    *
!*  Where   m1 = m2+m3           (m1 >= m2 >= m3)                     *
!*          is = (l1+l2+l3)/2                                         *
!*          it : all the factors of factrials are nonnegative         *
!*                                                                    *
!*                                                                    *
!*           (m)        (l-m)!      1                                 *
!*          P   =[ 2 * -------- ]**--- * P                            *
!*           (l)        (l+m)!      2     (l,m)                       *
!*                                                                    *
!*                                                                    *
!**********************************************************************
!
      module normgi_by_fact
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
!  ---------------------------------------------------------------------
!*
      double precision function gs_mul_fact(m1,l1,m2,l2,m3,l3)
!*
      use quicksort
      use factorials
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      integer(kind = kint) kiu(0:2), kil(0:4), ktu(0:2), ktl(0:4)
      integer(kind = kint) ksu(0:4), ksl(0:2)
      integer(kind = kint) :: itmin, itmax, is, it
      real(kind = kreal) :: ssqu , tsum , tfac
!
!
      is = int((l1+l2+l3)/2)
!*
      itmax = min ( (l1-m1) ,(l3-m3) ,(l2+l3-m1))
      itmin = -( min ( (l2-l3+m1) ,(l1+m1) , 0 ) )
      tsum = 0
!*
!*
      kil(0) = 0
      kil(1) = is-l1
      kil(2) = is-l2
      kil(3) = is-l3
      kil(4) = 2*is+1
      kiu(0) = 0
      kiu(1) = 2*is-2*l3
      kiu(2) = is
      ksu(0) = 0
      ksu(1) = l1-m1
      ksu(2) = l2+m2
      ksu(3) = l3+m3
      ksu(4) = l3-m3
      ksl(0) = 0
      ksl(1) = l1+m1
      ksl(2) = l2-m2
!*
      call quicksort_int(5,kil(0),1,5)
      call quicksort_int(3,kiu(0),1,3)
      call quicksort_int(3,ksl(0),1,3)
      call quicksort_int(5,ksu(0),1,5)
!!*
!!*  ------- caliculate of factrials --------------
!!*
      gs_mul_fact = (-one)**(is-l2-m3) * two
!*
      gs_mul_fact = gs_mul_fact * factorial(kiu(0) ,kiu(1) ,2)
      gs_mul_fact = gs_mul_fact / factorial(kil(0) ,kil(1) ,4)
!
      gs_mul_fact = gs_mul_fact * factorial(kiu(1) ,kiu(2) ,1)
      gs_mul_fact = gs_mul_fact / factorial(kil(1) ,kil(2) ,3)
!
      gs_mul_fact = gs_mul_fact / factorial(kil(2) ,kil(3) ,2)
      gs_mul_fact = gs_mul_fact / factorial(kil(3) ,kil(4) ,1)
!
!
      ssqu = one
      ssqu = ssqu * factorial(ksu(0) ,ksu(1) ,4)
      ssqu = ssqu / factorial(ksl(0) ,ksl(1) ,2)
!
      ssqu = ssqu * factorial(ksu(1) ,ksu(2) ,3)
      ssqu = ssqu / factorial(ksl(1) ,ksl(2) ,1)
!
      ssqu = ssqu * factorial(ksu(2) ,ksu(3) ,2)
      ssqu = ssqu * factorial(ksu(3) ,ksu(4) ,1)
!*
      gs_mul_fact = gs_mul_fact * sqrt(ssqu)
!*
!
!*  ---- calisulate summuation ------
!*
      do it = itmin ,itmax
        ktu(0) = 0
        ktu(1) = l1+m1+it
        ktu(2) = l2+l3-m1-it
        ktl(0) = 0
        ktl(1) = l1-m1-it
        ktl(2) = l2-l3+m1+it
        ktl(3) = l3-m3-it
        ktl(4) = it
!*
!*
        call quicksort_int(3,ktu(0),1,3)
        call quicksort_int(5,ktl(0),1,5)
!*
        tfac = (-1)**it
!*
        tfac = tfac * factorial(ktu(0),ktu(1),2)
        tfac = tfac / factorial(ktl(0),ktl(1),4)
!
        tfac = tfac * factorial(ktu(1),ktu(2),1)
        tfac = tfac / factorial(ktl(1),ktl(2),3)
!
        tfac = tfac / factorial(ktl(2),ktl(3),2)
        tfac = tfac / factorial(ktl(3),ktl(4),1)
!*
        tsum = tsum + tfac
      end do
!*
      gs_mul_fact = gs_mul_fact*tsum
!*
      if (m1 .ne. 0)  then
        gs_mul_fact = gs_mul_fact * sqrt(two)
      end if
      if (m2 .ne. 0)  then
        gs_mul_fact = gs_mul_fact * sqrt(two)
      end if
      if (m3 .ne. 0)  then
        gs_mul_fact = gs_mul_fact * sqrt(two)
      end if
!*      
      end function gs_mul_fact
!
!  ---------------------------------------------------------------------
!*
      double precision function gs_fact(m1,l1,m2,l2,m3,l3)
!*
      use quicksort
      use factorials
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      integer(kind = kint) kiu(0:2), kil(0:4), ktu(0:2), ktl(0:4)
      integer(kind = kint) ksu(0:4), ksl(0:2)
      integer(kind = kint) :: itmin, itmax, is, it
      real(kind = kreal) :: tsum , tfac
      real(kind = kreal) :: gp1, gp2, sqp1, sqp2, sqp3, sqp4
      real(kind = kreal) :: gn1, gn2, gn3, gn4, sqn1, sqn2
      real(kind = kreal) :: tfp1, tfp2, tfn1, tfn2, tfn3, tfn4
!
!
      is = int((l1+l2+l3)/2)
!*
      itmax = min ( (l1-m1) ,(l3-m3) ,(l2+l3-m1))
      itmin = -( min ( (l2-l3+m1) ,(l1+m1) , 0 ) )
      tsum = 0
!*
!*
      kil(0) = 0
      kil(1) = is-l1
      kil(2) = is-l2
      kil(3) = is-l3
      kil(4) = 2*is+1
      kiu(0) = 0
      kiu(1) = 2*is-2*l3
      kiu(2) = is
      ksu(0) = 0
      ksu(1) = l1-m1
      ksu(2) = l2+m2
      ksu(3) = l3+m3
      ksu(4) = l3-m3
      ksl(0) = 0
      ksl(1) = l1+m1
      ksl(2) = l2-m2
!*
      call quicksort_int(5,kil(0),1,5)
      call quicksort_int(3,kiu(0),1,3)
      call quicksort_int(3,ksl(0),1,3)
      call quicksort_int(5,ksu(0),1,5)
!
!!*  ------- caliculate of factrials --------------
!
      gs_fact = (-one)**(is-l2-m3) * two
!*
      gp1 = factorial(kiu(0) ,kiu(1) ,1)
      gn1 = one / factorial(kil(0) ,kil(1) ,1)
!
      gp2 = factorial(kiu(1) ,kiu(2) ,1)
      gn2 = one / factorial(kil(1) ,kil(2) ,1)
!
      gn3 = one / factorial(kil(2) ,kil(3) ,1)
      gn4 = one / factorial(kil(3) ,kil(4) ,1)
!
      gs_fact = gs_fact * gp1**2 * gn1**4 * gp2 * gn2**3 * gn3**2 * gn4
!
!
      sqp1 = factorial(ksu(0) ,ksu(1) ,1)
      sqn1 = one / factorial(ksl(0) ,ksl(1) ,1)
!
      sqp2 = factorial(ksu(1) ,ksu(2) ,1)
      sqn2 = one / factorial(ksl(1) ,ksl(2) ,1)
!
      sqp3 = factorial(ksu(2) ,ksu(3) ,1)
      sqp4 = factorial(ksu(3) ,ksu(4) ,1)
!
      gs_fact = gs_fact * sqp1**2 * sqn1 * sqp3                         &
     &         * sqrt(sqp2**3 * sqn2 * sqp4)
!
!*  ---- calisulate summuation ------
!*
      do it = itmin ,itmax
        ktu(0) = 0
        ktu(1) = l1+m1+it
        ktu(2) = l2+l3-m1-it
        ktl(0) = 0
        ktl(1) = l1-m1-it
        ktl(2) = l2-l3+m1+it
        ktl(3) = l3-m3-it
        ktl(4) = it
!*
!*
        call quicksort_int(3,ktu(0),1,3)
        call quicksort_int(5,ktl(0),1,5)
!*
        tfac = (-1)**it
!*
        tfp1 = factorial(ktu(0),ktu(1),1)
        tfn1 = one / factorial(ktl(0),ktl(1),1)
!
        tfp2 = factorial(ktu(1),ktu(2),1)
        tfn2 = one / factorial(ktl(1),ktl(2),1)
!
        tfn3 = one / factorial(ktl(2),ktl(3),1)
        tfn4 = one / factorial(ktl(3),ktl(4),1)
!*
        tsum = tsum + tfac * tfp1**2 * tfn1**4 * tfp2 * tfn2**3        &
     &                     * tfn3**2 * tfn4
      end do
!*
      gs_fact = gs_fact*tsum
!*
      if (m1 .ne. 0)  then
        gs_fact = gs_fact * sqrt(two)
      end if
      if (m2 .ne. 0)  then
        gs_fact = gs_fact * sqrt(two)
      end if
      if (m3 .ne. 0)  then
        gs_fact = gs_fact * sqrt(two)
      end if
!*      
      end function gs_fact
!
!  ---------------------------------------------------------------------
!
      end module normgi_by_fact
