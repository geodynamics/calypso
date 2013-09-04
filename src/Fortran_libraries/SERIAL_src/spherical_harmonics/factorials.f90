!
!      module factorials
!
!> @brief  double precision function fact(ns,ne,ni)
!!
!!
!!@n********************************************
!!@n*                                          *
!!@n*     caliculate factorials                *
!!@n*                                          *
!!@n*  factorial(ns,ne,ni) = [ne! / ns!] **ni  *
!!@n*                                          *
!!@n********************************************
!!@n @param ns       Start number for factorization
!!@n @param ne       End number for factorization
!!@n @param ni       Order of factorization
!!@n @param fact(ns,ne,ni)  \f$ \left(\frac{ns!}{ne!} \right)^{ni}\f$
!
      module factorials
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      double precision function factorial(ns,ne,ni)
!
      integer(kind = kint), intent(in) :: ns, ne, ni
!
      integer(kind = kint) :: i
!
!
      factorial = one
      if (ns.lt.0 .or. ne.lt.ns) then
        write (6,690) ns,ne,ni
        return
      end if
!
      do i = ns+1 ,ne
        factorial = factorial * (dble(i)**ni)
      end do
!
 690  format('error in factrials  (start,end,power) = ',3i3)
!
      end function factorial
!
!  ---------------------------------------------------------------------
!
      end module factorials
