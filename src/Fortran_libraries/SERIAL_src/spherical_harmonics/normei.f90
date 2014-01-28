!
!      module normei
!
!***************************************************
!*
!*      subroutine for lead elsasser integral
!*         switched the index
!*                                '98, 3, 9
!*
!***************************************************
!
!      double precision function setei(l1, m1, l2, m2, l3, m3)
!
!**********************************************************************
!*                                                                    *
!*   normarised Elsasser integrals                                    *
!*                                     (m3)         (m2)              *
!*  (m1,m2,m3)   /  1 (m1)     (m2)  dP(l3)       dP(l2)    (m3)      *
!* Es          = |   P   *[m2*P    *------- - m3*-------- *P    ] dx  *
!*  (l1,l2,l3)   / -1 (l1)     (l2)   dx            dx      (l3)      *
!*                                                                    *
!*     1                         1                             1      *
!*  = --- *[ {(l2+m2)*(l3-m3)}**--- *{ [(l2+m2-1)*(l3+m3+1)]**---     *
!*     2                         2                             2      *
!*                                                                    *
!*     (m1,m2-1,m3+1)                     1    (m1, m2 ,m3)           *
!*  *Gs              + [(l3-m3)*(l2-m2)]**- *Gs             }         *
!*     (l1,l2-1,l3)                       2    (l1,l2-1,l3)           *
!*                                                                    *
!*                          1                             1           *
!*  - [ {(l3+m3)*(l2-m2)}**--- *{ [(l3+m3-1)*(l2+m2+1)]**---          *
!*                          2                             2           *
!*                                                                    *
!*     (m1,m2+1,m3-1)                     1    (m1,m2,m3)             *
!*  *Gs              + [(l2-m2)*(l3-m3)]**- *Gs            }]         *
!*     (l1, l2 ,l3-1)                     2    (l1,l2,l3-1)           *
!*                                                                    *
!*      Gs : quasi normalised Gaunt integrals                         *
!*                                                                    *
!*              (m1,m2-1,m3+1)             (m1, m2 ,m3)               *
!*       g1 : Gs                    g2 : Gs                           *
!*              (l1,l2-1,l3)               (l1,l2-1,l3)               *
!*                                                                    *
!*              (m1,m2+1,m3-1)             (m1,m2,m3)                 *
!*       g3 : Gs                    g4 : Gs                           *
!*              (l1, l2 ,l3-1)             (l1,l2,l3-1)               *
!*                                                                    *
!**********************************************************************
!*
!***************************************************
!*
!*  ....   condition !!
!*  m1 = m2 + m3
!*
!***************************************************
!*
      module normei
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: leades
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      double precision function setei(l1, m1, l2, m2, l3, m3)
!
      use normgi
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      real(kind = kreal) gg(4)
!
!
      if ( l2.eq.l3 .and. m2.eq.m3 ) then
        setei = zero
      else
!*
        if ( m3+1.gt.l3 .or. abs(m2-1).gt.l2-1 )then
          gg(1) = zero
        else if (m2 .eq. 0 ) then
          gg(1) =-gs(m3+1,l3,1,l2-1,m1,l1)
        else
          gg(1) = gs(m1,l1,m2-1,l2-1,m3+1,l3)
        end if
!*
        if ( m2 .gt.l2-1 )then
          gg(2) = zero
        else
          gg(2) = gs(m1,l1,m2,l2-1,m3,l3)
        end if
!*
        if ( m2+1 .gt.l2 .or. abs(m3-1).gt.l3-1 )then
          gg(3) = zero
        else if ( m3 .eq. 0) then
          gg(3) = -gs(m2+1,l2,1,l3-1,m1,l1)
        else
          gg(3) = gs(m1,l1,m2+1,l2,m3-1,l3-1)
        end if
!*
        if ( m3 .gt.l3-1  )then
          gg(4) = zero
        else
          gg(4) = gs(m1,l1,m2,l2,m3,l3-1)
        end if
!*
        setei = leades(m2,l2,m3,l3,gg)
      end if
!*
      end function setei
!
!  ----------------------------------------------------------------------
!
      double precision function leades(m2,l2,m3,l3,gg)
!
      integer(kind = kint), intent(in) :: l2, l3
      integer(kind = kint), intent(in) :: m2, m3
      real(kind = kreal), intent(in) ::  gg(4)
!
      real(kind = kreal) a1,a2,a3,a4,a5
!
!
      a1 = dble( (l2+m2)*(l3-m3) )
      a2 = dble( (l3+m3)*(l2-m2) )
!
      a3 = dble( (l2+m2-1)*(l3+m3+1) )
      a4 = dble( (l3+m3-1)*(l2+m2+1) )
      a5 = dble( (l2-m2)*(l3-m3) )
!
      if (m2-1 .eq. 0 ) then
        a3 = a3 * two
      end if
      if (m3 .eq. 0 ) then
        a3 = a3 * half
        a4 = a4 * half
      end if
      if (m2 .eq. 0 ) then
        a3 = a3 * half
        a4 = a4 * half
      end if
      if (m3-1 .eq. 0 ) then
        a4 = a4 * two
      end if
!*
      leades = - (sqrt(a1) * (sqrt(a3) *gg(1) + sqrt(a5) * gg(2) )      &
     &         - sqrt(a2) * (sqrt(a4) *gg(3) + sqrt(a5) * gg(4) ))      &
     &        * half
!*
      end function leades
!
!  ----------------------------------------------------------------------
!
      end module normei
