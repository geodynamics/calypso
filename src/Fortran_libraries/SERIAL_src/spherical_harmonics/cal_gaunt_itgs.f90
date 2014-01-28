!cal_gaunt_itgs.f90
!      module cal_gaunt_itgs
!
!!***********************************************************************
!!*       subroutine of lead Gaunt integrals with harmonics
!!*                                  update : 1995 08 08
!!***********************************************************************
!!*
!!
!!      double precision function leadki(l1, m1, l2, m2, l3, m3)
!!
!!*******************************************************************
!!*
!!*  Adams - Gaunt integrals
!!*
!!*  (m1,m2,m3)  //  (m1)   (m2)   (m3)  
!!* Ki         = || Y    * Y    * Y    *sin(theta) d(theta)d(fai)   
!!*  (l1,l2,l3)  //  (l1)   (l2)   (l3)                             
!!*                                                                 
!!*  where                                                          
!!*                   (m)   (m)  | sin(m*fai) |                     
!!*                  Y   = P   * |   1        |                     
!!*                   (l)   (l)  | cos(m*fai) |                     
!!*                                                                 
!!*                         (m)    2*(l-m)!     1                   
!!*                        P   = [----------]**--- * P(l,m)         
!!*                         (l)      (l+m)!     2                   
!!*           mm1 = abs(m1)
!!*           mm2 = abs(m3)
!!*           mm2 = abs(m3)
!!*
!!*           mm1 = mm2 + m3
!!*
!!*******************************************************************
!!*
!!
!!      double precision function leadli(l1, m1, l2, m2, l3, m3)
!!
!!***********************************************************************
!!*
!!*          ei : elsasser integral only Schmidt function (input)
!!*          li : Elsasser integral with harmonics (output)
!!*                            (m2)        (m3)                     
!!*  (m1,m2,m3)  //  (m1)    dY(l2)      dY(l3)                     
!!* Li         = || Y    *[ -------- * ----------                   
!!*  (l1,l2,l3)  //  (l1)   d(theta)     d(phi)                    
!!*                                                                 
!!*                    (m2)        (m3)                             
!!*                  dY(l2)      dY(l3)                             
!!*              - ---------- * -------- ] d(theta)d(fai)           
!!*                  d(phi)     d(theta)                             
!!*                                                                 
!!*  where                                                          
!!*                   (m)   (m)  | sin(m*fai) |                     
!!*                  Y   = P   * |   1        |                     
!!*                   (l)   (l)  | cos(m*fai) |                     
!!*                                                                 
!!*                         (m)    2*(l-m)!     1                   
!!*                        P   = [----------]**--- * P(l,m)         
!!*                         (l)      (l+m)!     2                   
!!*           mm1 = abs(m1)
!!*           mm2 = abs(m3)
!!*           mm2 = abs(m3)
!!*
!!*           mm1 = mm2 + m3
!!*                                                                 
!!********************************************************************
!!*
      module cal_gaunt_itgs
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: normki, normli
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      double precision function leadki(l1, m1, l2, m2, l3, m3)
!
      use normgi
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      integer(kind = kint) :: mm1, mm2, mm3
      real(kind = kreal)  :: gi
!
!
      leadki = zero
      if ( l1+l2.ge.l3 .and. l2+l3.ge.l1 .and. l3+l1.ge.l2) then
        if ( mod(l1+l2+l3,2) .eq. 0) then
          if (    (m1+m2+m3).eq.0 .or. ( m1+m2-m3).eq.0                 &
     &      .or.  (m1-m2+m3).eq.0 .or. (-m1+m2+m3).eq.0) then
!*
            mm3 = abs(m3)
            mm2 = abs(m2)
            mm1 = abs(m1)
!
            if (mm1 .eq. mm2+mm3) then
              gi = gs(mm1,l1,mm2,l2,mm3,l3)
              leadki = normki(gi, m1, m2, m3)
!*
            else if (mm2 .eq. mm1+mm3) then
              gi = gs(mm2,l2,mm1,l1,mm3,l3)
              leadki = normki(gi, m2, m1, m3)
!*
            else if (mm3 .eq. mm2+mm1) then
              gi = gs(mm3,l3,mm2,l2,mm1,l1)
              leadki = normki(gi, m3, m2, m1)
            endif
!*
          end if
        end if
      end if
!
      end function leadki
!
! ----------------------------------------------------------------------
!
      double precision function leadli(l1, m1, l2, m2, l3, m3)
!
      use normei
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      integer(kind = kint) :: mm1, mm2, mm3
      real(kind = kreal)  :: ei
!
!
      leadli = zero
      if (l1+l2.ge.l3 .and. l2+l3.ge.l1 .and. l3+l1.ge.l2) then
        if ( mod(l1+l2+l3,2).eq.1 ) then
          if (   (m1+m2+m3).eq.0 .or. ( m1+m2-m3).eq.0                  &
     &      .or. (m1-m2+m3).eq.0 .or. (-m1+m2+m3).eq.0) then
!*
            mm3 = abs(m3)
            mm2 = abs(m2)
            mm1 = abs(m1)
!*
            if (mm1 .eq. mm2+mm3) then
              ei =     setei(l1, mm1, l2, mm2, l3, mm3)
              leadli =          normli(ei ,m1 ,m2 ,m3 ,mm1)
!*
            else if (mm2 .eq. mm1+mm3) then
              ei = setei(l2, mm2, l1, mm1, l3, mm3)
              leadli = dminus * normli(ei ,m2 ,m1 ,m3 ,mm2)
!*
            else if (mm3 .eq. mm2+mm1) then
              ei = setei(l3, mm3, l2, mm2, l1, mm1)
              leadli = dminus * normli( ei ,m3 ,m2 ,m1 ,mm3)
            end if
!*
          end if
        end if
      end if
!
      end function leadli
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      double precision function normki(gi, m1, m2, m3)
!
      implicit none
!
      integer(kind = kint), intent(in) :: m1, m2, m3
      real(kind = kreal), intent(in) ::  gi
!*
!
      if (m1*m2*m3 .gt. 0 ) then
        if (m2.lt.0 .and. m3.lt.0) then
          normki =-gi * half
        else
          normki = gi * half
        endif
!*
      else if (m2.eq.0 .and. m1*m3.gt.0 ) then
        normki = gi
      else if (m3.eq.0 .and. m2*m1.gt.0 ) then
        normki = gi
      else if (m3.eq.0 .and. m2.eq.0 .and. m1.eq.0) then
        normki = two * gi
      else
        normki = zero
      end if
!*
      end function normki
!
!  ---------------------------------------------------------------------
!
      double precision function normli(ei,m1,m2,m3,mm1)
!
      integer(kind = kint), intent(in) :: m1,  m2,  m3, mm1
      real(kind = kreal), intent(in) :: ei
!
!
      if (m1*m2*m3 .lt. 0 ) then
        if (m3.gt.0 .and. m2.gt.0) then
          normli =  ei * half
        else
          normli = -ei * half
        end if
!*
      else if (m2.eq.0 .and. m1*m3.lt.0) then
        normli = -dble(m1/mm1) * ei
      else if (m3.eq.0 .and. m2*m1.lt.0) then
        normli = -dble(m1/mm1) * ei
      else
        normli = zero
      end if
!*
      end function normli
!
!  ---------------------------------------------------------------------
!
      end module cal_gaunt_itgs
