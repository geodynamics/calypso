!tri_sph_for_coriolis.f90
!      module tri_sph_for_coriolis
!
!      subroutine gaunt_coriolis(ltr)
!
!*****************************************
!*                                       *
!*   Program of Gaunt integral           *
!*              updated on '95, 9,Aug    *
!*                                       *
!*****************************************
!*
!*****************************************************
!*  ki,gi :gaunt integral (m3,l3,m2,l2,m1,l1)        *
!*  li,ei :elsasser integral (m3,l3,m2,l2,m1,l1)     *
!*            Sin : negative m   Cos : positive m    *
!*****************************************************
!*
!*****    selection rule    ***********************************
!*                                                            *
!*   For gaunt integral                                       *
!*    1) l1+l2+l3 is even                                     *
!*    2) l1,l2,l3 form the side of a triangle                 *
!*          l1 + l2 >= l3                                     *
!*          l2 + l3 >= l1                                     *
!*          l3 + l1 >= l2                                     *
!*    3) m1 +- m2 +- m3 = 0                                   *
!*    4) three of the harmonics of fai have COS or one has    *
!*                                                            *
!*   For Elsasser integral                                    *
!*    1) l1+l2+l3 is odd                                      *
!*    2) l1,l2,l3 form the side of a triangle                 *
!*    3) m1 +- m2 +- m3 = 0                                   *
!*    4) two of the harmonics of fai have COS or none has     *
!*                                                            *
!**************************************************************
!*
!*******************************************************************
!*                                                                 *
!*  Adams - Gaunt integrals                                        *
!*                                                                 *
!*  (m1,m2,m3)  /  (m1)   (m2)   (m3)                              *
!* Gi         = | P    * P    * P    *sin(theta) d(theta)          *
!*  (l1,l2,l3)  /  (l1)   (l2)   (l3)                              *
!*                                                                 *
!*                                        (m3)                     *
!*  (m1,m2,m3)  /  (m1)         (m2)    dy(l3)                     *
!* Ei         = | P    *[ m2 * P     * --------                    *
!*  (l1,l2,l3)  /  (l1)         (l2)   d(theta)                    *
!*                                                                 *
!*                        (m2)                                     *
!*                      dy(l2)     (m3)                            *
!*              - m3 * -------- * P     ] d(theta)                 *
!*                     d(theta)    (l3)                            *
!*                                                                 *
!*  (m1,m2,m3)  //  (m1)   (m2)   (m3)                             *
!* Ki         = || Y    * Y    * Y    *sin(theta) d(theta)d(phi)   *
!*  (l1,l2,l3)  //  (l1)   (l2)   (l3)                             *
!*                                                                 *
!*                            (m2)        (m3)                     *
!*  (m1,m2,m3)  //  (m1)    dy(l2)      dy(l3)                     *
!* Li         = || Y    *[ -------- * ----------                   *
!*  (l1,l2,l3)  //  (l1)   d(theta)    d(phi)                      *
!*                                                                 *
!*                    (m2)        (m3)                             *
!*                  dy(l2)      dy(l3)                             *
!*              - ---------- * -------- ] d(theta)d(phi)           *
!*                  d(phi)     d(theta)                            *
!*                                                                 *
!*  where                                                          *
!*                   (m)   (m)  | sin(m*phi) |                     *
!*                  Y   = P   * |   1        |                     *
!*                   (l)   (l)  | cos(m*phi) |                     *
!*                                                                 *
!*                         (m)    2*(l-m)!     1                   *
!*                        P   = [----------]**--- * P(l,m)         *
!*                         (l)      (l+m)!     2                   *
!*                         (0)                                     *
!*                        P   =           P(l,0)                   *
!*                         (l)                                     *
!*                                                                 *
!*******************************************************************
!*
      module tri_sph_for_coriolis
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine gaunt_coriolis(ltr)
!
      use m_machine_parameter
      use m_integrals_4_sph_coriolis
      use m_int_4_sph_coriolis_IO
      use set_integral_sph_coriolis
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ltr
!
      integer(kind = kint) :: jmax
      integer(kind = kint), allocatable :: idx_lc(:,:)
!
      integer(kind = kint) :: j1, l, m, j
!
!
!*  -----------    set index  ---------------
!*
      jmax = ltr * (ltr+2)
      allocate(idx_lc(jmax,3))
!
      do l = 1, ltr
        do m = -l, l
          j = l*(l+1) + m
          idx_lc(j,1) = j
          idx_lc(j,2) = l
          idx_lc(j,3) = m
        end do
      end do
!
! ------ evaluate of Gaunt integral with hermmonics ------
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_int_sph_coriolis'
      call allocate_int_sph_coriolis(jmax)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_global_sph_id_4_sph_cor'
      call copy_global_sph_id_4_sph_cor(jmax, idx_lc(1,1) )
!
      if(iflag_debug .gt. 0) write(*,*) 's_set_integral_sph_coriolis'
      call s_set_integral_sph_coriolis(ltr, jmax, idx_lc(1,1) )
!
!* ------ write data ------
!
      call copy_int_sph_coriolis_to_IO(ltr)
!
      call sel_write_int_4_sph_coriolis
!
      end subroutine gaunt_coriolis
!
!   --------------------------------------------------------------------
!
      end module tri_sph_for_coriolis
