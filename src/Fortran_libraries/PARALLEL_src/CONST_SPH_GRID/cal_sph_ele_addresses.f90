!>@file   cal_sph_ele_addresses.f90
!!@brief  module cal_sph_ele_addresses
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Set element connectivity for spherical shell grid
!!
!!@verbatim
!!      subroutine reset_local_sph_ele_constants
!!
!!      subroutine set_nele_lc_shell(ip_r, ip_t, stbl)
!!      subroutine set_nele_lc_Spole(ip_r, stbl)
!!      subroutine set_nele_lc_Npole(ip_r, stbl)
!!      subroutine set_nele_lc_ctr_sph(stbl)
!!      subroutine set_nele_ext_ctr_sph(ip_t, stbl)
!!      subroutine set_nele_center_Spole(stbl)
!!      subroutine set_nele_center_Npole(stbl)
!!      subroutine set_nele_gl_shell(stbl)
!!      subroutine set_nele_gl_Spole(stbl)
!!      subroutine set_nele_gl_Npole(stbl)
!!      subroutine set_nele_gl_ctr_sph(stbl)
!!
!!      subroutine cal_sph_local_numele(numele)
!!
!!      integer(kind = kint) function sph_shell_ele_id                  &
!!     &                            (ip_r, ip_t, kr, lt, mp, stbl)
!!      integer(kind = kint) function sph_s_pole_ele_id                 &
!!     &                            (ip_r, kr, mp, stbl)
!!      integer(kind = kint) function sph_n_pole_ele_id                 &
!!     &                            (ip_r, kr, mp, stbl)
!!      integer(kind = kint) function sph_inter_ctr_shell_ele_id        &
!!     &                            (lt, mp, stbl)
!!      integer(kind = kint) function sph_exter_ctr_shell_ele_id        &
!!     &                            (ip_t, lt, mp, stbl)
!!      integer(kind= kint) function sph_inter_ctr_spole_ele_id(mp)
!!      integer(kind= kint) function sph_inter_ctr_npole_ele_id(mp)
!!      integer(kind= kint) function sph_exter_ctr_npole_ele_id(mp)
!!
!!      integer(kind = kint_gl) function global_sph_shell_ele_id        &
!!                                     (kr, lt, mp, stbl)
!!      integer(kind = kint_gl) function global_sph_s_pole_ele_id       &
!!     &                               (kr, mp, stbl)
!!      integer(kind = kint_gl) function global_sph_n_pole_ele_id       &
!!     &                               (kr, mp, stbl)
!!      integer(kind = kint_gl) function global_ctr_shell_ele_id        &
!!     &                               (lt, mp, stbl)
!!      integer(kind= kint_gl) function global_ctr_spole_ele_id(mp)
!!      integer(kind= kint_gl) function global_ctr_npole_ele_id(mp)
!!@endverbatim
!!
!
      module cal_sph_ele_addresses
!
      use m_precision
      use m_constants
!
      use t_sph_mesh_1d_connect
!
      implicit none
!
      integer(kind = kint) :: nele_lc_shell =    0
      integer(kind = kint) :: nele_lc_Spole =    0
      integer(kind = kint) :: nele_lc_Npole =    0
      integer(kind = kint) :: nele_lc_ctr_sph =  0
      integer(kind = kint) :: nele_ext_ctr_sph = 0
      integer(kind = kint) :: nele_ctr_Spole =   0
      integer(kind = kint) :: nele_ctr_Npole =   0
!
!
      integer(kind = kint_gl) :: nele_gl_shell =   0
      integer(kind = kint_gl) :: nele_gl_Spole =   0
      integer(kind = kint_gl) :: nele_gl_Npole =   0
      integer(kind = kint_gl) :: nele_gl_ctr_sph = 0
      integer(kind = kint_gl) :: nele_gl_ctr_Spole = 0
      integer(kind = kint_gl) :: nele_gl_ctr_Npole = 0
!
      private :: nele_lc_shell, nele_lc_Spole, nele_lc_Npole
      private :: nele_lc_ctr_sph, nele_ext_ctr_sph
      private :: nele_ctr_Spole, nele_ctr_Npole
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reset_local_sph_ele_constants
!
!
      nele_lc_shell =    0
      nele_lc_Spole =    0
      nele_lc_Npole =    0
      nele_lc_ctr_sph =  0
      nele_ext_ctr_sph = 0
      nele_ctr_Spole =   0
      nele_ctr_Npole =   0
!
      nele_gl_shell =     0
      nele_gl_Spole =     0
      nele_gl_Npole =     0
      nele_gl_ctr_sph =   0
      nele_gl_ctr_Spole = 0
      nele_gl_ctr_Npole = 0
!
      end subroutine reset_local_sph_ele_constants
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_shell(ip_r, ip_t, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      nele_lc_shell =  stbl%nele_sph_r(ip_r) * stbl%nele_sph_t(ip_t)    &
     &               * stbl%nidx_global_fem(3)
!
      end subroutine set_nele_lc_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_Spole(ip_r, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r
!
!
      nele_lc_Spole =  stbl%nele_sph_r(ip_r) * stbl%nele_around_pole
!
      end subroutine set_nele_lc_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_Npole(ip_r, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r
!
      nele_lc_Npole = stbl%nele_sph_r(ip_r) * stbl%nele_around_pole
!
      end subroutine set_nele_lc_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_ctr_sph(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
!
!
      nele_lc_ctr_sph = (stbl%nidx_global_fem(2)-1)                     &
     &                 * stbl%nidx_global_fem(3)
!
      end subroutine set_nele_lc_ctr_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_ext_ctr_sph(ip_t, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_t
!
      nele_ext_ctr_sph =  stbl%nele_sph_t(ip_t)                         &
     &                  * stbl%nidx_global_fem(3)
!
      end subroutine set_nele_ext_ctr_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_center_Spole(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
!
      nele_ctr_Spole = stbl%nele_around_pole
!
      end subroutine set_nele_center_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_center_Npole(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
!
      nele_ctr_Npole = stbl%nele_around_pole
!
      end subroutine set_nele_center_Npole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_shell(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint_gl) :: nr8, nt8, np8
!
      nr8 = stbl%nidx_global_fem(1)-1
      nt8 = stbl%nidx_global_fem(2)-1
      np8 = stbl%nidx_global_fem(3)
      nele_gl_shell = nr8 * nt8 * np8
!
      end subroutine set_nele_gl_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_Spole(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint_gl) :: nr8
!
!
      nr8 = stbl%nidx_global_fem(1) - 1
      nele_gl_Spole = nr8 * stbl%nele_around_pole
!
      end subroutine set_nele_gl_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_Npole(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint_gl) :: nr8
!
      nr8 = stbl%nidx_global_fem(1) - 1
      nele_gl_Npole = nr8 * stbl%nele_around_pole
!
      end subroutine set_nele_gl_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_ctr_sph(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint_gl) :: nt8, np8
!
      nt8 = stbl%nidx_global_fem(2) - 1
      np8 = stbl%nidx_global_fem(3)
      nele_gl_ctr_sph = nt8 * np8
      nele_gl_ctr_Spole = stbl%nele_around_pole
      nele_gl_ctr_Npole = stbl%nele_around_pole
!
      end subroutine set_nele_gl_ctr_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_local_numele(numele)
!
      integer(kind = kint), intent(inout) :: numele
!
!
      numele = nele_lc_shell + nele_lc_Spole + nele_lc_Npole            &
     &            + nele_lc_ctr_sph + nele_ext_ctr_sph                  &
     &            + nele_ctr_Spole + nele_ctr_Npole
!
      end subroutine cal_sph_local_numele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_shell_ele_id                    &
     &                            (ip_r, ip_t, kr, lt, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      sph_shell_ele_id =  kr + (lt-1) * stbl%nele_sph_r(ip_r)           &
     &         + (mp-1) * stbl%nele_sph_r(ip_r) * stbl%nele_sph_t(ip_t)
!
      end function sph_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_s_pole_ele_id                   &
     &                            (ip_r, kr, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r
      integer(kind = kint), intent(in) :: kr, mp
!
!
      sph_s_pole_ele_id = kr + (mp-1) * stbl%nele_sph_r(ip_r)           &
     &                       + nele_lc_shell
!
      end function sph_s_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_n_pole_ele_id                   &
     &                            (ip_r, kr, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r
      integer(kind = kint), intent(in) :: kr, mp
!
!
      sph_n_pole_ele_id = kr + (mp-1) * stbl%nele_sph_r(ip_r)           &
     &                       + nele_lc_shell + nele_lc_Spole
!
      end function sph_n_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_inter_ctr_shell_ele_id          &
     &                            (lt, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: lt, mp
!
!
      sph_inter_ctr_shell_ele_id                                        &
     &   = lt + (mp-1) * (stbl%nidx_global_fem(2)-1)                    &
     &        + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_inter_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_exter_ctr_shell_ele_id          &
     &                           (ip_t, lt, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_t, lt, mp
!
!
      sph_exter_ctr_shell_ele_id = lt + (mp-1) * stbl%nele_sph_t(ip_t)  &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_exter_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind= kint) function sph_inter_ctr_spole_ele_id(mp)
!
      integer(kind = kint), intent(in) :: mp
!
!
      sph_inter_ctr_spole_ele_id                                        &
     &         = mp + nele_lc_shell + nele_lc_Spole + nele_lc_Npole     &
     &              + nele_lc_ctr_sph
!
      end function sph_inter_ctr_spole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind= kint) function sph_inter_ctr_npole_ele_id(mp)
!
      integer(kind = kint), intent(in) :: mp
!
!
      sph_inter_ctr_npole_ele_id                                        &
     &         = mp + nele_lc_shell + nele_lc_Spole + nele_lc_Npole     &
     &              + nele_lc_ctr_sph + nele_ctr_Spole
!
      end function sph_inter_ctr_npole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind= kint) function sph_exter_ctr_npole_ele_id(mp)
!
      integer(kind = kint), intent(in) :: mp
!
!
      sph_exter_ctr_npole_ele_id                                        &
     &         = mp + nele_lc_shell + nele_lc_Spole + nele_lc_Npole     &
     &              + nele_ext_ctr_sph
!
      end function sph_exter_ctr_npole_ele_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint_gl) function global_sph_shell_ele_id          &
     &                               (kr, lt, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: kr, lt, mp
      integer(kind = kint_gl) :: nr8, nt8
!
!
      nr8 = stbl%nidx_global_fem(1) - 1
      nt8 = stbl%nidx_global_fem(2) - 1
      global_sph_shell_ele_id =  kr + (lt-1) * nr8 + (mp-1) * nr8*nt8
!
      end function global_sph_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint_gl) function global_sph_s_pole_ele_id         &
     &                               (kr, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: kr, mp
      integer(kind = kint_gl) :: nr8
!
!
      nr8 = stbl%nidx_global_fem(1) - 1
      global_sph_s_pole_ele_id = kr + (mp-1) * nr8                      &
     &                          + nele_gl_shell
!
      end function global_sph_s_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint_gl) function global_sph_n_pole_ele_id         &
     &                               (kr, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: kr, mp
      integer(kind = kint_gl) :: nr8
!
!
      nr8 = stbl%nidx_global_fem(1) - 1
      global_sph_n_pole_ele_id = kr + (mp-1) * nr8                      &
     &                          + nele_gl_shell + nele_gl_Spole
!
      end function global_sph_n_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint_gl) function global_ctr_shell_ele_id          &
     &                              (lt, mp, stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: lt, mp
      integer(kind = kint_gl) :: nt8
!
!
      nt8 = stbl%nidx_global_fem(2) - 1
      global_ctr_shell_ele_id = lt + (mp-1) * nt8                       &
     &                         + nele_gl_shell + nele_gl_Spole          &
     &                         + nele_gl_Npole
!
      end function global_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind= kint_gl) function global_ctr_spole_ele_id(mp)
!
      integer(kind = kint), intent(in) :: mp
!
!
      global_ctr_spole_ele_id                                           &
     &      = mp + nele_gl_shell + nele_gl_Spole + nele_gl_Npole        &
     &           + nele_gl_ctr_sph
!
      end function global_ctr_spole_ele_id
!
! -----------------------------------------------------------------------
!
      integer(kind= kint_gl) function global_ctr_npole_ele_id(mp)
!
      integer(kind = kint), intent(in) :: mp
!
!
      global_ctr_npole_ele_id                                           &
     &      = mp + nele_gl_shell + nele_gl_Spole + nele_gl_Npole        &
     &           + nele_gl_ctr_sph + nele_gl_ctr_Spole
!
      end function global_ctr_npole_ele_id
!
! -----------------------------------------------------------------------
!
      end module cal_sph_ele_addresses
