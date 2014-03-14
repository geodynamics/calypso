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
!!      subroutine set_nele_lc_shell(ip_r, ip_t, nidx_global_phi)
!!      subroutine set_nele_lc_Spole(ip_r)
!!      subroutine set_nele_lc_Npole(ip_r)
!!      subroutine set_nele_lc_ctr_sph(nidx_global_rtp)
!!      subroutine set_nele_ext_ctr_sph(ip_t, nidx_global_phi)
!!      subroutine set_nele_center_Spole
!!      subroutine set_nele_center_Npole
!!      subroutine set_nele_gl_shell(nidx_global_rtp)
!!      subroutine set_nele_gl_Spole(nidx_global_rtp)
!!      subroutine set_nele_gl_Npole(nidx_global_rtp)
!!      subroutine set_nele_gl_ctr_sph(nidx_global_rtp)
!!
!!      subroutine cal_sph_local_numele(numele)
!!
!!      integer function sph_shell_ele_id(ip_r, ip_t, kr, lt, mp)
!!      integer function sph_n_pole_ele_id(ip_r, kr, mp)
!!      integer function sph_inter_ctr_shell_ele_id(lt, mp)
!!      integer function sph_exter_ctr_shell_ele_id(ip_t, lt, mp)
!!      integer function sph_inter_ctr_spole_ele_id(mp)
!!      integer function sph_inter_ctr_npole_ele_id(mp)
!!      integer function sph_exter_ctr_npole_ele_id(mp)
!!
!!      integer function global_sph_shell_ele_id(kr, lt, mp)
!!      integer function global_sph_s_pole_ele_id(kr, mp)
!!      integer function global_sph_n_pole_ele_id(kr, mp)
!!      integer function global_ctr_shell_ele_id(lt, mp)
!!      integer function global_ctr_spole_ele_id(mp)
!!      integer function global_ctr_npole_ele_id(mp)
!!@endverbatim
!!
!
      module cal_sph_ele_addresses
!
      use m_precision
      use m_constants
!
      use m_sph_mesh_1d_connect
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
      integer(kind = kint) :: nele_gl_shell =   0
      integer(kind = kint) :: nele_gl_Spole =   0
      integer(kind = kint) :: nele_gl_Npole =   0
      integer(kind = kint) :: nele_gl_ctr_sph = 0
      integer(kind = kint) :: nele_gl_ctr_Spole = 0
      integer(kind = kint) :: nele_gl_ctr_Npole = 0
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
      subroutine set_nele_lc_shell(ip_r, ip_t, nidx_global_phi)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      integer(kind = kint), intent(in) :: nidx_global_phi
!
      nele_lc_shell = nele_sph_r(ip_r) * nele_sph_t(ip_t)               &
     &               * nidx_global_phi
!
      end subroutine set_nele_lc_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_Spole(ip_r)
!
      integer(kind = kint), intent(in) :: ip_r
!
!
      nele_lc_Spole =  nele_sph_r(ip_r) * nele_around_pole
!
      end subroutine set_nele_lc_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_Npole(ip_r)
!
      integer(kind = kint), intent(in) :: ip_r
!
      nele_lc_Npole = nele_sph_r(ip_r) * nele_around_pole
!
      end subroutine set_nele_lc_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_lc_ctr_sph(nidx_global_rtp)
!
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
!
      nele_lc_ctr_sph = (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
!
      end subroutine set_nele_lc_ctr_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_ext_ctr_sph(ip_t, nidx_global_phi)
!
      integer(kind = kint), intent(in) :: ip_t, nidx_global_phi
!
      nele_ext_ctr_sph = nele_sph_t(ip_t) * nidx_global_phi
!
      end subroutine set_nele_ext_ctr_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_center_Spole
!
      nele_ctr_Spole = nele_around_pole
!
      end subroutine set_nele_center_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_center_Npole
!
      nele_ctr_Npole = nele_around_pole
!
      end subroutine set_nele_center_Npole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_shell(nidx_global_rtp)
!
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
!
      nele_gl_shell = (nidx_global_rtp(1)-1) * (nidx_global_rtp(2)-1)   &
     &                                       *  nidx_global_rtp(3)
!
      end subroutine set_nele_gl_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_Spole(nidx_global_rtp)
!
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
!
      nele_gl_Spole = (nidx_global_rtp(1)-1) * nele_around_pole
!
      end subroutine set_nele_gl_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_Npole(nidx_global_rtp)
!
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
!
      nele_gl_Npole = (nidx_global_rtp(1)-1) * nele_around_pole
!
      end subroutine set_nele_gl_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_nele_gl_ctr_sph(nidx_global_rtp)
!
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
!
      nele_gl_ctr_sph = (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
      nele_gl_ctr_Spole = nele_around_pole
      nele_gl_ctr_Npole = nele_around_pole
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
      integer function sph_shell_ele_id(ip_r, ip_t, kr, lt, mp)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      sph_shell_ele_id =  kr + (lt-1) * nele_sph_r(ip_r)                &
     &                  + (mp-1) * nele_sph_r(ip_r)*nele_sph_t(ip_t)
!
      end function sph_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_s_pole_ele_id(ip_r, kr, mp)
!
      integer(kind = kint), intent(in) :: ip_r
      integer(kind = kint), intent(in) :: kr, mp
!
!
      sph_s_pole_ele_id = kr + (mp-1) * nele_sph_r(ip_r)                &
     &                        + nele_lc_shell
!
      end function sph_s_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_n_pole_ele_id(ip_r, kr, mp)
!
      integer(kind = kint), intent(in) :: ip_r
      integer(kind = kint), intent(in) :: kr, mp
!
!
      sph_n_pole_ele_id = kr + (mp-1) * nele_sph_r(ip_r)                &
     &                        + nele_lc_shell + nele_lc_Spole
!
      end function sph_n_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_inter_ctr_shell_ele_id(lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: lt, mp
!
!
      sph_inter_ctr_shell_ele_id = lt + (mp-1) * (nidx_global_rtp(2)-1) &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_inter_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_exter_ctr_shell_ele_id(ip_t, lt, mp)
!
      integer(kind = kint), intent(in) :: ip_t, lt, mp
!
!
      sph_exter_ctr_shell_ele_id = lt + (mp-1) * nele_sph_t(ip_t)       &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_exter_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_inter_ctr_spole_ele_id(mp)
!
      use m_spheric_parameter
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
      integer function sph_inter_ctr_npole_ele_id(mp)
!
      use m_spheric_parameter
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
      integer function sph_exter_ctr_npole_ele_id(mp)
!
      use m_spheric_parameter
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
      integer function global_sph_shell_ele_id(kr, lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      global_sph_shell_ele_id =  kr + (lt-1) * (nidx_global_rtp(1)-1)   &
     &        + (mp-1) * (nidx_global_rtp(1)-1)*(nidx_global_rtp(2)-1)
!
      end function global_sph_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_s_pole_ele_id(kr, mp)
!
      integer(kind = kint), intent(in) :: kr, mp
!
!
      global_sph_s_pole_ele_id = kr + (mp-1) * (nidx_global_rtp(1)-1)   &
     &                          + nele_gl_shell
!
      end function global_sph_s_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_n_pole_ele_id(kr, mp)
!
      integer(kind = kint), intent(in) :: kr, mp
!
!
      global_sph_n_pole_ele_id = kr + (mp-1) * (nidx_global_rtp(1)-1)   &
     &                          + nele_gl_shell + nele_gl_Spole
!
      end function global_sph_n_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_ctr_shell_ele_id(lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: lt, mp
!
!
      global_ctr_shell_ele_id = lt + (mp-1) * (nidx_global_rtp(2)-1)    &
     &           + nele_gl_shell + nele_gl_Spole + nele_gl_Npole
!
      end function global_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_ctr_spole_ele_id(mp)
!
      use m_spheric_parameter
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
      integer function global_ctr_npole_ele_id(mp)
!
      use m_spheric_parameter
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
