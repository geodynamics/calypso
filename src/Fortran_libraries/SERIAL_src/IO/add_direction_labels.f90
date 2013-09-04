!>@file   add_direction_labels.f90
!!@brief  module add_direction_labels
!!
!!@author H. Matsui
!!@date Programmed on  March 2012
!
!>@brief put direction labels at the end of a field label
!!
!!@verbatim
!!      subroutine add_vector_direction_label_xyz(field_label,          &
!!     &           label_x, label_y, label_z)
!!      subroutine add_vector_direction_label_rtp(field_label,          &
!!     &           label_r, label_t, label_p)
!!      subroutine add_vector_direction_label_cyl(field_label,          &
!!     &           label_s, label_p, label_z)
!!
!!      subroutine add_tensor_direction_label_xyz(field_label,          &
!!     &           label_xx, label_xy, label_xz, label_yy, label_yz,    &
!!     &           label_zz)
!!      subroutine add_tensor_direction_label_rtp(field_label,          &
!!     &           label_rr, label_rt, label_rp, label_tt, label_tp,    &
!!     &           label_pp)
!!      subroutine add_tensor_direction_label_cyl(field_label,          &
!!     &           label_ss, label_sp, label_sz, label_pp, label_pz,    &
!!     &           label_zz)
!!
!!      subroutine add_asym_tensor_dir_label_xyz(field_label,           &
!!     &           label_xy, label_xz, label_yz)
!!      subroutine add_asym_tensor_dir_label_rtp(field_label,           &
!!     &           label_rt, label_rp, label_tp)
!!      subroutine add_asym_tensor_dir_label_cyl(field_label,           &
!!     &           label_sp, label_sz, label_pz)
!!
!!      subroutine add_vector_sph_spectr_label(field_label,             &
!!     &           label_pol, label_tor, label_dpol)
!!      subroutine add_vector_power_sph_label(field_label,              &
!!     &           label_pol, label_tor, label_tot)
!!@endverbatim
!
!!@n @param  field_label  field name (input)
!!
!!@n @param  label_x  name for x-component (field_name_x)
!!@n @param  label_y  name for y-component (field_name_y)
!!@n @param  label_z  name for z-component (field_name_z)
!!@n @param  label_r  name for r-component (field_name_r)
!!@n @param  label_t  name for theta-component (field_name_t)
!!@n @param  label_p  name for phi-component (field_name_p)
!!@n @param  label_s  name for s-component (field_name_s)
!!
!!@n @param  label_xx  name for xx-component (field_name_xx)
!!@n @param  label_xy  name for xy-component (field_name_xy)
!!@n @param  label_xz  name for xz-component (field_name_xz)
!!@n @param  label_yy  name for yy-component (field_name_yy)
!!@n @param  label_yz  name for yz-component (field_name_yz)
!!@n @param  label_zz  name for zz-component (field_name_zz)
!!@n @param  label_rr  name for rr-component (field_name_rr)
!!@n @param  label_rt  name for rt-component (field_name_rt)
!!@n @param  label_rp  name for rp-component (field_name_rp)
!!@n @param  label_tt  name for tt-component (field_name_tt)
!!@n @param  label_tp  name for tp-component (field_name_tp)
!!@n @param  label_pp  name for pp-component (field_name_pp)
!!@n @param  label_ss  name for ss-component (field_name_ss)
!!@n @param  label_sp  name for sp-component (field_name_sp)
!!@n @param  label_sz  name for sz-component (field_name_sz)
!!@n @param  label_pz  name for pz-component (field_name_pz)
!!
!!@n @param  label_pol  name for poloidal component (field_name_pol)
!!@n @param  label_tor  name for toroidal component (field_name_tor)
!!@n @param  label_dpol name for diff. of poloidal (field_name_pol_dr)
!!@n @param  label_tot  name for total    amplitude (field_name_tot)
!
      module add_direction_labels
!
      use m_precision
!
      implicit none
!
      character(len=kchara), parameter :: x_label = '_x'
      character(len=kchara), parameter :: y_label = '_y'
      character(len=kchara), parameter :: z_label = '_z'
      character(len=kchara), parameter :: xx_label = '_xx'
      character(len=kchara), parameter :: xy_label = '_xy'
      character(len=kchara), parameter :: xz_label = '_xz'
      character(len=kchara), parameter :: yy_label = '_yy'
      character(len=kchara), parameter :: yz_label = '_yz'
      character(len=kchara), parameter :: zz_label = '_zz'
!
      character(len=kchara), parameter :: r_label = '_r'
      character(len=kchara), parameter :: t_label = '_theta'
      character(len=kchara), parameter :: p_label = '_phi'
      character(len=kchara), parameter :: rr_label = '_rr'
      character(len=kchara), parameter :: rt_label = '_rt'
      character(len=kchara), parameter :: rp_label = '_rp'
      character(len=kchara), parameter :: tt_label = '_tt'
      character(len=kchara), parameter :: tp_label = '_tp'
      character(len=kchara), parameter :: pp_label = '_pp'
!
      character(len=kchara), parameter :: s_label = '_s'
      character(len=kchara), parameter :: ss_label = '_ss'
      character(len=kchara), parameter :: sp_label = '_sp'
      character(len=kchara), parameter :: sz_label = '_sz'
      character(len=kchara), parameter :: pz_label = '_pz'
!
      character(len=kchara), parameter :: pol_label =    '_pol'
      character(len=kchara), parameter :: dr_pol_label = '_pol_dr'
      character(len=kchara), parameter :: tor_label =    '_tor'
!
      private :: x_label, y_label, z_label
      private :: xx_label, xy_label, xz_label
      private :: yy_label, yz_label, zz_label
      private :: r_label, t_label, p_label
      private :: rr_label, rt_label, rp_label
      private :: tt_label, tp_label, pp_label
      private :: s_label, ss_label, sp_label
      private :: sz_label, pz_label
      private :: pol_label, dr_pol_label, tor_label
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_direction_label_xyz(field_label,            &
     &           label_x, label_y, label_z)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_x
      character(len=kchara), intent(inout) :: label_y
      character(len=kchara), intent(inout) :: label_z
!
!
      write(label_x,'(a,a)') trim(field_label), trim(x_label)
      write(label_y,'(a,a)') trim(field_label), trim(y_label)
      write(label_z,'(a,a)') trim(field_label), trim(z_label)
!
      end subroutine add_vector_direction_label_xyz
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_direction_label_rtp(field_label,            &
     &           label_r, label_t, label_p)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_r
      character(len=kchara), intent(inout) :: label_t
      character(len=kchara), intent(inout) :: label_p
!
!
      write(label_r,'(a,a)') trim(field_label), trim(r_label)
      write(label_t,'(a,a)') trim(field_label), trim(t_label)
      write(label_p,'(a,a)') trim(field_label), trim(p_label)
!
      end subroutine add_vector_direction_label_rtp
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_direction_label_cyl(field_label,            &
     &           label_s, label_p, label_z)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_s
      character(len=kchara), intent(inout) :: label_p
      character(len=kchara), intent(inout) :: label_z
!
!
      write(label_s,'(a,a)') trim(field_label), trim(s_label)
      write(label_p,'(a,a)') trim(field_label), trim(p_label)
      write(label_z,'(a,a)') trim(field_label), trim(z_label)
!
      end subroutine add_vector_direction_label_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_tensor_direction_label_xyz(field_label,            &
     &           label_xx, label_xy, label_xz, label_yy, label_yz,      &
     &           label_zz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_xx
      character(len=kchara), intent(inout) :: label_xy
      character(len=kchara), intent(inout) :: label_xz
      character(len=kchara), intent(inout) :: label_yy
      character(len=kchara), intent(inout) :: label_yz
      character(len=kchara), intent(inout) :: label_zz
!
!
      write(label_xx,'(a,a)') trim(field_label), trim(xx_label)
      write(label_xy,'(a,a)') trim(field_label), trim(xy_label)
      write(label_xz,'(a,a)') trim(field_label), trim(xz_label)
      write(label_yy,'(a,a)') trim(field_label), trim(yy_label)
      write(label_yz,'(a,a)') trim(field_label), trim(yz_label)
      write(label_zz,'(a,a)') trim(field_label), trim(zz_label)
!
      end subroutine add_tensor_direction_label_xyz
!
!-----------------------------------------------------------------------
!
      subroutine add_tensor_direction_label_rtp(field_label,            &
     &           label_rr, label_rt, label_rp, label_tt, label_tp,      &
     &           label_pp)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_rr
      character(len=kchara), intent(inout) :: label_rt
      character(len=kchara), intent(inout) :: label_rp
      character(len=kchara), intent(inout) :: label_tt
      character(len=kchara), intent(inout) :: label_tp
      character(len=kchara), intent(inout) :: label_pp
!
!
      write(label_rr,'(a,a)') trim(field_label), trim(rr_label)
      write(label_rt,'(a,a)') trim(field_label), trim(rt_label)
      write(label_rp,'(a,a)') trim(field_label), trim(rp_label)
      write(label_tt,'(a,a)') trim(field_label), trim(tt_label)
      write(label_tp,'(a,a)') trim(field_label), trim(tp_label)
      write(label_pp,'(a,a)') trim(field_label), trim(pp_label)
!
      end subroutine add_tensor_direction_label_rtp
!
!-----------------------------------------------------------------------
!
      subroutine add_tensor_direction_label_cyl(field_label,            &
     &           label_ss, label_sp, label_sz, label_pp, label_pz,      &
     &           label_zz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_ss
      character(len=kchara), intent(inout) :: label_sp
      character(len=kchara), intent(inout) :: label_sz
      character(len=kchara), intent(inout) :: label_pp
      character(len=kchara), intent(inout) :: label_pz
      character(len=kchara), intent(inout) :: label_zz
!
!
      write(label_ss,'(a,a)') trim(field_label), trim(ss_label)
      write(label_sp,'(a,a)') trim(field_label), trim(sp_label)
      write(label_sz,'(a,a)') trim(field_label), trim(sz_label)
      write(label_pp,'(a,a)') trim(field_label), trim(pp_label)
      write(label_pz,'(a,a)') trim(field_label), trim(pz_label)
      write(label_zz,'(a,a)') trim(field_label), trim(zz_label)
!
      end subroutine add_tensor_direction_label_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_asym_tensor_dir_label_xyz(field_label,             &
     &           label_xy, label_xz, label_yz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_xy
      character(len=kchara), intent(inout) :: label_xz
      character(len=kchara), intent(inout) :: label_yz
!
!
      write(label_xy,'(a,a)') trim(field_label), trim(xy_label)
      write(label_xz,'(a,a)') trim(field_label), trim(xz_label)
      write(label_yz,'(a,a)') trim(field_label), trim(yz_label)
!
      end subroutine add_asym_tensor_dir_label_xyz
!
!-----------------------------------------------------------------------
!
      subroutine add_asym_tensor_dir_label_rtp(field_label,             &
     &           label_rt, label_rp, label_tp)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_rt
      character(len=kchara), intent(inout) :: label_rp
      character(len=kchara), intent(inout) :: label_tp
!
!
      write(label_rt,'(a,a)') trim(field_label), trim(rt_label)
      write(label_rp,'(a,a)') trim(field_label), trim(rp_label)
      write(label_tp,'(a,a)') trim(field_label), trim(tp_label)
!
      end subroutine add_asym_tensor_dir_label_rtp
!
!-----------------------------------------------------------------------
!
      subroutine add_asym_tensor_dir_label_cyl(field_label,             &
     &           label_sp, label_sz, label_pz)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_sp
      character(len=kchara), intent(inout) :: label_sz
      character(len=kchara), intent(inout) :: label_pz
!
!
      write(label_sp,'(a,a)') trim(field_label), trim(sp_label)
      write(label_sz,'(a,a)') trim(field_label), trim(sz_label)
      write(label_pz,'(a,a)') trim(field_label), trim(pz_label)
!
      end subroutine add_asym_tensor_dir_label_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_vector_sph_spectr_label(field_label,               &
     &           label_pol, label_tor, label_dpol)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_pol
      character(len=kchara), intent(inout) :: label_tor
      character(len=kchara), intent(inout) :: label_dpol
!
!
      write(label_pol,'(a,a)') trim(field_label), trim(pol_label)
      write(label_tor,'(a,a)') trim(field_label), trim(tor_label)
      write(label_dpol,'(a,a)') trim(field_label), trim(dr_pol_label)
!
      end subroutine add_vector_sph_spectr_label
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_power_sph_label(field_label,                &
     &           label_pol, label_tor, label_tot)
!
      character(len=kchara), intent(in) :: field_label
      character(len=kchara), intent(inout) :: label_pol
      character(len=kchara), intent(inout) :: label_tor
      character(len=kchara), intent(inout) :: label_tot
!
!
      write(label_pol,'(a,a)') trim(field_label), trim(pol_label)
      write(label_tor,'(a,a)') trim(field_label), trim(tor_label)
      write(label_tot,'(a,a)') trim(field_label)
!
      end subroutine add_vector_power_sph_label
!
!-----------------------------------------------------------------------
!
      end module add_direction_labels
