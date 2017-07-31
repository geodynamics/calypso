!>@file   check_address_sph_trans.f90
!!@brief  module check_address_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine check_add_trans_sph_MHD                              &
!!     &         (ipol, idpdr, itor, b_trns, f_trns,                    &
!!     &          ncomp_rj_2_rtp, nvector_rj_2_rtp, nscalar_rj_2_rtp,   &
!!     &          ncomp_rtp_2_rj, nvector_rtp_2_rj, nscalar_rtp_2_rj)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: b_trns, f_trns
!!@endverbatim
!
      module check_address_sph_trans
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine check_add_trans_sph_MHD                                &
     &         (ipol, idpdr, itor, iphys, b_trns, f_trns,               &
     &          ncomp_rj_2_rtp, nvector_rj_2_rtp, nscalar_rj_2_rtp,     &
     &          ncomp_rtp_2_rj, nvector_rtp_2_rj, nscalar_rtp_2_rj)
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: b_trns, f_trns
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: nvector_rj_2_rtp
      integer(kind = kint), intent(in) :: nscalar_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      integer(kind = kint), intent(in) :: nvector_rtp_2_rj
      integer(kind = kint), intent(in) :: nscalar_rtp_2_rj
!
!
      write(*,*) 'transfer, poloidal, (toroidal, dP/dr), field'
!
      write(*,*) 'ncomp_rj_2_rtp  ', ncomp_rj_2_rtp
      write(*,*) 'ncomp_rtp_2_rj  ', ncomp_rtp_2_rj
!
      write(*,*) 'nvector_rj_2_rtp  ', nvector_rj_2_rtp
!
!  Vector fields
      if(b_trns%i_velo .gt. 0) write(*,*)                               &
     &        'b_trns%i_velo  ', b_trns%i_velo,                         &
     &        ipol%i_velo, itor%i_velo, idpdr%i_velo, iphys%i_velo
      if(b_trns%i_vort .gt. 0) write(*,*)                               &
     &        'b_trns%i_vort  ', b_trns%i_vort,                         &
     &        ipol%i_vort, itor%i_vort, idpdr%i_vort, iphys%i_vort
      if(b_trns%i_magne .gt. 0) write(*,*)                              &
     &        'b_trns%i_magne ', b_trns%i_magne,                        &
     &        ipol%i_magne, itor%i_magne, idpdr%i_magne, iphys%i_magne
      if(b_trns%i_current .gt. 0) write(*,*)                            &
     &        'b_trns%i_current ', b_trns%i_current, ipol%i_current,    &
     &        itor%i_current, idpdr%i_current, iphys%i_current
!
      if(b_trns%i_filter_velo .gt. 0) write(*,*)                        &
     &        'b_trns%i_filter_velo  ', b_trns%i_filter_velo,           &
     &        ipol%i_filter_velo, itor%i_filter_velo,                   &
     &        idpdr%i_filter_velo, iphys%i_filter_velo
      if(b_trns%i_filter_vort .gt. 0) write(*,*)                        &
     &        'b_trns%i_filter_vort  ', b_trns%i_filter_vort,           &
     &        ipol%i_filter_vort, itor%i_filter_vort,                   &
     &        idpdr%i_filter_vort, iphys%i_filter_vort
      if(b_trns%i_filter_magne .gt. 0) write(*,*)                       &
     &        'b_trns%i_filter_magne ', b_trns%i_filter_magne,          &
     &        ipol%i_filter_magne, itor%i_filter_magne,                 &
     &       idpdr%i_filter_magne, iphys%i_filter_magne
      if(b_trns%i_filter_current .gt. 0) write(*,*)                     &
     &        'b_trns%i_filter_current ', b_trns%i_filter_current,      &
     &        ipol%i_filter_current, itor%i_filter_current,             &
     &        idpdr%i_filter_current, iphys%i_filter_current
!
      if(b_trns%i_wide_fil_velo .gt. 0) write(*,*)                      &
     &        'b_trns%i_wide_fil_velo  ', b_trns%i_wide_fil_velo,       &
     &        ipol%i_wide_fil_velo, itor%i_wide_fil_velo,               &
     &        idpdr%i_wide_fil_velo, iphys%i_wide_fil_velo
      if(b_trns%i_wide_fil_vort .gt. 0) write(*,*)                      &
     &        'b_trns%i_wide_fil_vort  ', b_trns%i_wide_fil_vort,       &
     &        ipol%i_wide_fil_vort, itor%i_wide_fil_vort,               &
     &        idpdr%i_wide_fil_vort, iphys%i_wide_fil_vort
      if(b_trns%i_wide_fil_magne .gt. 0) write(*,*)                     &
     &        'b_trns%i_wide_fil_magne ', b_trns%i_wide_fil_magne,      &
     &        ipol%i_wide_fil_magne, itor%i_wide_fil_magne,             &
     &        idpdr%i_wide_fil_magne, iphys%i_wide_fil_magne
      if(b_trns%i_wide_fil_current .gt. 0) write(*,*)                   &
     &        'b_trns%i_wide_fil_current ', b_trns%i_wide_fil_current,  &
     &       ipol%i_wide_fil_current, itor%i_wide_fil_current,          &
     &        idpdr%i_wide_fil_current, iphys%i_wide_fil_current
!
!  Filetered forces
      if(b_trns%i_SGS_inertia .gt. 0) write(*,*)                        &
     &        'b_trns%i_SGS_inertia ', b_trns%i_SGS_inertia,            &
     &        ipol%i_SGS_inertia, itor%i_SGS_inertia,                   &
     &        idpdr%i_SGS_inertia, iphys%i_SGS_inertia
      if(b_trns%i_SGS_Lorentz .gt. 0) write(*,*)                        &
     &        'b_trns%i_SGS_Lorentz  ', b_trns%i_SGS_Lorentz,           &
     &        ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz,                   &
     &        idpdr%i_SGS_Lorentz, iphys%i_SGS_Lorentz
      if(b_trns%i_SGS_vp_induct .gt. 0) write(*,*)                      &
     &        'b_trns%i_SGS_vp_induct ', b_trns%i_SGS_vp_induct,        &
     &        ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,               &
     &        idpdr%i_SGS_vp_induct, iphys%i_SGS_vp_induct
      if(b_trns%i_SGS_h_flux .gt. 0) write(*,*) 'b_trns%i_SGS_h_flux',  &
     &        b_trns%i_SGS_h_flux, ipol%i_SGS_h_flux,                   &
     &        itor%i_SGS_h_flux, idpdr%i_SGS_h_flux, iphys%i_SGS_h_flux
      if(b_trns%i_SGS_c_flux .gt. 0) write(*,*) 'b_trns%i_SGS_c_flux',  &
     &        b_trns%i_SGS_c_flux, ipol%i_SGS_c_flux,                   &
     &        itor%i_SGS_c_flux, idpdr%i_SGS_c_flux, iphys%i_SGS_c_flux
!
      if(b_trns%i_geostrophic .gt. 0) write(*,*)                        &
     &        'b_trns%i_geostrophic', b_trns%i_geostrophic,             &
     &        ipol%i_geostrophic, itor%i_geostrophic,                   &
     &        idpdr%i_geostrophic, iphys%i_geostrophic
!
      if(b_trns%i_h_flux_w_sgs .gt. 0) write(*,*)                       &
     &        'b_trns%i_h_flux_w_sgs', b_trns%i_h_flux_w_sgs,           &
     &        ipol%i_h_flux_w_sgs, itor%i_h_flux_w_sgs,                 &
     &        idpdr%i_h_flux_w_sgs, iphys%i_h_flux_w_sgs
      if(b_trns%i_c_flux_w_sgs .gt. 0) write(*,*)                       &
     &        'b_trns%i_c_flux_w_sgs', b_trns%i_c_flux_w_sgs,           &
     &        ipol%i_c_flux_w_sgs, itor%i_c_flux_w_sgs,                 &
     &        idpdr%i_c_flux_w_sgs, iphys%i_c_flux_w_sgs
      if(b_trns%i_inertia_w_sgs .gt. 0) write(*,*)                      &
     &        'b_trns%i_inertia_w_sgs', b_trns%i_inertia_w_sgs,         &
     &        ipol%i_inertia_w_sgs, itor%i_inertia_w_sgs,               &
     &        idpdr%i_inertia_w_sgs, iphys%i_inertia_w_sgs
      if(b_trns%i_Lorentz_w_sgs .gt. 0) write(*,*)                      &
     &        'b_trns%i_Lorentz_w_sgs', b_trns%i_Lorentz_w_sgs,         &
     &        ipol%i_Lorentz_w_sgs,itor%i_Lorentz_w_sgs,                &
     &        idpdr%i_Lorentz_w_sgs, iphys%i_Lorentz_w_sgs
      if(b_trns%i_vp_induct_w_sgs .gt. 0) write(*,*)                    &
     &        'b_trns%i_vp_induct_w_sgs', b_trns%i_vp_induct_w_sgs,     &
     &        ipol%i_vp_induct_w_sgs, itor%i_vp_induct_w_sgs,           &
     &        idpdr%i_vp_induct_w_sgs, iphys%i_vp_induct_w_sgs
      if(b_trns%i_mag_induct_w_sgs .gt. 0) write(*,*)                   &
     &        'b_trns%i_mag_induct_w_sgs', b_trns%i_mag_induct_w_sgs,   &
     &        ipol%i_mag_induct_w_sgs, itor%i_mag_induct_w_sgs,         &
     &        idpdr%i_mag_induct_w_sgs, iphys%i_mag_induct_w_sgs
!
      if(b_trns%i_wide_SGS_inertia .gt. 0) write(*,*)                   &
     &        'b_trns%i_wide_SGS_inertia ', b_trns%i_wide_SGS_inertia,  &
     &        ipol%i_wide_SGS_inertia, itor%i_wide_SGS_inertia,         &
     &        idpdr%i_wide_SGS_inertia, iphys%i_wide_SGS_inertia
      if(b_trns%i_wide_SGS_Lorentz .gt. 0) write(*,*)                   &
     &        'b_trns%i_wide_SGS_Lorentz  ', b_trns%i_wide_SGS_Lorentz, &
     &        ipol%i_wide_SGS_Lorentz, itor%i_wide_SGS_Lorentz,         &
     &        idpdr%i_wide_SGS_Lorentz, iphys%i_wide_SGS_Lorentz
      if(b_trns%i_wide_SGS_vp_induct .gt. 0) write(*,*)                 &
     &        'b_trns%i_wide_SGS_vp_induct ',                           &
     &        b_trns%i_wide_SGS_vp_induct,                              &
     &        ipol%i_wide_SGS_vp_induct, itor%i_wide_SGS_vp_induct,     &
     &        idpdr%i_wide_SGS_vp_induct, iphys%i_wide_SGS_vp_induct
      if(b_trns%i_wide_SGS_h_flux .gt. 0) write(*,*)                    &
     &        'b_trns%i_wide_SGS_h_flux', b_trns%i_wide_SGS_h_flux,     &
     &        ipol%i_wide_SGS_h_flux, itor%i_wide_SGS_h_flux,           &
     &        idpdr%i_wide_SGS_h_flux, iphys%i_wide_SGS_h_flux
      if(b_trns%i_wide_SGS_c_flux .gt. 0) write(*,*)                    &
     &        'b_trns%i_wide_SGS_c_flux', b_trns%i_wide_SGS_c_flux,     &
     &        ipol%i_wide_SGS_c_flux, itor%i_wide_SGS_c_flux,           &
     &        idpdr%i_wide_SGS_c_flux, iphys%i_wide_SGS_c_flux
!
!   Snapshots
      if(b_trns%i_v_diffuse .gt. 0) write(*,*)                          &
     &        'b_trns%i_v_diffuse', b_trns%i_v_diffuse,                 &
     &        ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse,    &
     &        iphys%i_v_diffuse
      if(b_trns%i_w_diffuse .gt. 0) write(*,*)                          &
     &        'b_trns%i_w_diffuse', b_trns%i_w_diffuse,                 &
     &        ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse,    &
     &        iphys%i_w_diffuse
      if(b_trns%i_vp_diffuse .gt. 0) write(*,*)                         &
     &        'b_trns%i_vp_diffuse', b_trns%i_vp_diffuse,               &
     &        ipol%i_vp_diffuse, itor%i_vp_diffuse, idpdr%i_vp_diffuse, &
     &        iphys%i_vp_diffuse
      if(b_trns%i_b_diffuse .gt. 0) write(*,*)                          &
     &        'b_trns%i_b_diffuse', b_trns%i_b_diffuse,                 &
     &        ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse,    &
     &        iphys%i_b_diffuse
!
      if(b_trns%i_rot_inertia .gt. 0) write(*,*)                        &
     &        'b_trns%i_rot_inertia', b_trns%i_rot_inertia,             &
     &        ipol%i_rot_inertia, itor%i_rot_inertia,                   &
     &        idpdr%i_rot_inertia, iphys%i_rot_inertia
      if(b_trns%i_rot_Coriolis .gt. 0) write(*,*)                       &
     &        'b_trns%i_rot_Coriolis', b_trns%i_rot_Coriolis,           &
     &        ipol%i_rot_Coriolis, itor%i_rot_Coriolis,                 &
     &        idpdr%i_rot_Coriolis, iphys%i_rot_Coriolis
      if(b_trns%i_rot_Lorentz .gt. 0) write(*,*)                        &
     &        'b_trns%i_rot_Lorentz', b_trns%i_rot_Lorentz,             &
     &        ipol%i_rot_Lorentz, itor%i_rot_Lorentz,                   &
     &        idpdr%i_rot_Lorentz, iphys%i_rot_Lorentz
      if(b_trns%i_rot_buoyancy .gt. 0) write(*,*)                       &
     &        'b_trns%i_rot_buoyancy', b_trns%i_rot_buoyancy,           &
     &        ipol%i_rot_buoyancy, itor%i_rot_buoyancy,                 &
     &        idpdr%i_rot_buoyancy, iphys%i_rot_buoyancy
      if(b_trns%i_rot_comp_buo .gt. 0) write(*,*)                       &
     &        'b_trns%i_rot_comp_buo', b_trns%i_rot_comp_buo,           &
     &        ipol%i_rot_comp_buo, itor%i_rot_comp_buo,                 &
     &        idpdr%i_rot_comp_buo, iphys%i_rot_comp_buo
      if(b_trns%i_press_grad .gt. 0) write(*,*)                         &
     &        'b_trns%i_press_grad', b_trns%i_press_grad,               &
     &        ipol%i_press_grad, itor%i_press_grad,                     &
     &        idpdr%i_press_grad, iphys%i_press_grad
      if(b_trns%i_induction .gt. 0) write(*,*)                          &
     &        'b_trns%i_induction', b_trns%i_induction,                 &
     &        ipol%i_induction, itor%i_induction,                       &
     &        idpdr%i_induction, iphys%i_induction
      if(b_trns%i_grad_t .gt. 0) write(*,*) 'b_trns%i_grad_t',          &
     &        b_trns%i_grad_t, ipol%i_grad_t, itor%i_grad_t,            &
     &        idpdr%i_grad_t, iphys%i_grad_t
      if(b_trns%i_grad_composit .gt. 0) write(*,*)                      &
     &        'b_trns%i_grad_composit', b_trns%i_grad_composit,         &
     &        ipol%i_grad_composit, itor%i_grad_composit,               &
     &        idpdr%i_grad_composit, iphys%i_grad_composit
!
      if(b_trns%i_grad_vx .gt. 0) write(*,*) 'b_trns%i_grad_vx',        &
     &        b_trns%i_grad_vx, ipol%i_grad_vx, itor%i_grad_vx,         &
     &        idpdr%i_grad_vx, iphys%i_grad_vx
      if(b_trns%i_grad_vy .gt. 0) write(*,*) 'b_trns%i_grad_vy',        &
     &        b_trns%i_grad_vy, ipol%i_grad_vy, itor%i_grad_vy,         &
     &        idpdr%i_grad_vy, iphys%i_grad_vy
      if(b_trns%i_grad_vz .gt. 0) write(*,*) 'b_trns%i_grad_vz',        &
     &        b_trns%i_grad_vz, ipol%i_grad_vz, itor%i_grad_vz,         &
     &        idpdr%i_grad_vz, iphys%i_grad_vz
!
      if(b_trns%i_comp_buo .gt. 0) write(*,*)                           &
     &         'b_trns%i_comp_buo  ', b_trns%i_comp_buo,                &
     &         ipol%i_comp_buo, iphys%i_comp_buo
      if(b_trns%i_comp_buo .gt. 0) write(*,*)                           &
     &         'b_trns%i_comp_buo  ', b_trns%i_comp_buo,                &
     &         ipol%i_comp_buo, iphys%i_comp_buo
      write(*,*)
!
!
!  Scalars
      write(*,*) 'nscalar_rj_2_rtp  ', nscalar_rj_2_rtp
      if(b_trns%i_temp .gt. 0) write(*,*) 'b_trns%i_temp   ',           &
     &         b_trns%i_temp, ipol%i_temp, iphys%i_temp
      if(b_trns%i_light .gt. 0) write(*,*) 'b_trns%i_light  ',          &
     &         b_trns%i_light, ipol%i_light, iphys%i_light
      if(b_trns%i_press .gt. 0) write(*,*) 'b_trns%i_press   ',         &
     &         b_trns%i_press, ipol%i_press, iphys%i_press
      if(b_trns%i_par_temp .gt. 0) write(*,*) 'b_trns%i_par_temp   ',   &
     &         b_trns%i_par_temp, ipol%i_par_temp, iphys%i_par_temp
!
      if(b_trns%i_filter_temp .gt. 0) write(*,*)                        &
     &         'b_trns%i_filter_temp   ', b_trns%i_filter_temp,         &
     &         ipol%i_filter_temp, iphys%i_filter_temp
      if(b_trns%i_filter_comp .gt. 0) write(*,*)                        &
     &         'b_trns%i_filter_comp  ', b_trns%i_filter_comp,          &
     &         ipol%i_filter_comp, iphys%i_filter_comp
!
      if(b_trns%i_wide_fil_temp .gt. 0) write(*,*)                      &
     &         'b_trns%i_wide_fil_temp   ', b_trns%i_wide_fil_temp,     &
     &         ipol%i_wide_fil_temp, iphys%i_wide_fil_temp
      if(b_trns%i_wide_fil_comp .gt. 0) write(*,*)                      &
     &         'b_trns%i_wide_fil_comp  ', b_trns%i_wide_fil_comp,      &
     &         ipol%i_wide_fil_comp, iphys%i_wide_fil_comp
!
      if(b_trns%i_t_diffuse .gt. 0) write(*,*)                          &
     &         'b_trns%i_t_diffuse  ',                                  &
     &         b_trns%i_t_diffuse, ipol%i_t_diffuse, iphys%i_t_diffuse
      if(b_trns%i_c_diffuse .gt. 0) write(*,*)                          &
     &         'b_trns%i_c_diffuse  ',                                  &
     &         b_trns%i_c_diffuse, ipol%i_c_diffuse, iphys%i_c_diffuse
!
      if(b_trns%i_h_advect .gt. 0) write(*,*)                           &
     &         'b_trns%i_h_advect  ', b_trns%i_h_advect,                &
     &         ipol%i_h_advect, iphys%i_h_advect
      if(b_trns%i_c_advect .gt. 0) write(*,*)                           &
     &         'b_trns%i_c_advect  ', b_trns%i_c_advect,                &
     &         ipol%i_c_advect, iphys%i_c_advect
!
      if(b_trns%i_buoyancy .gt. 0) write(*,*)                           &
     &         'b_trns%i_buoyancy  ', b_trns%i_buoyancy,                &
     &         ipol%i_buoyancy, iphys%i_buoyancy
!
!
      if(b_trns%i_Csim_SGS_h_flux .gt. 0) write(*,*)                    &
     &       'b_trns%i_Csim_SGS_h_flux  ', b_trns%i_Csim_SGS_h_flux,    &
     &       ipol%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux
      if(b_trns%i_Csim_SGS_c_flux .gt. 0) write(*,*)                    &
     &       'b_trns%i_Csim_SGS_c_flux  ', b_trns%i_Csim_SGS_c_flux,    &
     &       ipol%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux
      if(b_trns%i_Csim_SGS_m_flux .gt. 0) write(*,*)                    &
     &       'b_trns%i_Csim_SGS_m_flux  ', b_trns%i_Csim_SGS_m_flux,    &
     &       ipol%i_Csim_SGS_m_flux, iphys%i_Csim_SGS_m_flux
      if(b_trns%i_Csim_SGS_Lorentz .gt. 0) write(*,*)                   &
     &       'b_trns%i_Csim_SGS_Lorentz  ', b_trns%i_Csim_SGS_Lorentz,  &
     &       ipol%i_Csim_SGS_Lorentz, iphys%i_Csim_SGS_Lorentz
      if(b_trns%i_Csim_SGS_induction .gt. 0) write(*,*)                 &
     &       'b_trns%i_Csim_SGS_induction  ',                           &
     &       b_trns%i_Csim_SGS_induction, ipol%i_Csim_SGS_induction,    &
     &       iphys%i_Csim_SGS_induction
      if(b_trns%i_Csim_SGS_buoyancy .gt. 0) write(*,*)                  &
     &       'b_trns%i_Csim_SGS_buoyancy  ',                            &
     &       b_trns%i_Csim_SGS_buoyancy, ipol%i_Csim_SGS_buoyancy,      &
     &       iphys%i_Csim_SGS_buoyancy
      if(b_trns%i_Csim_SGS_comp_buo .gt. 0) write(*,*)                  &
     &       'b_trns%i_Csim_SGS_comp_buo  ',                            &
     &       b_trns%i_Csim_SGS_comp_buo, ipol%i_Csim_SGS_comp_buo,      &
     &       iphys%i_Csim_SGS_comp_buo
!
!   Snapshots
      if(b_trns%i_wide_fil_comp .gt. 0) write(*,*)                      &
     &         'b_trns%i_wide_fil_comp  ', b_trns%i_wide_fil_comp,      &
     &         ipol%i_wide_fil_comp, iphys%i_wide_fil_comp
      if(b_trns%i_wide_fil_comp .gt. 0) write(*,*)                      &
     &         'b_trns%i_wide_fil_comp  ', b_trns%i_wide_fil_comp,      &
     &         ipol%i_wide_fil_comp, iphys%i_wide_fil_comp
      if(b_trns%i_wide_fil_comp .gt. 0) write(*,*)                      &
     &         'b_trns%i_wide_fil_comp  ', b_trns%i_wide_fil_comp,      &
     &         ipol%i_wide_fil_comp, iphys%i_wide_fil_comp
      if(b_trns%i_wide_fil_comp .gt. 0) write(*,*)                      &
     &         'b_trns%i_wide_fil_comp  ', b_trns%i_wide_fil_comp,      &
     &         ipol%i_wide_fil_comp, iphys%i_wide_fil_comp
      write(*,*)
!
      write(*,*) 'nvector_rtp_2_rj  ', nvector_rtp_2_rj
!   Forces
      if(f_trns%i_m_advect .gt. 0) write(*,*) 'f_trns%i_m_advect ',     &
     &        f_trns%i_m_advect, ipol%i_m_advect,                       &
     &        itor%i_m_advect, idpdr%i_m_advect, iphys%i_m_advect
      if(f_trns%i_coriolis .gt. 0) write(*,*) 'f_trns%i_coriolis  ',    &
     &        f_trns%i_coriolis, ipol%i_coriolis,                       &
     &        itor%i_coriolis, idpdr%i_coriolis, iphys%i_coriolis
      if(f_trns%i_rot_Coriolis .gt. 0) write(*,*)                       &
     &       'f_trns%i_rot_Coriolis  ', f_trns%i_rot_Coriolis,          &
     &        ipol%i_rot_Coriolis, itor%i_rot_Coriolis,                 &
     &        idpdr%i_rot_Coriolis, iphys%i_rot_Coriolis
      if(f_trns%i_lorentz .gt. 0) write(*,*) 'f_trns%i_lorentz  ',      &
     &        f_trns%i_lorentz, ipol%i_lorentz,                         &
     &        itor%i_lorentz, idpdr%i_lorentz, iphys%i_lorentz
      if(f_trns%i_vp_induct .gt. 0) write(*,*) 'f_trns%i_vp_induct ',   &
     &        f_trns%i_vp_induct, ipol%i_vp_induct,                     &
     &        itor%i_vp_induct, idpdr%i_vp_induct, iphys%i_vp_induct
      if(f_trns%i_h_flux .gt. 0) write(*,*) 'f_trns%i_h_flux',          &
     &        f_trns%i_h_flux, ipol%i_h_flux,                           &
     &        itor%i_h_flux, idpdr%i_h_flux, iphys%i_h_flux
      if(f_trns%i_c_flux .gt. 0) write(*,*) 'f_trns%i_c_flux',          &
     &        f_trns%i_c_flux, ipol%i_c_flux,                           &
     &        itor%i_c_flux, idpdr%i_c_flux, iphys%i_c_flux
!
      if(f_trns%i_electric .gt. 0) write(*,*) 'f_trns%i_electric',      &
     &        f_trns%i_electric, ipol%i_electric,                       &
     &        itor%i_electric, idpdr%i_electric, iphys%i_electric
      if(f_trns%i_poynting .gt. 0) write(*,*) 'f_trns%i_poynting',      &
     &        f_trns%i_poynting, ipol%i_poynting,                       &
     &        itor%i_poynting, idpdr%i_poynting, iphys%i_poynting
      if(f_trns%i_mag_stretch .gt. 0) write(*,*)                        &
     &        'f_trns%i_mag_stretch', f_trns%i_mag_stretch,             &
     &        ipol%i_mag_stretch, itor%i_mag_stretch,                   &
     &        idpdr%i_mag_stretch, iphys%i_mag_stretch
!
!  SGS terms
      if(f_trns%i_SGS_inertia .gt. 0) write(*,*)                        &
     &        'f_trns%i_SGS_inertia ', f_trns%i_SGS_inertia,            &
     &        ipol%i_SGS_inertia, itor%i_SGS_inertia,                   &
     &        idpdr%i_SGS_inertia, iphys%i_SGS_inertia
      if(f_trns%i_SGS_Lorentz .gt. 0) write(*,*)                        &
     &        'f_trns%i_SGS_Lorentz  ', f_trns%i_SGS_Lorentz,           &
     &        ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz,                   &
     &        idpdr%i_SGS_Lorentz, iphys%i_SGS_Lorentz
      if(f_trns%i_SGS_vp_induct .gt. 0) write(*,*)                      &
     &        'f_trns%i_SGS_vp_induct ', f_trns%i_SGS_vp_induct,        &
     &        ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,               &
     &        idpdr%i_SGS_vp_induct, iphys%i_SGS_vp_induct
      if(f_trns%i_SGS_h_flux .gt. 0) write(*,*) 'f_trns%i_SGS_h_flux',  &
     &        f_trns%i_SGS_h_flux, ipol%i_SGS_h_flux,                   &
     &        itor%i_SGS_h_flux,idpdr%i_SGS_h_flux, iphys%i_SGS_h_flux
      if(f_trns%i_SGS_c_flux .gt. 0) write(*,*) 'f_trns%i_SGS_c_flux',  &
     &        f_trns%i_SGS_c_flux, ipol%i_SGS_c_flux,                   &
     &        itor%i_SGS_c_flux, idpdr%i_SGS_c_flux, iphys%i_SGS_c_flux
!
      if(f_trns%i_grad_vx .gt. 0) write(*,*) 'f_trns%i_grad_vx',        &
     &        f_trns%i_grad_vx, ipol%i_grad_vx,                         &
     &        itor%i_grad_vx, idpdr%i_grad_vx, iphys%i_grad_vx
      if(f_trns%i_grad_vy .gt. 0) write(*,*) 'f_trns%i_grad_vy',        &
     &        f_trns%i_grad_vy, ipol%i_grad_vy,                         &
     &        itor%i_grad_vy, idpdr%i_grad_vy, iphys%i_grad_vy
      if(f_trns%i_grad_vz .gt. 0) write(*,*) 'f_trns%i_grad_vz',        &
     &        f_trns%i_grad_vz, ipol%i_grad_vz,                         &
     &        itor%i_grad_vz, idpdr%i_grad_vz, iphys%i_grad_vz
!
      write(*,*) 'nscalar_rtp_2_rj  ', nscalar_rtp_2_rj
      if(f_trns%i_me_gen .gt. 0) write(*,*) 'f_trns%i_me_gen  ',        &
     &        f_trns%i_me_gen, ipol%i_me_gen, iphys%i_me_gen
      if(f_trns%i_ujb .gt. 0) write(*,*) 'f_trns%i_ujb  ',              &
     &        f_trns%i_ujb, ipol%i_ujb, iphys%i_ujb
      if(f_trns%i_nega_ujb .gt. 0) write(*,*) 'f_trns%i_nega_ujb  ',    &
     &        f_trns%i_nega_ujb, ipol%i_nega_ujb, iphys%i_nega_ujb
!
      if(f_trns%i_buo_gen .gt. 0) write(*,*) 'f_trns%i_buo_gen  ',      &
     &        f_trns%i_buo_gen, ipol%i_buo_gen, iphys%i_buo_gen
      if(f_trns%i_c_buo_gen .gt. 0) write(*,*) 'f_trns%i_c_buo_gen  ',  &
     &        f_trns%i_c_buo_gen, ipol%i_c_buo_gen, iphys%i_c_buo_gen
      if(f_trns%i_f_buo_gen .gt. 0) write(*,*) 'f_trns%i_f_buo_gen  ',  &
     &        f_trns%i_f_buo_gen, ipol%i_f_buo_gen, iphys%i_f_buo_gen
!
      if(f_trns%i_reynolds_wk .gt. 0) write(*,*)                        &
     &       'f_trns%i_reynolds_wk  ', f_trns%i_reynolds_wk,            &
     &        ipol%i_reynolds_wk, iphys%i_reynolds_wk
      if(f_trns%i_SGS_Lor_wk .gt. 0) write(*,*)                         &
     &       'f_trns%i_SGS_Lor_wk  ', f_trns%i_SGS_Lor_wk,              &
     &        ipol%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk
      if(f_trns%i_SGS_me_gen .gt. 0) write(*,*)                         &
     &       'f_trns%i_SGS_me_gen  ', f_trns%i_SGS_me_gen,              &
     &        ipol%i_SGS_me_gen, iphys%i_SGS_me_gen
!
      if(f_trns%i_SGS_buo_wk .gt. 0) write(*,*)                         &
     &       'f_trns%i_SGS_buo_wk  ', f_trns%i_SGS_buo_wk,              &
     &        ipol%i_SGS_buo_wk, iphys%i_SGS_buo_wk
      if(f_trns%i_SGS_comp_buo_wk .gt. 0) write(*,*)                    &
     &       'f_trns%i_SGS_comp_buo_wk  ', f_trns%i_SGS_comp_buo_wk,    &
     &        ipol%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk
!
      if(f_trns%i_Csim_SGS_h_flux .gt. 0) write(*,*)                    &
     &       'f_trns%i_Csim_SGS_h_flux  ', f_trns%i_Csim_SGS_h_flux,    &
     &       ipol%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux
      if(f_trns%i_Csim_SGS_c_flux .gt. 0) write(*,*)                    &
     &       'f_trns%i_Csim_SGS_c_flux  ', f_trns%i_Csim_SGS_c_flux,    &
     &       ipol%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux
      if(f_trns%i_Csim_SGS_m_flux .gt. 0) write(*,*)                    &
     &       'f_trns%i_Csim_SGS_m_flux  ', f_trns%i_Csim_SGS_m_flux,    &
     &       ipol%i_Csim_SGS_m_flux, iphys%i_Csim_SGS_m_flux
      if(f_trns%i_Csim_SGS_Lorentz .gt. 0) write(*,*)                   &
     &       'f_trns%i_Csim_SGS_Lorentz  ', f_trns%i_Csim_SGS_Lorentz,  &
     &       ipol%i_Csim_SGS_Lorentz, iphys%i_Csim_SGS_Lorentz
      if(f_trns%i_Csim_SGS_induction .gt. 0) write(*,*)                 &
     &       'f_trns%i_Csim_SGS_induction  ',                           &
     &       f_trns%i_Csim_SGS_induction, ipol%i_Csim_SGS_induction,    &
     &       iphys%i_Csim_SGS_induction
      if(f_trns%i_Csim_SGS_buoyancy .gt. 0) write(*,*)                  &
     &       'f_trns%i_Csim_SGS_buoyancy  ',                            &
     &       f_trns%i_Csim_SGS_buoyancy, ipol%i_Csim_SGS_buoyancy,      &
     &       iphys%i_Csim_SGS_buoyancy
      if(f_trns%i_Csim_SGS_comp_buo .gt. 0) write(*,*)                  &
     &       'f_trns%i_Csim_SGS_comp_buo  ',                            &
     &       f_trns%i_Csim_SGS_comp_buo, ipol%i_Csim_SGS_comp_buo,      &
     &       iphys%i_Csim_SGS_comp_buo
!
      if(f_trns%i_velo_scale .gt. 0) write(*,*)                         &
     &       'f_trns%i_velo_scale  ', f_trns%i_velo_scale,              &
     &        ipol%i_velo_scale, iphys%i_velo_scale
      if(f_trns%i_magne_scale .gt. 0) write(*,*)                        &
     &       'f_trns%i_magne_scale  ', f_trns%i_magne_scale,            &
     &        ipol%i_magne_scale, iphys%i_magne_scale
      if(f_trns%i_temp_scale .gt. 0) write(*,*)                         &
     &       'f_trns%i_temp_scale  ', f_trns%i_temp_scale,              &
     &        ipol%i_temp_scale, iphys%i_temp_scale
      if(f_trns%i_comp_scale .gt. 0) write(*,*)                         &
     &       'f_trns%i_comp_scale  ', f_trns%i_comp_scale,              &
     &        ipol%i_comp_scale, iphys%i_comp_scale
!
      if(f_trns%i_div_Coriolis .gt. 0) write(*,*)                       &
     &       'f_trns%i_div_Coriolis  ', f_trns%i_div_Coriolis,          &
     &        ipol%i_div_Coriolis, iphys%i_div_Coriolis
      write(*,*)
!
      end subroutine check_add_trans_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module check_address_sph_trans
