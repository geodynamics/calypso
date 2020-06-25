!>@file   explicit_scalars_sph.f90
!!@brief  module explicit_scalars_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine explicit_scalars_sph_adams                           &
!!     &         (dt, sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,     &
!!     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, rj_fld)
!!      subroutine explicit_scalars_sph_euler                           &
!!     &         (dt, sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,     &
!!     &          ipol_base, ipol_frc, ipol_dif, rj_fld)
!!      subroutine first_scalars_prev_step_adams                        &
!!     &         (sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,         &
!!     &          ipol_base, ipol_exp, ipol_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(diffusion_address), intent(in) :: ipol_dif
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module explicit_scalars_sph
!
      use m_precision
!
      use t_spheric_rj_data
      use t_boundary_data_sph_MHD
      use t_physical_property
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
      use t_phys_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine explicit_scalars_sph_adams                             &
     &         (dt, sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,       &
     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, rj_fld)
!
      use select_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scalar_diff_adv_src_adams temperature'
        call sel_scalar_diff_adv_src_adams                              &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                  &
     &      ipol_base%i_heat_source, ipol_base%i_temp,                  &
     &      ipol_exp%i_pre_heat, dt, ht_prop%coef_exp,                  &
     &      ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scalar_diff_adv_src_adams composition'
        call sel_scalar_diff_adv_src_adams                              &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out,                            &
     &      ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                  &
     &      ipol_base%i_light_source, ipol_base%i_light,                &
     &      ipol_exp%i_pre_composit, dt, cp_prop%coef_exp,              &
     &      cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
!  Center evolution
!
      if(sph_rj%inod_rj_center .eq. 0) return
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_diff_adv_src_adams temperature'
        call sel_ctr_scl_diff_adv_src_adams                             &
     &     (ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                  &
     &      ipol_base%i_heat_source, ipol_base%i_temp,                  &
     &      ipol_exp%i_pre_heat, dt, ht_prop%coef_exp,                  &
     &      ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_ctr_scl_diff_adv_src_adams composition'
        call sel_ctr_scl_diff_adv_src_adams(ipol_dif%i_c_diffuse,       &
     &      ipol_frc%i_c_advect, ipol_base%i_light_source,              &
     &      ipol_base%i_light, ipol_exp%i_pre_composit,                 &
     &      dt, cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine explicit_scalars_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine explicit_scalars_sph_euler                             &
     &         (dt, sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,       &
     &          ipol_base, ipol_frc, ipol_dif, rj_fld)
!
      use select_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scalar_diff_adv_src_euler temperature'
        call sel_scalar_diff_adv_src_euler                              &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                  &
     &      ipol_base%i_heat_source, ipol_base%i_temp,                  &
     &      dt, ht_prop%coef_exp, ht_prop%coef_advect,                  &
     &      ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scalar_diff_adv_src_euler composition'
        call sel_scalar_diff_adv_src_euler                              &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out,                            &
     &      ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                  &
     &      ipol_base%i_light_source, ipol_base%i_light,                &
     &      dt, cp_prop%coef_exp, cp_prop%coef_advect,                  &
     &      cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
!   Center evolution
!
      if(sph_rj%inod_rj_center .eq. 0) return
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_diff_adv_src_euler temperature'
        call sel_ctr_scl_diff_adv_src_euler(ipol_dif%i_t_diffuse,       &
     &     ipol_frc%i_h_advect, ipol_base%i_heat_source,                &
     &     ipol_base%i_temp, dt, ht_prop%coef_exp, ht_prop%coef_advect, &
     &     ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_diff_adv_src_euler composition'
        call sel_ctr_scl_diff_adv_src_euler                             &
     &     (ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                  &
     &      ipol_base%i_light_source, ipol_base%i_light, dt,            &
     &      cp_prop%coef_exp, cp_prop%coef_advect, cp_prop%coef_source, &
     &      sph_rj, rj_fld)
      end if
!
      end subroutine explicit_scalars_sph_euler
!
! ----------------------------------------------------------------------
!
      subroutine first_scalars_prev_step_adams                          &
     &         (sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_exp, ipol_frc, rj_fld)
!
      use select_diff_adv_source
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ini_adams_scalar_w_src                                 &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out, ipol_frc%i_h_advect,       &
     &      ipol_base%i_heat_source, ipol_exp%i_pre_heat,               &
     &      ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ini_adams_scalar_w_src                                 &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out, ipol_frc%i_c_advect,       &
     &      ipol_base%i_light_source, ipol_exp%i_pre_composit,          &
     &      cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
!   Center evolution
!
      if(sph_rj%inod_rj_center .eq. 0) return
      if(ht_prop%iflag_scheme .gt.     id_no_evolution                  &
      &  .and. ipol_base%i_heat_source .gt. izero) then
        call center_ini_adams_scalar_w_src                              &
     &     (sph_rj%inod_rj_center, ipol_frc%i_h_advect,                 &
     &      ipol_base%i_heat_source, ipol_exp%i_pre_heat,               &
     &      ht_prop%coef_source, rj_fld%n_point, rj_fld%ntot_phys,      &
     &      rj_fld%d_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution                      &
     &  .and. ipol_base%i_light_source .gt. izero) then
        call center_ini_adams_scalar_w_src                              &
     &     (sph_rj%inod_rj_center, ipol_frc%i_c_advect,                 &
     &      ipol_base%i_light_source, ipol_exp%i_pre_composit,          &
     &      cp_prop%coef_source, rj_fld%n_point, rj_fld%ntot_phys,      &
     &      rj_fld%d_fld)
      end if
!
      end subroutine first_scalars_prev_step_adams
!
! ----------------------------------------------------------------------
!
      end module explicit_scalars_sph
