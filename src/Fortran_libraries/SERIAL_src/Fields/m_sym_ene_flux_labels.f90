!>@file   m_sym_ene_flux_labels.f90
!!        module m_sym_ene_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for energy fluxes by sym field
!!
!!@verbatim
!!      logical function check_filter_enegy_fluxes(field_name)
!!
!!      subroutine set_sym_ene_flux_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  List of energy flux by SGS terms  !!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   mns_ua_d_ws_x_us, mns_ua_d_wa_x_ua,
!!   mns_us_d_ws_x_ua, mns_us_d_wa_x_us: [eflux%i_m_advect_work]
!!          Work of Reynolds stress   -u \cdot (\omega \times u)
!!   ua_d_ws_x_us, ua_d_wa_x_ua,
!!   us_d_ws_x_ua, us_d_wa_x_us:         [eflux%i_nega_m_advect_wk]
!!          Work against Reynolds stress   u \cdot (\omega \times u)
!!
!!   ua_d_js_x_bs, ua_d_ja_x_ba,
!!   us_d_js_x_ba, us_d_ja_x_bs:         [eflux%i_ujb]
!!          Work of Lorentz force      u \cdot (J \times B)
!!   mns_us_d_js_x_ba, mns_us_d_ja_x_bs,
!!   mns_ua_d_js_x_bs, mns_ua_d_ja_x_ba: [eflux%i_nega_ujb]
!!          Work against Lorentz force       -u \cdot (J \times B)
!!
!!   mag_tension_work_sym      [eflux_by_filter%i_m_tension_wk]
!!
!!   sym_thermal_buoyancy_flux,
!!   asym_thermal_buoyancy_flux:    [eflux_by_filter%i_t_buo_gen]
!!          Thermal buoyancy flux         -u \cdot g \alpha_{T} T
!!   sym_composite_buoyancy_flux,
!!   asym_composite_buoyancy_flux:  [eflux_by_filter%i_c_buo_gen]
!!      Compositional buoyancy flux  -u \cdot g \alpha_{C} C
!!   sym_buoyancy_flux,
!!   asym_buoyancy_flux:            [eflux_by_filter%i_t_buo_gen]
!!      Thermal buoyancy flux  -u \cdot g (\alpha_{T} T + \alpha_{C} C)
!!
!!   mag_ene_generation_sym    [eflux_by_filter%i_me_gen]
!!   mag_stretch_flux_sym
!!                              [eflux_by_filter%i_mag_stretch_flux]
!!
!!   temp_generation_sym       [eflux_by_filter%i_temp_gen]
!!   part_temp_gen_sym         [eflux_by_filter%i_par_t_gen]
!!   comp_generation_sym       [eflux_by_filter%i_comp_gen]
!!   part_comp_gen_sym         [eflux_by_filter%i_par_c_gen]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
        module m_sym_ene_flux_labels
!
        use m_precision
        use m_phys_constants
        use t_field_labels
!
        implicit  none
!
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
      type(field_def), parameter :: mns_us_d_ws_x_ua                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = '-us_d_ws_x_ua',                           &
     &                math = '$ u_{i} (e_{ijk}'                         &
     &                    //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
      type(field_def), parameter :: mns_us_d_wa_x_us                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = '-us_d_wa_x_us',                           &
     &                math = '$-u_{i} (e_{ijk}'                         &
     &                    //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
      type(field_def), parameter :: mns_ua_d_ws_x_us                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = '-ua_d_ws_x_us',                           &
     &                math = '$-u_{i} (e_{ijk}'                         &
     &                    //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
      type(field_def), parameter :: mns_ua_d_wa_x_ua                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = '-ua_d_wa_x_ua',                           &
     &                math = '$-u_{i} (e_{ijk}'                         &
     &                    //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!
!>        Field label of work of Lorentz force
!!         @f$u_{symi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{symk}) @f$
      type(field_def), parameter :: us_d_ja_x_bs                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'us_d_ja_x_bs',                            &
     &                math = '$ u_{symi} (e_{ijk}'                      &
     &                  //' \tilde{J}_{asymj} \tilde{B}_{symk})$')
!>        Field label of work of Lorentz force
!!         @f$u_{asymi} (e_{ijk} \tilde{J}_{symj} \tilde{B}_{symk}) @f$
      type(field_def), parameter :: ua_d_js_x_bs                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'ua_d_js_x_bs',                            &
     &                math = '$ u_{asymi} (e_{ijk}'                     &
     &                  // ' \tilde{J}_{symj} \tilde{B}_{symk})$')
!>        Field label of work of Lorentz force
!!         @f$u_{asymi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{asymk}) @f$
      type(field_def), parameter :: ua_d_ja_x_ba                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'ua_d_ja_x_ba',                            &
     &                math = '$ u_{asymi} (e_{ijk}'                     &
     &                  // ' \tilde{J}_{asymj} \tilde{B}_{asymk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{symi} (e_{ijk} \tilde{J}_{symj} \tilde{B}_{asymk}) @f$
      type(field_def), parameter :: us_d_js_x_ba                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'us_d_js_x_ba',                            &
     &                math = '$-u_{symi} (e_{ijk}'                      &
     &                  // '  \tilde{J}_{symj} \tilde{B}_{asymk})$')
!
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: mns_ua_d_js_x_bs                    &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = '-ua_d_js_x_bs',                               &
     &            math = '$ -u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: mns_ua_d_ja_x_ba                    &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = '-ua_d_ja_x_ba',                               &
     &            math = '$ -u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: mns_us_d_js_x_ba                    &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = '-us_d_js_x_ba',                               &
     &            math = '$ -u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: mns_us_d_ja_x_bs                    &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = '-us_d_ja_x_bs',                               &
     &            math = '$ -u \cdot (J_{asym} \times B_{sym}) $')
!
!>        Field label of work against inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{sym}) @f$
      type(field_def), parameter :: ua_d_ws_x_us                        &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'ua_d_ws_x_us',                                   &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{symi})$, '   &
     &             //' $ u \cdot (\omega_{sym} \times u_{sym})$')
!>        Field label of work against inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{asym}) @f$
      type(field_def), parameter :: ua_d_wa_x_ua                        &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'ua_d_wa_x_ua',                                   &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{asymi})$, ' &
     &             //' $ u \cdot (\omega_{asym} \times u_{asym})$')
!>        Field label of work against inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{asym}) @f$
      type(field_def), parameter :: us_d_ws_x_ua                        &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'us_d_ws_x_ua',                                   &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{asymi})$, '  &
     &             //' $ u \cdot (\omega_{sym} \times u_{asym})$')
!
!>        Field label of work against inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{sym}) @f$
      type(field_def), parameter :: us_d_wa_x_us                        &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'us_d_wa_x_us',                                   &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{symi})$, '  &
     &             //' $ u \cdot (\omega_{asym} \times u_{sym})$')
!
!>        Field label for sym thermal buoyancy flux
!!         @f$ -u_{symi} \alpha_{T} \tilde{T}_{sym} g_{i} @f$
      type(field_def), parameter :: sym_thermal_buoyancy_flux           &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'sym_thermal_buoyancy_flux',                      &
     &         math = '$-u_{symi} \alpha_{T} \tilde{T}_{sym} g_{i} $')
!>        Field label for asym thermal buoyancy flux
!!         @f$ -u_{asymi} \alpha_{T} \tilde{T}_{asym} g_{i} @f$
      type(field_def), parameter :: asym_thermal_buoyancy_flux          &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'asym_thermal_buoyancy_flux',                     &
     &         math = '$-u_{asymi} \alpha_{T} \tilde{T}_{asym} g_{i}$')
!>        Field label for sym compositional buoyancy flux
!!         @f$ -u_{symi} \alpha_{T} \tilde{T}_{sym} g_{i} @f$
      type(field_def), parameter :: sym_composite_buoyancy_flux         &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'sym_composite_buoyancy_flux',                    &
     &         math = '$-u_{symi} \alpha_{C} \tilde{C}_{sym} g_{i}$')
!>        Field label for asym compositional buoyancy flux
!!         @f$ -u_{asymi} \alpha_{T} \tilde{T}_{asym} g_{i} @f$
      type(field_def), parameter :: asym_composite_buoyancy_flux        &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'asym_composite_buoyancy_flux',            &
     &         math = '$-u_{asymi} \alpha_{C} \tilde{C}_{asym} g_{i}$')
!>        Field label for sym total buoyancy flux
!!         @f$ -u_{symi} \alpha_{T} \tilde{T}_{sym} g_{i} @f$
      type(field_def), parameter :: sym_buoyancy_flux                   &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'sym_buoyancy_flux',                              &
     &         math = '$ -u_{symi} (\alpha_{T} \tilde{T}_{sym} '        &
     &             // ' + \alpha_{C} \tilde{C}_{sym}) g_{i} $')
!>        Field label for asym total buoyancy flux
!!         @f$ -u_{asymi} \alpha_{T} \tilde{T}_{asym} g_{i} @f$
      type(field_def), parameter :: asym_buoyancy_flux                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'asym_buoyancy_flux',                             &
     &         math = '$ -u_{asymi} (\alpha_{T} \tilde{T}_{asym} '      &
     &             // ' + \alpha_{C} \tilde{C}_{asym}) g_{i} $')
!
! ----------------------------------------------------------------------
!
        contains
!
! ----------------------------------------------------------------------
!
      logical function check_enegy_fluxes_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_enegy_fluxes_w_sym                                          &
     &   =    (field_name .eq. mns_us_d_ws_x_ua%name)                   &
     &   .or. (field_name .eq. mns_us_d_wa_x_us%name)                   &
     &   .or. (field_name .eq. mns_ua_d_ws_x_us%name)                   &
     &   .or. (field_name .eq. mns_ua_d_wa_x_ua%name)                   &
     &   .or. (field_name .eq. us_d_js_x_ba%name)                       &
     &   .or. (field_name .eq. us_d_ja_x_bs%name)                       &
     &   .or. (field_name .eq. ua_d_js_x_bs%name)                       &
     &   .or. (field_name .eq. ua_d_ja_x_ba%name)                       &
     &   .or. (field_name .eq. mns_ua_d_js_x_bs%name)                   &
     &   .or. (field_name .eq. mns_ua_d_ja_x_ba%name)                   &
     &   .or. (field_name .eq. mns_us_d_js_x_ba%name)                   &
     &   .or. (field_name .eq. mns_us_d_ja_x_bs%name)                   &
     &   .or. (field_name .eq. ua_d_ws_x_us%name)                       &
     &   .or. (field_name .eq. ua_d_wa_x_ua%name)                       &
     &   .or. (field_name .eq. us_d_ws_x_ua%name)                       &
     &   .or. (field_name .eq. us_d_wa_x_us%name)                       &
!
     &   .or. (field_name .eq. sym_thermal_buoyancy_flux%name)          &
     &   .or. (field_name .eq. asym_thermal_buoyancy_flux%name)         &
     &   .or. (field_name .eq. sym_composite_buoyancy_flux%name)        &
     &   .or. (field_name .eq. asym_composite_buoyancy_flux%name)       &
     &   .or. (field_name .eq. sym_buoyancy_flux%name)                  &
     &   .or. (field_name .eq. asym_buoyancy_flux%name)
!
      end function check_enegy_fluxes_w_sym
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_sym_ene_flux_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(mns_us_d_ws_x_ua,   array_c2i)
      call set_field_label_to_ctl(mns_us_d_wa_x_us,   array_c2i)
      call set_field_label_to_ctl(mns_ua_d_ws_x_us,   array_c2i)
      call set_field_label_to_ctl(mns_ua_d_wa_x_ua,   array_c2i)
!
      call set_field_label_to_ctl(us_d_js_x_ba,       array_c2i)
      call set_field_label_to_ctl(us_d_ja_x_bs,       array_c2i)
      call set_field_label_to_ctl(ua_d_js_x_bs,       array_c2i)
      call set_field_label_to_ctl(ua_d_ja_x_ba,       array_c2i)
!
      call set_field_label_to_ctl(mns_ua_d_js_x_bs,  array_c2i)
      call set_field_label_to_ctl(mns_ua_d_ja_x_ba,  array_c2i)
      call set_field_label_to_ctl(mns_us_d_js_x_ba,  array_c2i)
      call set_field_label_to_ctl(mns_us_d_ja_x_bs,  array_c2i)
!
      call set_field_label_to_ctl(ua_d_ws_x_us,      array_c2i)
      call set_field_label_to_ctl(ua_d_wa_x_ua,      array_c2i)
      call set_field_label_to_ctl(us_d_ws_x_ua,      array_c2i)
      call set_field_label_to_ctl(us_d_wa_x_us,      array_c2i)
!
      call set_field_label_to_ctl(sym_thermal_buoyancy_flux,            &
     &                            array_c2i)
      call set_field_label_to_ctl(asym_thermal_buoyancy_flux,           &
     &                            array_c2i)
      call set_field_label_to_ctl(sym_composite_buoyancy_flux,          &
     &                            array_c2i)
      call set_field_label_to_ctl(asym_composite_buoyancy_flux,         &
     &                            array_c2i)
      call set_field_label_to_ctl(sym_buoyancy_flux,  array_c2i)
      call set_field_label_to_ctl(asym_buoyancy_flux, array_c2i)
!
      end subroutine set_sym_ene_flux_names
!
! ----------------------------------------------------------------------
!
        end module m_sym_ene_flux_labels
