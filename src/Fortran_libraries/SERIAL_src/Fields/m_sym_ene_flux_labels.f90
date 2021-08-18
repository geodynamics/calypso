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
!!      integer(kind = kint) function num_sym_ene_fluxes()
!!      subroutine set_sym_ene_flax_labels(n_comps, names, maths)
!!
!! !!!!!  List of energy flux by SGS terms  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   inertia_work_sym          [eflux_by_filter%i_m_advect_work]
!!   wk_against_Lorentz_sym    [eflux_by_filter%i_nega_ujb]
!!   Lorentz_work_sym          [eflux_by_filter%i_ujb]
!!   mag_tension_work_sym      [eflux_by_filter%i_m_tension_wk]
!!
!!   sym_buoyancy_flux            [eflux_by_filter%i_buo_gen]
!!   sym_comp_buoyancy_flux       [eflux_by_filter%i_c_buo_gen]
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
        integer(kind = kint), parameter, private :: neflux_w_sym = 10
!
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
        type(field_def), parameter :: mns_us_d_ws_x_ua            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = '-us_d_ws_x_ua',                &
        &                math = '$ u_{i} (e_{ijk}'                         &
        &                     //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
        type(field_def), parameter :: mns_us_d_wa_x_us            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = '-us_d_wa_x_us',                &
        &                math = '$ u_{i} (e_{ijk}'                         &
        &                     //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
        type(field_def), parameter :: mns_ua_d_ws_x_us            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = '-ua_d_ws_x_us',                &
        &                math = '$ u_{i} (e_{ijk}'                         &
        &                     //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
        type(field_def), parameter :: mns_ua_d_wa_x_ua            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = '-ua_d_wa_x_ua',                &
        &                math = '$ u_{i} (e_{ijk}'                         &
        &                     //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!
!>        Field label of work of Lorentz force
!!         @f$ u_{symi} (e_{ijk} \tilde{J}_{symj} \tilde{B}_{asymk}) @f$
        type(field_def), parameter :: us_d_js_x_ba            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = 'us_d_js_x_ba',                &
        &         math = '$ u_{symi} (e_{ijk} \tilde{J}_{symj} \tilde{B}_{asymk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{symi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{symk}) @f$
        type(field_def), parameter :: us_d_ja_x_bs            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = 'us_d_ja_x_bs',                &
        &         math = '$ u_{symi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{symk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{asymi} (e_{ijk} \tilde{J}_{symj} \tilde{B}_{symk}) @f$
        type(field_def), parameter :: ua_d_js_x_bs            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = 'ua_d_js_x_bs',                &
        &         math = '$ u_{asymi} (e_{ijk} \tilde{J}_{symj} \tilde{B}_{symk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{asymi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{asymk}) @f$
        type(field_def), parameter :: ua_d_ja_x_ba            &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = 'ua_d_ja_x_ba',                &
        &         math = '$ u_{asymi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{asymk})$')
! 
!>        Field label for sym buoyancy flux
!!         @f$ -u_{symi} \alpha_{T} g_{i} \tilde{T}_{sym} @f$
        type(field_def), parameter :: sym_buoyancy_flux              &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = 'sym_buoyancy_flux',                  &
        &                math = '$ u_{asymi} (e_{ijk} \tilde{J}_{asymj} \tilde{B}_{symk})$')
!>        Field label for asym buoyancy flux
!!         @f$ -u_{asymi} \alpha_{T} g_{i} \tilde{T}_{asym} @f$
        type(field_def), parameter :: asym_buoyancy_flux              &
        &    = field_def(n_comp = n_scalar,                                &
        &                name = 'asym_buoyancy_flux',                  &
        &                math = '$ -u_{asymi} \alpha_{T} g_{i} \tilde{T}_{asym} $')
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
        check_enegy_fluxes_w_sym                              &
        &   =    (field_name .eq. mns_us_d_ws_x_ua%name)           &
        &   .or. (field_name .eq. mns_us_d_wa_x_us%name)           &
        &   .or. (field_name .eq. mns_ua_d_ws_x_us%name)             &
        &   .or. (field_name .eq. mns_ua_d_wa_x_ua%name)             &
        &   .or. (field_name .eq. us_d_js_x_ba%name)             &
        &   .or. (field_name .eq. us_d_ja_x_bs%name)             &
        &   .or. (field_name .eq. ua_d_js_x_bs%name)           &
        &   .or. (field_name .eq. ua_d_ja_x_ba%name)           &
        &   .or. (field_name .eq. sym_buoyancy_flux%name)      &
        &   .or. (field_name .eq. asym_buoyancy_flux%name)     
!
        end function check_enegy_fluxes_w_sym
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
        integer(kind = kint) function num_sym_ene_fluxes()
        num_sym_ene_fluxes = neflux_w_sym
        return
        end function num_sym_ene_fluxes
!
! ----------------------------------------------------------------------
!
        subroutine set_sym_ene_flax_labels(n_comps, names, maths)
!
        integer(kind = kint_4b), intent(inout)                            &
        &                        :: n_comps(neflux_w_sym)
        character(len = kchara), intent(inout) :: names(neflux_w_sym)
        character(len = kchara), intent(inout) :: maths(neflux_w_sym)
!
!
        call set_field_labels(us_d_js_x_ba,                   &
        &    n_comps( 1), names( 1), maths( 1))
        call set_field_labels(us_d_ja_x_bs,             &
        &    n_comps( 2), names( 2), maths( 2))
        call set_field_labels(ua_d_js_x_bs,                   &
        &    n_comps( 3), names( 3), maths( 3))
        call set_field_labels(ua_d_ja_x_ba,               &
        &    n_comps( 4), names( 4), maths( 4))
!
        call set_field_labels(sym_buoyancy_flux,                     &
        &    n_comps( 5), names( 5), maths( 5))
        call set_field_labels(asym_buoyancy_flux,                &
        &    n_comps( 6), names( 6), maths( 6))
!
        call set_field_labels(mns_us_d_ws_x_ua,                &
        &    n_comps( 7), names( 7), maths( 7))
        call set_field_labels(mns_us_d_wa_x_us,                &
        &    n_comps( 8), names( 8), maths( 8))
        call set_field_labels(mns_ua_d_ws_x_us,                &
        &    n_comps( 9), names( 9), maths( 9))
        call set_field_labels(mns_ua_d_wa_x_ua,                &
        &    n_comps( 10), names( 10), maths( 10))
! 
        end subroutine set_sym_ene_flax_labels
!
! ----------------------------------------------------------------------
!
        end module m_sym_ene_flux_labels
