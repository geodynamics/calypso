!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine sel_explicit_sph(time_d, MHD_prop, sph_MHD_bc,       &
!!     &                            sph, ipol, rj_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) ::  sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine sel_explicit_sph_momentum(time_d, sph_rj,            &
!!     &          fl_prop, sph_bc_U, ipol, rj_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module cal_momentum_eq_explicit
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
      use t_time_data
!
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      implicit  none
!
      private :: sel_explicit_sph_induction
      private :: sel_explicit_sph_scalar
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph(time_d, MHD_prop, sph_MHD_bc,         &
     &                            sph, ipol, rj_fld)
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_explicit_sph_momentum(time_d, sph%sph_rj,                &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, ipol, rj_fld)
      call sel_explicit_sph_induction(time_d, MHD_prop%cd_prop,         &
     &                                ipol, rj_fld)
!
      call sel_explicit_sph_scalar                                      &
     &   (time_d, sph%sph_rj, MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T,    &
     &    ipol%diffusion%i_t_diffuse, ipol%forces%i_h_advect,           &
     &    ipol%base%i_heat_source, ipol%base%i_temp,                    &
     &    ipol%exp_work%i_pre_heat, rj_fld)
      call sel_explicit_sph_scalar                                      &
     &   (time_d, sph%sph_rj, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C,    &
     &    ipol%diffusion%i_c_diffuse, ipol%forces%i_c_advect,           &
     &    ipol%base%i_light_source, ipol%base%i_light,                  &
     &    ipol%exp_work%i_pre_composit, rj_fld)
!
      end subroutine sel_explicit_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_momentum(time_d, sph_rj,              &
     &          fl_prop, sph_bc_U, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      if(fl_prop%iflag_scheme .eq. id_no_evolution) return
      ist = (sph_bc_U%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied = sph_bc_U%kr_out *    sph_rj%nidx_rj(2)
!
      if(fl_prop%coef_velo .eq. zero) then
        call sel_exp_static_vorticity_euler                             &
     &     (ist, ied, sph_rj%inod_rj_center,                            &
     &      fl_prop, ipol%base, ipol%exp_work,                          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(fl_prop%iflag_scheme .eq. id_explicit_euler) then
        call cal_vorticity_eq_euler                                     &
     &     (ist, ied, sph_rj%inod_rj_center, time_d%dt, fl_prop,        &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(time_d%i_time_step .eq. 1) then
        call cal_vorticity_eq_euler                                     &
     &     (ist, ied, sph_rj%inod_rj_center, time_d%dt, fl_prop,        &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call set_ini_adams_inertia                                      &
     &     (ist, ied, sph_rj%inod_rj_center, ipol%exp_work,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_vorticity_eq_adams                                     &
     &     (ist, ied, sph_rj%inod_rj_center, time_d%dt, fl_prop,        &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_explicit_sph_momentum
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_induction(time_d, cd_prop,            &
     &                                      ipol, rj_fld)
!
      use cal_explicit_terms
!
      type(time_data), intent(in) :: time_d
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
!
      if(cd_prop%coef_magne .eq. zero) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                  'sel_exp_static_induction_euler'
        call sel_exp_static_induction_euler(ipol%base, ipol%forces,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(cd_prop%iflag_Bevo_scheme .eq. id_explicit_euler) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                  'cal_diff_induction_MHD_euler'
        call cal_diff_induction_MHD_euler(cd_prop, ipol%base,           &
     &      ipol%forces, ipol%diffusion, time_d%dt,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(time_d%i_time_step .eq. 1) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                  'cal_diff_induction_MHD_euler'
        call cal_diff_induction_MHD_euler(cd_prop, ipol%base,           &
     &      ipol%forces, ipol%diffusion, time_d%dt,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'set_ini_adams_mag_induct'
        call set_ini_adams_mag_induct(ipol%exp_work, ipol%forces,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'cal_diff_induction_MHD_adams'
        call cal_diff_induction_MHD_adams(cd_prop, ipol%base,           &
     &      ipol%exp_work, ipol%forces, ipol%diffusion, time_d%dt,      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_explicit_sph_induction
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_scalar                                &
     &         (time_d, sph_rj, scl_prop, sph_bc_S,                     &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, rj_fld)
!
      use select_diff_adv_source
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: scl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      if(scl_prop%iflag_scheme .eq. id_no_evolution) return
      ist = (sph_bc_S%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_S%kr_out *   sph_rj%nidx_rj(2)
!
      if(scl_prop%coef_advect .eq. zero) then
        call sel_exp_static_src_euler(ist, ied, sph_rj%inod_rj_center,  &
     &      ipol_source, ipol_scalar, scl_prop%coef_source, rj_fld)
      else if(scl_prop%iflag_scheme .eq. id_explicit_euler) then
        call sel_scalar_diff_adv_src_euler                              &
     &     (ist, ied, sph_rj%inod_rj_center,                            &
     &      ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,        &
     &      time_d%dt, scl_prop%coef_exp, scl_prop%coef_source, rj_fld)
      else if(time_d%i_time_step .eq. 1) then
        call sel_scalar_diff_adv_src_euler                              &
     &     (ist, ied, sph_rj%inod_rj_center,                            &
     &      ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,        &
     &      time_d%dt, scl_prop%coef_exp, scl_prop%coef_source, rj_fld)
        call sel_ini_adams_scalar_w_src                                 &
     &     (ist, ied, sph_rj%inod_rj_center, ipol_advect,               &
     &      ipol_source, ipol_pre, scl_prop%coef_source, rj_fld)
      else
        call sel_scalar_diff_adv_src_adams                              &
     &     (ist, ied, sph_rj%inod_rj_center, ipol_diffuse, ipol_advect, &
     &      ipol_source, ipol_scalar, ipol_pre, time_d%dt,              &
     &      scl_prop%coef_exp, scl_prop%coef_source, rj_fld)
      end if
!
      end subroutine sel_explicit_sph_scalar
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
