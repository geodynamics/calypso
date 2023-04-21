!>@file   cal_self_buoyancies_sph.f90
!!@brief  module cal_self_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine sel_buoyancies_sph_MHD                               &
!!     &         (sph_rj, leg, ipol_base, ipol_frc,                     &
!!     &          fl_prop, ref_param_T, ref_param_C, sph_bc_U, rj_fld)
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module cal_self_buoyancies_sph
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use t_physical_property
      use t_reference_scalar_param
      use t_schmidt_poly_on_rtm
      use t_spheric_rj_data
      use t_base_force_labels
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_buoyancies_sph_MHD                                 &
     &         (sph_rj, leg, ipol_base, ipol_frc,                       &
     &          fl_prop, ref_param_T, ref_param_C, sph_bc_U, rj_fld)
!
      use adjust_reference_fields
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ipol_temp,  ipol_comp
!
!
      if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp        &
     &  .or. ref_param_T%iflag_reference .eq. id_takepiro_temp          &
     &  .or. ref_param_T%iflag_reference .eq. id_numerical_solution     &
     &   ) then
        ipol_temp =  ipol_base%i_per_temp
      else
        ipol_temp =  ipol_base%i_temp
      end if
!
      if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp        &
     &  .or. ref_param_C%iflag_reference .eq. id_takepiro_temp          &
     &  .or. ref_param_C%iflag_reference .eq. id_numerical_solution     &
     &   ) then
        ipol_comp =  ipol_base%i_per_light
      else
        ipol_comp =  ipol_base%i_light
      end if
!
!
      if(fl_prop%i_grav .eq. iflag_radial_g) then
        call sel_r_const_buoyancies_sph_MHD                             &
     &     (sph_rj, leg, ipol_temp, ipol_comp, ipol_frc,                &
     &      fl_prop, sph_bc_U, rj_fld)
      else
        call sel_self_buoyancies_sph_MHD                                &
     &     (sph_rj, leg, ipol_temp, ipol_comp, ipol_frc,                &
     &      fl_prop, sph_bc_U, rj_fld)
      end if
!
      end subroutine sel_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_self_buoyancies_sph_MHD                            &
     &         (sph_rj, leg, ipol_temp, ipol_comp, ipol_frc,            &
     &          fl_prop, sph_bc_U, rj_fld)
!
      use cal_buoyancies_sph_MHD
      use adjust_reference_fields
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: ipol_temp, ipol_comp
      type(base_force_address), intent(in) :: ipol_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (ipol_temp * ipol_frc%i_buoyancy .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &    'cal_self_buoyancy_sph_MHD by pert. temperature'
        call cal_self_buoyancy_sph_MHD                                  &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, leg%g_sph_rj,              &
     &      fl_prop%coef_buo, ipol_temp, ipol_frc%i_buoyancy,           &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average                                      &
     &     (ipol_frc%i_buoyancy, sph_rj, rj_fld)
      end if
!
      if (ipol_comp * ipol_frc%i_comp_buo .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &      'cal_self_buoyancy_sph_MHD by composition'
        call cal_self_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &      leg%g_sph_rj, fl_prop%coef_comp_buo,                        &
     &      ipol_comp, ipol_frc%i_comp_buo,                             &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average                                      &
     &     (ipol_frc%i_comp_buo, sph_rj, rj_fld)
      end if
!
      end subroutine sel_self_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_r_const_buoyancies_sph_MHD                         &
     &         (sph_rj, leg, ipol_temp, ipol_comp, ipol_frc,            &
     &          fl_prop, sph_bc_U, rj_fld)
!
      use cal_buoyancies_sph_MHD
      use adjust_reference_fields
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: ipol_temp, ipol_comp
      type(base_force_address), intent(in) :: ipol_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (ipol_temp * ipol_frc%i_buoyancy .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &    'cal_radial_buoyancy_sph_MHD by pert. temperature'
        call cal_radial_buoyancy_sph_MHD                                &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, leg%g_sph_rj,              &
     &      fl_prop%coef_buo, ipol_temp, ipol_frc%i_buoyancy,           &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average                                      &
     &     (ipol_frc%i_buoyancy, sph_rj, rj_fld)
      end if
!
      if (ipol_comp * ipol_frc%i_comp_buo .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &      'cal_radial_buoyancy_sph_MHD by composition'
        call cal_radial_buoyancy_sph_MHD                                &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, leg%g_sph_rj,              &
     &      fl_prop%coef_comp_buo, ipol_comp, ipol_frc%i_comp_buo,      &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average                                      &
     &     (ipol_frc%i_comp_buo, sph_rj, rj_fld)
      end if
!
      end subroutine sel_r_const_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_self_buoyancies_sph
