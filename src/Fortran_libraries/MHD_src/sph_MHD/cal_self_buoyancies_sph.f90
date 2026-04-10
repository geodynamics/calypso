!>@file   cal_self_buoyancies_sph.f90
!!@brief  module cal_self_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine sel_field_address_for_buoyancies                     &
!!     &         (ipol_base, ref_param_T, ref_param_C,                  &
!!     &          ibuo_temp, ibuo_comp)
!!      subroutine sel_buoyancies_sph_MHD                               &
!!     &         (sph_rj, leg, ipol_frc, fl_prop, sph_bc_U,             &
!!     &          ibuo_temp, ibuo_comp, rj_fld)
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_total_buoyancy(i_frc, fld)
!!        type(base_force_address), intent(in) :: i_frc
!!        type(phys_data), intent(inout) :: fld
!!      subroutine cal_total_div_buoyancy(i_div_frc, fld)
!!        type(base_force_address), intent(in) :: i_div_frc
!!        type(phys_data), intent(inout) :: fld
!!      subroutine cal_total_buoyancy_scalar(i_thrm, i_comp, i_flux,    &
!!     &                                     nnod, ntot_comp, d_fld)
!!        integer(kind = kint), intent(in) :: i_thrm, i_comp
!!        integer(kind = kint), intent(in) :: i_flux
!!        integer(kind = kint), intent(in) :: nnod, ntot_comp
!!        real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
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
      use m_phys_constants
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
      private :: sel_buoyancy_sph_rj
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_field_address_for_buoyancies                       &
     &         (ipol_base, ref_param_T, ref_param_C,                    &
     &          ibuo_temp, ibuo_comp)
!
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(base_field_address), intent(in) :: ipol_base
!
      integer(kind = kint), intent(inout) :: ibuo_temp,  ibuo_comp
!
!
      if(ref_param_T%flag_ref_field) then
        ibuo_temp =  ipol_base%i_per_temp
      else
        ibuo_temp =  ipol_base%i_temp
      end if
!
      if(ref_param_C%flag_ref_field) then
        ibuo_comp =  ipol_base%i_per_light
      else
        ibuo_comp =  ipol_base%i_light
      end if
!
      end subroutine sel_field_address_for_buoyancies
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_buoyancies_sph_MHD                                 &
     &         (sph_rj, leg, ipol_frc, fl_prop, sph_bc_U,               &
     &          ibuo_temp, ibuo_comp, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(fluid_property), intent(in) :: fl_prop
      type(base_force_address), intent(in) :: ipol_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: ibuo_temp,  ibuo_comp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_buoyancy_sph_rj(fl_prop%i_grav, sph_rj, leg, ibuo_temp,  &
     &    ipol_frc%i_thrm_buo, fl_prop%coef_buo, sph_bc_U, rj_fld)
      call sel_buoyancy_sph_rj(fl_prop%i_grav, sph_rj, leg, ibuo_comp,  &
     &    ipol_frc%i_comp_buo, fl_prop%coef_comp_buo, sph_bc_U, rj_fld)
!
      end subroutine sel_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_buoyancy_sph_rj                                    &
     &         (i_grav, sph_rj, leg, ipol_scalar, ipol_buo,             &
     &          coef_buo, sph_bc_U, rj_fld)
!
      use cal_buoyancies_sph_MHD
      use adjust_reference_fields
!
      integer(kind = kint), intent(in) :: i_grav
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_buo
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_boundary_type), intent(in) :: sph_bc_U
      real(kind = kreal), intent(in) :: coef_buo
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (ipol_scalar * ipol_buo .le. 0) return
!
      if(i_grav .eq. iflag_radial_g) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &    'cal_radial_buoyancy_sph_MHD by pert. temperature'
        call cal_radial_buoyancy_sph_MHD                                &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, leg%g_sph_rj,              &
     &      coef_buo, ipol_scalar, ipol_buo,                            &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
       else
        if (iflag_debug.ge.1)  write(*,*)                               &
     &    'cal_self_buoyancy_sph_MHD by pert. temperature'
        call cal_self_buoyancy_sph_MHD                                  &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, leg%g_sph_rj,              &
     &      coef_buo, ipol_scalar, ipol_buo,                            &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      call delete_sphere_average(ipol_buo, sph_rj, rj_fld)
!
      end subroutine sel_buoyancy_sph_rj
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_total_buoyancy(i_frc, fld)
!
      use cal_add_smp
      use copy_nodal_fields
!
      type(base_force_address), intent(in) :: i_frc
      type(phys_data), intent(inout) :: fld
!
!
      if(i_frc%i_buoyancy .le. 0) return
      if(i_frc%i_thrm_buo*i_frc%i_comp_buo .gt. 0) then
        call add_2_nod_vectors(fld, i_frc%i_thrm_buo, i_frc%i_comp_buo, &
     &                              i_frc%i_buoyancy)
      else if(i_frc%i_thrm_buo .gt. 0) then
        call copy_vector_component(fld, i_frc%i_thrm_buo,               &
     &                                  i_frc%i_buoyancy)
      else if(i_frc%i_comp_buo .gt. 0) then
        call copy_vector_component(fld, i_frc%i_comp_buo,               &
     &                                  i_frc%i_buoyancy)
      else
        call clear_field_data(fld, n_vector, i_frc%i_buoyancy)
      end if
!
      end subroutine cal_total_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine cal_total_div_buoyancy(i_div_frc, fld)
!
      type(base_force_address), intent(in) :: i_div_frc
      type(phys_data), intent(inout) :: fld
!
!
      call cal_total_buoyancy_scalar                                    &
     &  (i_div_frc%i_thrm_buo, i_div_frc%i_comp_buo,                    &
     &   i_div_frc%i_buoyancy, fld%n_point, fld%ntot_phys, fld%d_fld)
!
      end subroutine cal_total_div_buoyancy
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_total_buoyancy_scalar(i_thrm, i_comp, i_flux,      &
     &                                     nnod, ntot_comp, d_fld)
!
      use cal_add_smp
      use copy_field_smp
      use delete_field_smp
!
      integer(kind = kint), intent(in) :: i_thrm, i_comp
      integer(kind = kint), intent(in) :: i_flux
      integer(kind = kint), intent(in) :: nnod, ntot_comp
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
!
      if(i_flux .le. 0) return
      if(i_thrm*i_comp .gt. 0) then
        call add_scalars_smp(nnod, d_fld(1,i_thrm), d_fld(1,i_comp),    &
     &                       d_fld(1,i_flux))
      else if(i_thrm .gt. 0) then
        call copy_nod_scalar_smp(nnod, d_fld(1,i_thrm),                 &
     &                           d_fld(1,i_flux))
      else if(i_comp .gt. 0) then
        call copy_nod_scalar_smp(nnod, d_fld(1,i_comp),                 &
     &                           d_fld(1,i_flux))
      else
        call constant_scalar_smp(zero, nnod, ione, nnod,                &
     &                           d_fld(1,i_flux))
      end if
!
      end subroutine cal_total_buoyancy_scalar
!
! ----------------------------------------------------------------------
!
      end module cal_self_buoyancies_sph
