!>@file   const_radial_forces_on_bc.f90
!!@brief  module const_radial_forces_on_bc
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Evaluate radial component of forces at boundaries
!!
!!@verbatim
!!      subroutine s_const_radial_forces_on_bc(sph_rj, g_sph_rj,        &
!!     &          fl_prop, sph_bc_U, ref_param_T, ref_param_C,          &
!!     &          ipol_base, ipol_dif, ipol_frc, ipol_div_frc, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_radial_force_on_sph                              &
!!     &         (kr, is_fld, is_fr, nidx_rj, ar_1d_rj, g_sph_rj,       &
!!     &          n_point, ntot_phys_rj, d_rj)
!!@endverbatim
!
      module const_radial_forces_on_bc
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      implicit none
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine s_const_radial_forces_on_bc(sph_rj, g_sph_rj,          &
     &          fl_prop, sph_bc_U, ref_param_T, ref_param_C,            &
     &          ipol_base, ipol_dif, ipol_frc, ipol_div_frc, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
      use t_boundary_params_sph_MHD
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      use self_buoyancy_on_sphere
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(base_field_address), intent(in) :: ipol_base
      type(diffusion_address), intent(in) :: ipol_dif
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_div_frc
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call r_buoyancy_on_sphere                                         &
     &   (fl_prop%iflag_4_gravity, fl_prop%iflag_4_composit_buo,        &
     &    sph_bc_U%kr_in, sph_rj, ipol_base, ipol_div_frc,              &
     &    fl_prop%coef_buo, fl_prop%coef_comp_buo,                      &
     &    ref_param_T, ref_param_C, rj_fld)
      call r_buoyancy_on_sphere                                         &
     &   (fl_prop%iflag_4_gravity, fl_prop%iflag_4_composit_buo,        &
     &    sph_bc_U%kr_out, sph_rj, ipol_base, ipol_div_frc,             &
     &    fl_prop%coef_buo, fl_prop%coef_comp_buo,                      &
     &    ref_param_T, ref_param_C, rj_fld)
!
!$omp parallel
      call cal_radial_force_on_sph(sph_bc_U%kr_in,                      &
     &      ipol_dif%i_v_diffuse, ipol_dif%i_div_viscous,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_radial_force_on_sph(sph_bc_U%kr_out,                     &
     &      ipol_dif%i_v_diffuse, ipol_dif%i_div_viscous,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(fl_prop%iflag_4_inertia) then
        call cal_radial_force_on_sph(sph_bc_U%kr_in,                    &
     &      ipol_frc%i_m_advect, ipol_div_frc%i_m_advect,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_radial_force_on_sph(sph_bc_U%kr_out,                   &
     &      ipol_frc%i_m_advect, ipol_div_frc%i_m_advect,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_lorentz) then
        call cal_radial_force_on_sph(sph_bc_U%kr_in,                    &
     &      ipol_frc%i_lorentz, ipol_div_frc%i_lorentz,                 &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_radial_force_on_sph(sph_bc_U%kr_out,                   &
     &      ipol_frc%i_lorentz, ipol_div_frc%i_lorentz,                 &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine s_const_radial_forces_on_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_radial_force_on_sph                                &
     &         (kr, is_fld, is_fr, nidx_rj, ar_1d_rj, g_sph_rj,         &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_fld, is_fr
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: nidx_rj(2)
      real(kind = kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp do private(inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = d_rj(inod,is_fld)                            &
     &                     * max(g_sph_rj(j,3),half)*ar_1d_rj(kr,2)
      end do
!$omp end do
!
      end subroutine cal_radial_force_on_sph
!
!-----------------------------------------------------------------------
!
      end module const_radial_forces_on_bc
