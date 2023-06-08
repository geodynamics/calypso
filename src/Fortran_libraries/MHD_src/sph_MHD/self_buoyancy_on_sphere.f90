!>@file   self_buoyancy_on_sphere.f90
!!@brief  module self_buoyancy_on_sphere
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate buoyancy at specific radius
!!
!!@verbatim
!!      subroutine r_buoyancy_on_sphere                                 &
!!     &         (iflag_4_gravity, iflag_4_composit_buo, kr, sph_rj,    &
!!     &          ipol_base, ipol_div_frc, coef_buo, coef_comp_buo,     &
!!     &          ref_param_T, ref_param_C, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_r_buoyancy_on_sphere(kr, coef, is_fld, is_fr,    &
!!     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@param kr  Radial grid ID
!
      module self_buoyancy_on_sphere
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
      private :: cal_r_double_buo_on_sphere
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine r_buoyancy_on_sphere                                   &
     &         (iflag_4_gravity, iflag_4_composit_buo, kr, sph_rj,      &
     &          ipol_base, ipol_div_frc, coef_buo, coef_comp_buo,       &
     &          ref_param_T, ref_param_C, rj_fld)
!
      use t_reference_scalar_param
      use t_spheric_rj_data
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
!
      logical, intent(in) :: iflag_4_gravity, iflag_4_composit_buo
      real(kind = kreal), intent(in) :: coef_buo, coef_comp_buo
      integer(kind= kint), intent(in) :: kr
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_div_frc
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ipol_temp,  ipol_comp
!
!
      if(iflag_4_gravity) then
        if(ref_param_T%flag_ref_field) then
          ipol_temp =  ipol_base%i_per_temp
        else
          ipol_temp =  ipol_base%i_temp
        end if
      end if
!
      if(iflag_4_composit_buo) then
        if(ref_param_C%flag_ref_field) then
          ipol_comp =  ipol_base%i_per_light
        else
          ipol_comp =  ipol_base%i_light
        end if
      end if
!
      if(iflag_4_gravity .and. iflag_4_composit_buo) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*)'cal_r_double_buo_on_sphere', ipol_temp
        call cal_r_double_buo_on_sphere(kr, coef_buo, coef_comp_buo,    &
     &      ipol_temp, ipol_comp, ipol_div_frc%i_buoyancy,              &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if(iflag_4_gravity) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sphere'
        call cal_r_buoyancy_on_sphere(kr, coef_buo,                     &
     &      ipol_temp, ipol_div_frc%i_buoyancy,                         &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if(iflag_4_composit_buo) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sphere'
        call cal_r_buoyancy_on_sphere(kr, coef_comp_buo,                &
     &      ipol_comp, ipol_div_frc%i_comp_buo,                         &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine r_buoyancy_on_sphere
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_r_double_buo_on_sphere                             &
     &         (kr, coef_buo, coef_comp_buo, is_t, is_c, is_fr,         &
     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind= kint), intent(in) :: is_t, is_c, is_fr, kr
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef_buo, coef_comp_buo
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: inod, j
!
!
!$omp parallel do private (inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = (coef_buo*d_rj(inod,is_t)                    &
     &                     + coef_comp_buo*d_rj(inod,is_c))             &
     &                     * radius_1d_rj_r(kr)
      end do
!$omp end parallel do
!
      end subroutine cal_r_double_buo_on_sphere
!
!-----------------------------------------------------------------------
!
      subroutine cal_r_buoyancy_on_sphere(kr, coef, is_fld, is_fr,      &
     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind= kint), intent(in) :: is_fld, is_fr, kr
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: inod, j
!
!
!$omp parallel do private (inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = coef*d_rj(inod,is_fld)*radius_1d_rj_r(kr)
      end do
!$omp end parallel do
!
      end subroutine cal_r_buoyancy_on_sphere
!
!-----------------------------------------------------------------------
!
      end module self_buoyancy_on_sphere
