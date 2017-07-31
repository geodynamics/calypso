!>@file   cal_buoyancies_sph_MHD.f90
!!@brief  module cal_buoyancies_sph_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine sel_buoyancies_sph_MHD                               &
!!     &         (sph_rj, leg, ipol, fl_prop, ref_param_T, ref_param_C, &
!!     &          sph_bc_U, rj_fld)
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module cal_buoyancies_sph_MHD
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
!
      implicit  none
!
      private :: cal_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_buoyancies_sph_MHD                                 &
     &         (sph_rj, leg, ipol, fl_prop, ref_param_T, ref_param_C,   &
     &          sph_bc_U, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
      use t_schmidt_poly_on_rtm
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_boundary_params_sph_MHD
      use adjust_reference_fields
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ipol_temp,  ipol_comp
!
!
      if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp        &
     &  .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
        ipol_temp =  ipol%i_par_temp
      else
        ipol_temp =  ipol%i_temp
      end if
!
      if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp        &
     &  .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
        ipol_comp =  ipol%i_par_light
      else
        ipol_comp =  ipol%i_light
      end if
!
      if (ipol_temp*ipol%i_buoyancy .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &    'cal_buoyancy_sph_MHD by pert. temperature'
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &      leg%g_sph_rj, fl_prop%coef_buo, ipol_temp, ipol%i_buoyancy, &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average(ipol%i_buoyancy, sph_rj, rj_fld)
      end if
!
      if (ipol_comp*ipol%i_comp_buo .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &      'cal_buoyancy_sph_MHD by composition'
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &      leg%g_sph_rj, fl_prop%coef_comp_buo,                        &
     &      ipol_comp, ipol%i_comp_buo,                                 &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average(ipol%i_comp_buo, sph_rj, rj_fld)
      end if
!
      if(ipol%i_filter_temp*ipol%i_filter_buo .gt. 0) then
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_buoyancy_sph_MHD by filtrered temperature'
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &      leg%g_sph_rj, fl_prop%coef_buo,                             &
     &      ipol%i_filter_temp, ipol%i_filter_buo,                      &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average(ipol%i_filter_buo, sph_rj, rj_fld)
      end if
!
      end subroutine sel_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj, coef,    &
     &          is_fld, is_buo, nidx_rj, radius_1d_rj_r,                &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      use set_reference_sph_mhd
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),17)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,is_buo) = coef * g_sph_rj(j,13)                       &
     &                     * d_rj(inod,is_fld) * radius_1d_rj_r(k)**3
      end do
!$omp end parallel do
!
      end subroutine cal_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_buoyancies_sph_MHD
