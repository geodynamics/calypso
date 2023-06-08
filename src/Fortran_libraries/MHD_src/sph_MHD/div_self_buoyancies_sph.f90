!>@file   div_self_buoyancies_sph.f90
!!@brief  module div_self_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine sel_div_buoyancies_sph_MHD                           &
!!     &         (iflag_4_gravity, iflag_4_composit_buo,                &
!!     &          sph_rj, ipol_base, ipol_grd, ipol_div_frc,            &
!!     &          coef_buo, coef_comp_buo, sph_bc_U,                    &
!!     &          ref_param_T, ref_param_C, rj_fld)
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(gradient_field_address), intent(in) :: ipol_grd
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_div_buoyancy_sph_MHD(kr_in, kr_out, coef,        &
!!     &          is_fld, ids_fld, is_div, nidx_rj, radius_1d_rj_r,     &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module div_self_buoyancies_sph
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      implicit  none
!
      private :: cal_div_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_div_buoyancies_sph_MHD                             &
     &         (iflag_4_gravity, iflag_4_composit_buo,                  &
     &          sph_rj, ipol_base, ipol_grd, ipol_div_frc,              &
     &          coef_buo, coef_comp_buo, sph_bc_U,                      &
     &          ref_param_T, ref_param_C, rj_fld)
!
      use t_reference_scalar_param
      use t_spheric_rj_data
      use t_base_field_labels
      use t_base_force_labels
      use t_grad_field_labels
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      logical, intent(in) :: iflag_4_gravity, iflag_4_composit_buo
      real(kind = kreal), intent(in) :: coef_buo, coef_comp_buo
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(gradient_field_address), intent(in) :: ipol_grd
      type(base_force_address), intent(in) :: ipol_div_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ipol_temp,  ipol_comp
      integer(kind = kint) :: igrad_temp, igrad_comp
!
!
      if(iflag_4_gravity) then
        if(ref_param_T%flag_ref_field) then
          ipol_temp =  ipol_base%i_per_temp
          igrad_temp = ipol_grd%i_grad_per_t
        else
          ipol_temp =  ipol_base%i_temp
          igrad_temp = ipol_grd%i_grad_temp
        end if
      end if
!
      if(iflag_4_composit_buo) then
        if(ref_param_C%flag_ref_field) then
          ipol_comp =  ipol_base%i_per_light
          igrad_comp = ipol_grd%i_grad_per_c
        else
          ipol_comp =  ipol_base%i_light
          igrad_comp = ipol_grd%i_grad_composit
        end if
      end if
!
      if(iflag_4_gravity .and. iflag_4_composit_buo) then
        if (iflag_debug.ge.1) write(*,*)                                &
     &        'cal_div_double_buoyancy_sph_MHD', ipol_temp
        call cal_div_double_buoyancy_sph_MHD                            &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo,                  &
     &      ipol_temp, igrad_temp, coef_comp_buo,                       &
     &      ipol_comp, igrad_comp, ipol_div_frc%i_buoyancy,             &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if(iflag_4_gravity) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &    'cal_div_buoyancy_sph_MHD by pert. temperature'
        call cal_div_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo,                  &
     &      ipol_temp, igrad_temp, ipol_div_frc%i_buoyancy,             &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if(iflag_4_composit_buo) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &      'cal_div_buoyancy_sph_MHD by composition'
        call cal_div_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_comp_buo,             &
     &      ipol_comp, igrad_comp, ipol_div_frc%i_buoyancy,             &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_div_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_double_buoyancy_sph_MHD(kr_in, kr_out,         &
     &          coef_t_buo, is_t, ids_t,  coef_c_buo, is_c, ids_c,      &
     &          is_div, nidx_rj, radius_1d_rj_r,                        &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, ids_t
      integer(kind= kint), intent(in) :: is_c, ids_c
      integer(kind= kint), intent(in) :: is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
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
!
          d_rj(inod,is_div)                                             &
     &          = three * (coef_t_buo * d_rj(inod,is_t)                 &
     &                   + coef_c_buo * d_rj(inod,is_c))                &
     &                  + (coef_t_buo * d_rj(inod,ids_t)                &
     &                   + coef_c_buo * d_rj(inod,ids_c) )              &
     &                 * radius_1d_rj_r(k)
        end do
!$omp end parallel do
!
      end subroutine cal_div_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_buoyancy_sph_MHD(kr_in, kr_out, coef,          &
     &          is_fld, ids_fld, is_div, nidx_rj, radius_1d_rj_r,       &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, ids_fld, is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
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
        d_rj(inod,is_div) = coef * ( three * d_rj(inod,is_fld)          &
     &                       + d_rj(inod,ids_fld) * radius_1d_rj_r(k))
      end do
!$omp end parallel do
!
      end subroutine cal_div_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module div_self_buoyancies_sph
