!>@file   const_sph_divergence.f90
!!@brief  module const_sph_divergence
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate divergence of forces
!!
!!@verbatim
!!      subroutine const_sph_heat_advect
!!      subroutine const_sph_scalar_advect
!!
!!      subroutine const_sph_div_force(is_fld, is_div)
!!@endverbatim
!
      module const_sph_divergence
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use cal_sph_exp_1st_diff
      use cal_sph_exp_rotation
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_heat_advect
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
!
      call cal_sph_nod_vect_div2(nlayer_ICB, nlayer_CMB,                &
     &    ipol%i_h_flux, ipol%i_h_advect)
!
      if (sph_bc_T%iflag_icb .eq. iflag_fixed_flux) then
        call cal_div_sph_in_fix_flux_2(nidx_rj(2),                      &
     &      sph_bc_T%kr_in, sph_bc_T%r_ICB, sph_bc_T%ICB_flux,          &
     &      ipol%i_h_flux, ipol%i_h_advect)
      else
        call cal_sph_div_flux_4_fix_in(nidx_rj(2),                      &
     &      sph_bc_T%kr_in, sph_bc_T%r_ICB, sph_bc_T%fdm2_fix_fld_ICB,  &
     &      sph_bc_T%ICB_fld, ipol%i_h_flux, ipol%i_h_advect)
      end if
!
      if (sph_bc_T%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_div_sph_out_fix_flux_2(nidx_rj(2),                     &
     &      sph_bc_T%kr_out, sph_bc_T%r_CMB,                            &
     &      sph_bc_T%CMB_flux, ipol%i_h_flux, ipol%i_h_advect)
      else
        call cal_sph_div_flux_4_fix_out(nidx_rj(2),                     &
     &      sph_bc_T%kr_out, sph_bc_T%r_CMB, sph_bc_T%fdm2_fix_fld_CMB, &
     &      sph_bc_T%CMB_fld, ipol%i_h_flux, ipol%i_h_advect)
      end if
!
      end subroutine const_sph_heat_advect
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_advect
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
!
      call cal_sph_nod_vect_div2(nlayer_ICB, nlayer_CMB,                &
     &    ipol%i_c_flux, ipol%i_c_advect)
!
      if (sph_bc_C%iflag_icb .eq. iflag_fixed_flux) then
        call cal_div_sph_in_fix_flux_2(nidx_rj(2),                      &
     &      sph_bc_C%kr_in, sph_bc_C%r_ICB, sph_bc_C%ICB_flux,          &
     &      ipol%i_c_flux, ipol%i_c_advect)
      else
        call cal_sph_div_flux_4_fix_in(nidx_rj(2),                      &
     &      sph_bc_C%kr_in, sph_bc_C%r_ICB, sph_bc_C%fdm2_fix_fld_ICB,  &
     &      sph_bc_C%ICB_fld, ipol%i_c_flux, ipol%i_c_advect)
      end if
!
      if (sph_bc_C%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_div_sph_out_fix_flux_2(nidx_rj(2),                     &
     &      sph_bc_C%kr_out, sph_bc_C%r_CMB,                            &
     &      sph_bc_C%CMB_flux, ipol%i_c_flux, ipol%i_c_advect)
      else
        call cal_sph_div_flux_4_fix_out(nidx_rj(2),                     &
     &      sph_bc_C%kr_out, sph_bc_C%r_CMB, sph_bc_C%fdm2_fix_fld_CMB, &
     &      sph_bc_C%CMB_fld, ipol%i_c_flux, ipol%i_c_advect)
      end if
!
      end subroutine const_sph_scalar_advect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_div_force(sph_bc_U, is_fld, is_div)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_none_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_fld, is_div
!
!
      call cal_sph_nod_vect_div2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    is_fld, is_div)
!
      call cal_sph_nod_nobc_in_div2(nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_fld_ICB,    &
     &    is_fld, is_div)
      call cal_sph_nod_nobc_out_div2(nidx_rj(2),                        &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_fld_CMB,   &
     &    is_fld, is_div)
!
      end subroutine const_sph_div_force
!
! -----------------------------------------------------------------------
!
      end module const_sph_divergence
