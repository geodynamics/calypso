!>@file   const_sph_divergence.f90
!!@brief  module const_sph_divergence
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate divergence of forces
!!
!!@verbatim
!!      subroutine const_sph_scalar_advect(sph_bc, is_flux, is_advect)
!!      subroutine const_sph_div_force(is_fld, is_div)
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!
!!@param is_flux    Spherical hermonics data address for input flux
!!@param is_advect  Spherical hermonics data address for advection
!!@param is_fld     Spherical hermonics data address for input vector
!!@param is_div     Spherical hermonics data address for divergence
!
      module const_sph_divergence
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_advect(sph_bc, is_flux, is_advect)
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_coef_fdm_to_center
      use t_boundary_params_sph_MHD
      use cal_sph_exp_rotation
      use select_exp_scalar_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_flux, is_advect
!
!
      call cal_sph_nod_vect_div2(sph_bc%kr_in, sph_bc%kr_out,           &
     &    is_flux, is_advect)
      call sel_bc_sph_scalar_advect(sph_bc, is_flux, is_advect)
!
      end subroutine const_sph_scalar_advect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_div_force(sph_bc_U, is_fld, is_div)
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use t_boundary_params_sph_MHD
      use cal_sph_exp_rotation
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
