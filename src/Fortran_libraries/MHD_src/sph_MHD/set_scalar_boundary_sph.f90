!>@file   set_scalar_boundary_sph.f90
!!@brief  module set_scalar_boundary_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Evaluate scalar fields at boundaries
!!
!!@verbatim
!!      subroutine set_icb_fix_temp_sph
!!      subroutine set_cmb_fix_temp_sph
!!      subroutine set_icb_fix_h_flux_sph
!!      subroutine set_cmb_fix_h_flux_sph
!!
!!      subroutine set_icb_fix_composition_sph
!!      subroutine set_cmb_fix_composition_sph
!!      subroutine set_icb_fix_c_flux_sph
!!      subroutine set_cmb_fix_c_flux_sph
!!@endverbatim
!
      module set_scalar_boundary_sph
!
      use m_precision
!
      use m_constants
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit none
!
      private :: set_fixed_scalar_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_fix_temp_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), ione, nlayer_ICB,           &
     &    ipol%i_temp, temp_ICB_bc)
!
      end subroutine set_icb_fix_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_fix_temp_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), nlayer_CMB, nidx_rj(1),     &
     &    ipol%i_temp, temp_CMB_bc)
!
      end subroutine set_cmb_fix_temp_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_icb_fix_h_flux_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), ione, nlayer_ICB,           &
     &    ipol%i_grad_t, h_flux_ICB_bc)
!
      end subroutine set_icb_fix_h_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_fix_h_flux_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), nlayer_CMB, nidx_rj(1),     &
     &    ipol%i_grad_t, h_flux_CMB_bc)
!
      end subroutine set_cmb_fix_h_flux_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_icb_fix_composition_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), ione, nlayer_ICB,           &
     &    ipol%i_light, composition_ICB_bc)
!
      end subroutine set_icb_fix_composition_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_fix_composition_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), nlayer_CMB, nidx_rj(1),     &
     &    ipol%i_light, composition_CMB_bc)
!
      end subroutine set_cmb_fix_composition_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_icb_fix_c_flux_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), ione, nlayer_ICB,           &
     &    ipol%i_grad_composit, c_flux_ICB_bc)
!
      end subroutine set_icb_fix_c_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_fix_c_flux_sph
!
!
      call set_fixed_scalar_sph(nidx_rj(2), nlayer_CMB, nidx_rj(1),     &
     &    ipol%i_grad_composit, c_flux_CMB_bc)
!
      end subroutine set_cmb_fix_c_flux_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_sph(jmax, kr_bc_st, kr_bc_ed, is_fld, &
     &          fixed_bc)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: jmax, kr_bc_st, kr_bc_ed
      real(kind = kreal), intent(in) :: fixed_bc(jmax)
      integer(kind = kint) :: j, inod, k
!
!
!$omp parallel do private (k,j,inod)
      do k = kr_bc_st, kr_bc_ed
        do j = 1, jmax
          inod = j + (k-1) * jmax
          d_rj(inod,is_fld) = fixed_bc(j)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_fixed_scalar_sph
!
! -----------------------------------------------------------------------
!
      end module set_scalar_boundary_sph
