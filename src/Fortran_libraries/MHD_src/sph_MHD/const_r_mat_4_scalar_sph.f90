!>@file   const_r_mat_4_scalar_sph.f90
!!@brief  module const_r_mat_4_scalar_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_radial_mat_4_temp_sph
!!      subroutine const_radial_mat_4_composit_sph
!!@endverbatim
!
      module const_r_mat_4_scalar_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use m_constants
      use m_machine_parameter
      use m_control_params_sph_MHD
      use m_t_int_parameter
      use m_spheric_param_smp
      use m_spheric_parameter
      use m_radial_matrices_sph
      use m_ludcmp_3band
      use m_physical_property
      use set_radial_mat_sph
      use set_sph_scalar_mat_bc
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_temp_sph
!
      integer(kind = kint) :: ip, jst, jed, j
!
!
!$omp parallel
      call set_radial_scalar_evo_mat_sph(nidx_rj(1), nidx_rj(2),        &
     &    nlayer_ICB, nlayer_CMB, coef_imp_t, coef_d_temp,              &
     &    temp_evo_mat)
!$omp end parallel
!
      if (iflag_icb_temp .eq. iflag_fixed_flux) then
        call set_fix_flux_icb_rmat_sph(nidx_rj(1), nidx_rj(2),          &
     &     coef_imp_t, coef_d_temp, temp_evo_mat)
      else
        call set_fix_scalar_icb_rmat_sph(nidx_rj(1), nidx_rj(2),        &
     &      temp_evo_mat)
      end if
!
      if (iflag_cmb_temp .eq. iflag_fixed_flux) then
        call set_fix_flux_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),          &
     &     coef_imp_t, coef_d_temp, temp_evo_mat)
      else
        call set_fix_scalar_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),        &
     &      temp_evo_mat)
      end if
!
!$omp parallel do private(jst,jed,j)
      do ip = 1, np_smp
        jst = idx_rj_smp_stack(ip-1,2) + 1
        jed = idx_rj_smp_stack(ip,  2)
        do j = jst, jed
          call ludcmp_3band(nidx_rj(1), temp_evo_mat(1,1,j),            &
     &        i_temp_pivot(1,j), ierr, temp_evo_lu(1,1,j),              &
     &        temp_evo_det(1,j))
        end do
      end do
!$omp end parallel do
!
      end subroutine const_radial_mat_4_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_composit_sph
!
      integer(kind = kint) :: ip, jst, jed, j
!
!
!$omp parallel
      call set_radial_scalar_evo_mat_sph(nidx_rj(1), nidx_rj(2),        &
     &    nlayer_ICB, nlayer_CMB, coef_imp_c, coef_d_light,             &
     &    composit_evo_mat)
!$omp end parallel
!
      if (iflag_icb_composition .eq. iflag_fixed_flux) then
        call set_fix_flux_icb_rmat_sph(nidx_rj(1), nidx_rj(2),          &
     &     coef_imp_c, coef_d_light, composit_evo_mat)
      else
        call set_fix_scalar_icb_rmat_sph(nidx_rj(1), nidx_rj(2),        &
     &      composit_evo_mat)
      end if
!
      if (iflag_cmb_composition .eq. iflag_fixed_flux) then
        call set_fix_flux_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),          &
     &     coef_imp_c, coef_d_light, composit_evo_mat)
      else
        call set_fix_scalar_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),        &
     &      composit_evo_mat)
      end if
!
!$omp parallel do private(jst,jed,j)
      do ip = 1, np_smp
        jst = idx_rj_smp_stack(ip-1,2) + 1
        jed = idx_rj_smp_stack(ip,  2)
        do j = jst, jed
          call ludcmp_3band(nidx_rj(1), composit_evo_mat(1,1,j),        &
     &        i_composit_pivot(1,j), ierr, composit_evo_lu(1,1,j),      &
     &        composit_evo_det(1,j))
        end do
      end do
!$omp end parallel do
!
      end subroutine const_radial_mat_4_composit_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_scalar_sph
