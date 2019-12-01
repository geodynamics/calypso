!>@file   set_evoluved_boundaries.f90
!!@brief  module set_evoluved_boundaries
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!
!!@verbatim
!!      subroutine set_evo_scalar_boundaries                            &
!!     &         (time, sph_rj, sph_bc, bcs_S)
!!      subroutine set_evo_vector_boundaries                            &
!!     &         (time, sph_rj, sph_bc, bcs_V)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_V
!!@endverbatim
!!
      module set_evoluved_boundaries
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      implicit none
!
      private :: cal_sph_nod_evo_scalar_BC, cal_sph_nod_evo_vector_BC
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_evo_scalar_boundaries                              &
     &         (time, sph_rj, sph_bc, bcs_S)
!
      real(kind = kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
!
      type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!
      if(sph_bc%iflag_icb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_scalar_BC                                  &
     &     (time, sph_rj, bcs_S%ICB_Sevo, bcs_S%ICB_Sspec)
      end if
!
      if(sph_bc%iflag_cmb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_scalar_BC                                  &
     &     (time, sph_rj, bcs_S%CMB_Sevo, bcs_S%CMB_Sspec)
      end if
!
      end subroutine set_evo_scalar_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine set_evo_vector_boundaries                              &
     &         (time, sph_rj, sph_bc, bcs_V)
!
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      real(kind = kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
!
      type(sph_vector_boundary_data), intent(inout) :: bcs_V
!
!
      if(sph_bc%iflag_icb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_vector_BC                                  &
     &     (time, sph_rj, bcs_V%ICB_Vevo, bcs_V%ICB_Vspec)
      end if
!
      if(sph_bc%iflag_cmb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_vector_BC                                  &
     &     (time, sph_rj, bcs_V%CMB_Vevo, bcs_V%CMB_Vspec)
      end if
!
      end subroutine set_evo_vector_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_evo_scalar_BC                              &
     &         (time, sph_rj, BC_Sevo, BC_Sspec)
!
      real(kind = kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_scalar_BC_evo), intent(in) :: BC_Sevo
!
      type(sph_scalar_BC_coef), intent(inout) :: BC_Sspec
!
      integer(kind = kint) :: j, m
      real(kind = kreal) :: phase_sin
!
!
!$omp parallel do private(j,m,phase_sin)
      do j = 1, sph_rj%nidx_rj(2)
        m = abs(sph_rj%idx_gl_1d_rj_j(j,3))
        phase_sin = (dble(sign(1,sph_rj%idx_gl_1d_rj_j(j,3))) - 1.0d0)  &
     &             * atan(one)
        BC_Sspec%S_BC(j) = BC_Sevo%S_BC_mag(j)                          &
     &                * cos(dble(m) * BC_Sevo%S_BC_freq(j) * time       &
     &                      + BC_Sevo%S_BC_phase(j) + phase_sin)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_evo_scalar_BC
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_evo_vector_BC                              &
     &         (time, sph_rj, BC_Vevo, BC_Vspec)
!
      real(kind = kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_vector_BC_evo), intent(in) :: BC_Vevo
!
      type(sph_vector_BC_coef), intent(inout) :: BC_Vspec
!
      integer(kind = kint) :: j, m
      real(kind = kreal) :: phase_sin
!
!
!$omp parallel do private(j,m,phase_sin)
      do j = 1, sph_rj%nidx_rj(2)
        m = abs(sph_rj%idx_gl_1d_rj_j(j,3))
        phase_sin = (dble(sign(1,sph_rj%idx_gl_1d_rj_j(j,3))) - 1.0d0)  &
     &             * atan(one)
        BC_Vspec%Vp_BC(j) = BC_Vevo%Vp_BC_mag(j)                        &
     &                  * cos(dble(m) * BC_Vevo%Vp_BC_freq(j) * time    &
     &                        + BC_Vevo%Vp_BC_phase(j) + phase_sin)
        BC_Vspec%Dp_BC(j) = BC_Vevo%Dp_BC_mag(j)                        &
     &                  * cos(dble(m) * BC_Vevo%Vp_BC_freq(j) * time    &
     &                        + BC_Vevo%Dp_BC_phase(j) + phase_sin)
        BC_Vspec%Vt_BC(j) = BC_Vevo%Vt_BC_mag(j)                        &
     &                  * cos(dble(m) * BC_Vevo%Vt_BC_freq(j) * time    &
     &                        + BC_Vevo%Vt_BC_phase(j) + phase_sin)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_evo_vector_BC
!
! -----------------------------------------------------------------------
!
      end module set_evoluved_boundaries
