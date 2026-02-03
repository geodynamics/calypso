!>@file   add_sph_ref_scalar_advect.f90
!!@brief  module add_sph_ref_scalar_advect
!!
!!@author H. Matsui (UC Berkeley)
!!@date Programmed in Jan., 2026
!
!>@brief  Evaluate scalar advection of reference scalar
!!
!!@verbatim
!!      subroutine add_ref_advect_sph_MHD                               &
!!     &         (sph_rj, leg, sph_MHD_bc, MHD_prop,                    &
!!     &          iref_grad, ref_field, ipol_base, ipol_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(gradient_field_address), intent(in) :: iref_grad
!!        type(phys_data), intent(in) :: ref_field
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine add_ref_advect_sph_licv                              &
!!     &         (sph_rj, leg, sph_MHD_bc, MHD_prop,                    &
!!     &          iref_grad, ref_field, ipol_base, ipol_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(gradient_field_address), intent(in) :: ipol_grad
!!        type(phys_data), intent(in) :: ref_field
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module add_sph_ref_scalar_advect
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_base_field_labels
      use t_base_force_labels
      use t_control_parameter
      use t_boundary_data_sph_MHD
      use t_radial_reference_field
      use t_reference_scalar_param
!
      implicit none
!
      private :: set_sphere_average_scalar
      private :: add_reference_scalar_advect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_ref_advect_sph_MHD                                 &
     &         (sph_rj, leg, sph_MHD_bc, MHD_prop,                      &
     &          iref_grad, ref_field, ipol_base, ipol_frc, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
!
      type(gradient_field_address), intent(in) :: iref_grad
      type(phys_data), intent(in) :: ref_field
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  Lead advection of reference field
!
      if(MHD_prop%ref_param_T%flag_ref_field) then
        call add_reference_scalar_advect(sph_rj, leg,                   &
     &      sph_MHD_bc%sph_bc_T, MHD_prop%ht_prop,                      &
     &      ref_field%d_fld(1,iref_grad%i_grad_temp),                   &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol_base%i_velo),           &
     &      rj_fld%d_fld(1,ipol_frc%i_h_advect))
      end if
!
      if(MHD_prop%ref_param_C%flag_ref_field) then
        call add_reference_scalar_advect(sph_rj, leg,                   &
     &      sph_MHD_bc%sph_bc_C, MHD_prop%cp_prop,                      &
     &      ref_field%d_fld(1,iref_grad%i_grad_composit),               &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol_base%i_velo),           &
     &      rj_fld%d_fld(1,ipol_frc%i_c_advect))
      end if
!
      end subroutine add_ref_advect_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine add_ref_advect_sph_licv                                &
     &         (sph_rj, leg, sph_MHD_bc, MHD_prop,                      &
     &          iref_grad, ref_field, ipol_base, ipol_frc, rj_fld)
!
      use t_grad_field_labels
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
!
      type(gradient_field_address), intent(in) :: iref_grad
      type(phys_data), intent(in) :: ref_field
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: iflag
!
!   ----  Lead advection of reference field
!
      iflag = ipol_frc%i_h_advect * iref_grad%i_grad_temp
      if(iflag .gt. 0) then
        call add_reference_scalar_advect(sph_rj, leg,                   &
     &      sph_MHD_bc%sph_bc_T, MHD_prop%ht_prop,                      &
     &      ref_field%d_fld(1,iref_grad%i_grad_temp),                   &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol_base%i_velo),           &
     &      rj_fld%d_fld(1,ipol_frc%i_h_advect))
      end if
!
!
      iflag = ipol_frc%i_c_advect * iref_grad%i_grad_composit
      if(iflag .gt. 0) then
        call add_reference_scalar_advect(sph_rj, leg,                   &
     &      sph_MHD_bc%sph_bc_C, MHD_prop%cp_prop,                      &
     &      ref_field%d_fld(1,iref_grad%i_grad_composit),               &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol_base%i_velo),           &
     &      rj_fld%d_fld(1,ipol_frc%i_c_advect))
      end if
!
      end subroutine add_ref_advect_sph_licv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_sphere_average_scalar(sph_rj, ref_param_S,         &
     &          refgrad_r, d_grad, ref_grad_local, ref_grad_S)
!
      use calypso_mpi_real
      use transfer_to_long_integers
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(reference_scalar_param), intent(in) :: ref_param_S
      real(kind = kreal), intent(in) :: d_grad(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: refgrad_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout)                                 &
     &                :: ref_grad_local(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(inout)                                 &
     &                :: ref_grad_S(0:sph_rj%nidx_rj(1))
!
      integer(kind = kint) :: nri
!
!
      nri = sph_rj%nidx_rj(1)
      if(ref_param_S%flag_ref_field) then
!$omp parallel workshare
        ref_grad_S(0:nri) = refgrad_r(0:nri)
!$omp end parallel workshare
      else
!$omp parallel workshare
        ref_grad_S(0:nri) = 0.0d0
!$omp end parallel workshare
        if(sph_rj%idx_rj_degree_zero .gt. 0) then
!$omp parallel workshare
          ref_grad_local(1:nri) = half * d_grad(1:nri)                  &
     &                           * sph_rj%ar_1d_rj(1:nri,2)
!$omp end parallel workshare
        end if
!
        call calypso_mpi_allreduce_real(ref_grad_local(1),              &
     &      ref_grad_S(1), cast_long(sph_rj%nidx_rj(1)), MPI_SUM)
      end if
!
      end subroutine set_sphere_average_scalar
!
!-----------------------------------------------------------------------
!
      subroutine add_reference_scalar_advect                            &
     &         (sph_rj, leg, sph_bc_S, prop_S, refgrad_r,               &
     &          n_point, d_velo, d_advect)
!
      use t_scalar_property
      use t_boundary_params_sph_MHD
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(scalar_property), intent(in) :: prop_S
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_velo(n_point,3)
      real(kind = kreal), intent(in) :: refgrad_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_advect(n_point)
!
      integer(kind= kint) :: inod, j, k
!
!
!$omp parallel do private (inod,j,k)
      do k = sph_bc_S%kr_in, sph_bc_S%kr_out
        do j = 1, sph_rj%nidx_rj(2)
          inod = j + (k-1) * sph_rj%nidx_rj(2)
!
          d_advect(inod) = d_advect(inod)                               &
     &         + prop_S%coef_advect * leg%g_sph_rj(j,3) * refgrad_r(k)  &
     &          * sph_rj%ar_1d_rj(k,2) * d_velo(inod,1)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_reference_scalar_advect
!
!-----------------------------------------------------------------------
!
      end module add_sph_ref_scalar_advect
