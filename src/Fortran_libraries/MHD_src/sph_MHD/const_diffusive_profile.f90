!>@file   const_diffusive_profile.f90
!!@brief  module const_diffusive_profile
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Refelence scalar by diffusive profile
!!
!!@verbatim
!!      subroutine s_const_diffusive_profile(sph_rj, r_2nd,             &
!!     &          sph_bc, bcs_S, fdm2_center, band_s00_poisson,         &
!!     &          reftemp_r, refgrad_r, ref_local)
!!      subroutine const_diffusive_profile_fixS(is_scalar, is_source,   &
!!     &          sph_rj, r_2nd, sc_prop, sph_bc, bcs_S, fdm2_center,   &
!!     &          band_s00_poisson, rj_fld, reftemp_rj, reftemp_local)
!!      subroutine gradient_of_radial_reference(sph_rj, r_2nd,          &
!!     &          sph_bc, bcs_S, fdm2_center, reftemp_r, refgrad_r)
!!        integer(kind = kint), intent(in) :: is_scalar, is_source
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(phys_data), intent(in) :: rj_fld
!!        type(band_matrix_type), intent(in) :: band_s00_poisson
!!@endverbatim
      module const_diffusive_profile
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_work_4_sph_trans
      use t_schmidt_poly_on_rtm
!
      use t_control_parameter
      use t_physical_property
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_MHD_boundaries
!
      use t_fdm_coefs
      use t_sph_matrix
      use t_sph_center_matrix
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_diffusive_profile(sph_rj, r_2nd,               &
     &          sph_bc, bcs_S, fdm2_center, band_s00_poisson,           &
     &          reftemp_r, refgrad_r, ref_local)
!
      use calypso_mpi
      use calypso_mpi_real
      use const_sph_radial_grad
      use fill_scalar_field
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
!      use cal_sol_reftemp_BiCGSTAB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrix_type), intent(in) :: band_s00_poisson
!
      real(kind=kreal), intent(inout) :: reftemp_r(0:sph_rj%nidx_rj(1))
      real(kind=kreal), intent(inout) :: refgrad_r(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(inout)                                 &
     &                :: ref_local(0:sph_rj%nidx_rj(1),0:1)
!
      integer(kind = kint_gl) :: num64
!      integer :: k
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        call set_ICB_scalar_boundary_1d                                 &
     &     (sph_rj, sph_bc, bcs_S%ICB_Sspec, ref_local(0,0))
        call set_CMB_scalar_boundary_1d                                 &
     &     (sph_rj, sph_bc, bcs_S%CMB_Sspec, ref_local(0,0))
!
!        do k = 0, sph_rj%nidx_rj(1)
!          write(*,*) k, 'RHS', ref_local(k,0)
!        end do
!
        call lubksb_3band_ctr(band_s00_poisson, ref_local(0,0))
!        call s_cal_sol_reftemp_BiCGSTAB                                &
!     &     (band_s00_poisson, ref_local(0,0))
!
!        do k = 0, sph_rj%nidx_rj(1)
!          write(*,*) k, 'Solution', ref_local(k,0)
!        end do
!
        call fill_scalar_1d_external(sph_bc, sph_rj%inod_rj_center,     &
     &                               sph_rj%nidx_rj(1), ref_local(0,0))
!
        call cal_sph_nod_gradient_1d(sph_bc%kr_in, sph_bc%kr_out,       &
     &                            sph_rj%nidx_rj(1), r_2nd%fdm(1)%dmat, &
     &                            ref_local(0,0), ref_local(0,1))
!
        call sel_ICB_radial_grad_1d_scalar                              &
     &     (sph_rj, sph_bc, bcs_S%ICB_Sspec, fdm2_center,               &
     &      ref_local(0,0), ref_local(0,1))
        call sel_CMB_radial_grad_1d_scalar                              &
     &     (sph_rj, sph_bc, bcs_S%CMB_Sspec,                            &
     &      ref_local(0,0), ref_local(0,1))
      end if
!
!$omp parallel workshare
      reftemp_r(0:sph_rj%nidx_rj(1)) = 0.0d0
      refgrad_r(0:sph_rj%nidx_rj(1)) = 0.0d0
!$omp end parallel workshare
!
      num64 = sph_rj%nidx_rj(1) + 1
      call calypso_mpi_allreduce_real(ref_local(0,0), reftemp_r,        &
     &                                num64, MPI_SUM)
      call calypso_mpi_allreduce_real(ref_local(0,1), refgrad_r,        &
     &                                num64, MPI_SUM)
!
      end subroutine s_const_diffusive_profile
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profile_fixS(is_scalar, is_source,     &
     &          sph_rj, r_2nd, sc_prop, sph_bc, bcs_S, fdm2_center,     &
     &          band_s00_poisson, rj_fld, reftemp_rj, reftemp_local)
!
      use calypso_mpi
      use calypso_mpi_real
      use const_sph_radial_grad
      use fill_scalar_field
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
!
      integer(kind = kint), intent(in) :: is_scalar, is_source
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson
!
      real(kind = kreal), intent(inout)                                 &
     &                :: reftemp_rj(0:sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(inout)                                 &
     &                :: reftemp_local(0:sph_rj%nidx_rj(1),0:1)
!
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: num64
!
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        if(is_source .gt. 0) then
          call copy_degree0_comps_to_sol(sph_rj%nidx_rj(2),             &
     &       sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,          &
     &       rj_fld%n_point, rj_fld%d_fld(1,is_source),                 &
     &       sph_rj%nidx_rj(1), reftemp_local(0,0))
!$omp parallel workshare
          reftemp_local(0:sph_rj%nidx_rj(1),0)                          &
     &       = (sc_prop%coef_source / sc_prop%coef_diffuse)             &
     &        * reftemp_local(0:sph_rj%nidx_rj(1),0)
!$omp end parallel workshare
          else
!$omp parallel workshare
            reftemp_local(0:sph_rj%nidx_rj(1),0) = 0.0d0
!$omp end parallel workshare
        end if
!
        call set_ICB_scalar_boundary_1d                                 &
     &     (sph_rj, sph_bc, bcs_S%ICB_Sspec, reftemp_local(0,0))
!
        inod = sph_rj%idx_rj_degree_zero                                &
     &        + (sph_bc%kr_out-1) * sph_rj%nidx_rj(2)
        reftemp_local(sph_bc%kr_out,0) = rj_fld%d_fld(inod,is_scalar)
!
        call lubksb_3band_ctr(band_s00_poisson, reftemp_local(0,0))
        call fill_scalar_1d_external(sph_bc, sph_rj%inod_rj_center,     &
     &      sph_rj%nidx_rj(1), reftemp_local(0,0))
!
        call cal_sph_nod_gradient_1d(sph_bc%kr_in, sph_bc%kr_out,       &
     &      sph_rj%nidx_rj(1), r_2nd%fdm(1)%dmat,                       &
     &      reftemp_local(0,0), reftemp_local(0,1))
!
        call fix_ICB_radial_grad_1d_scalar(sph_rj, sph_bc, fdm2_center, &
     &      reftemp_local(0,0), reftemp_local(0,1))
        call fix_CMB_radial_grad_1d_scalar(sph_rj, sph_bc,              &
     &      reftemp_local(0,0), reftemp_local(0,1))
      end if
!
!$omp parallel workshare
      reftemp_rj(0:sph_rj%nidx_rj(1),0:1) = 0.0d0
!$omp end parallel workshare
      num64 = 2 * (sph_rj%nidx_rj(1) + 1)
      call calypso_mpi_allreduce_real(reftemp_local, reftemp_rj,        &
     &                                num64, MPI_SUM)
!
      end subroutine const_diffusive_profile_fixS
!
! -----------------------------------------------------------------------
!
      subroutine gradient_of_radial_reference(sph_rj, r_2nd,            &
     &          sph_bc, bcs_S, fdm2_center, reftemp_r, refgrad_r)
!
      use const_sph_radial_grad
      use fill_scalar_field
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind=kreal), intent(inout) :: reftemp_r(0:sph_rj%nidx_rj(1))
      real(kind=kreal), intent(inout) :: refgrad_r(0:sph_rj%nidx_rj(1))
!
!
      call fill_scalar_1d_external(sph_bc, sph_rj%inod_rj_center,     &
     &                             sph_rj%nidx_rj(1), reftemp_r(0))
!
      call cal_sph_nod_gradient_1d(sph_bc%kr_in, sph_bc%kr_out,         &
     &                            sph_rj%nidx_rj(1), r_2nd%fdm(1)%dmat, &
     &                            reftemp_r(0), refgrad_r(0))
!
      call sel_ICB_radial_grad_1d_scalar                                &
     &   (sph_rj, sph_bc, bcs_S%ICB_Sspec, fdm2_center,                 &
     &    reftemp_r(0), refgrad_r(0))
      call sel_CMB_radial_grad_1d_scalar                                &
     &   (sph_rj, sph_bc, bcs_S%CMB_Sspec, reftemp_r(0), refgrad_r(0))
!
      end subroutine gradient_of_radial_reference
!
! -----------------------------------------------------------------------
!
      end module const_diffusive_profile
