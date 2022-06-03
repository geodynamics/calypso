!>@file   t_radial_references.f90
!!@brief  module t_radial_references
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Refelence scalar by diffusive profile
!!
!!@verbatim
!!      subroutine const_diffusive_profiles(sph_rj, sc_prop,            &
!!     &          sph_bc_S, bcs_S, fdm2_center, r_2nd, band_s00_poisson,&
!!     &          i_source, rj_fld, reftemp_r, refgrad_r)
!!      subroutine const_diffusive_profile_fix_bc                       &
!!     &        (sph_rj, sc_prop, sph_bc_S, fdm2_center, r_2nd,         &
!!     &         band_s00_poisson, i_temp, i_source, rj_fld, file_name, &
!!     &         reftemp_rj, reftemp_local)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_S
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(phys_data), intent(in) :: rj_fld
!!        type(band_matrix_type), intent(in) :: band_s00_poisson
!!@endverbatim
      module t_radial_references
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
      use t_coef_fdm2_MHD_boundaries
!
      use t_fdm_coefs
      use t_sph_matrix
      use t_sph_center_matrix
!
      implicit none
!
      private :: write_diffusive_profile_file
      private :: const_diffusive_profile, const_diffusive_profile_fixS
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profiles(sph_rj, sc_prop,              &
     &          sph_bc_S, bcs_S, fdm2_center, r_2nd, band_s00_poisson,  &
     &          i_source, rj_fld, reftemp_r, refgrad_r)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson
!
      integer(kind = kint), intent(in) :: i_source
!
      real(kind=kreal), intent(inout) :: reftemp_r(0:sph_rj%nidx_rj(1))
      real(kind=kreal), intent(inout) :: refgrad_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), allocatable :: ref_local(:,:)
      integer(kind = kint) :: k
!
!
      allocate(ref_local(0:sph_rj%nidx_rj(1),0:1))
!
!$omp parallel workshare
      ref_local(0:sph_rj%nidx_rj(1),0:1) = 0.0d0
!$omp end parallel workshare
!
      call const_diffusive_profile(i_source, sph_rj, r_2nd,             &
     &    sc_prop, sph_bc_S, bcs_S, fdm2_center, band_s00_poisson,      &
     &    rj_fld, reftemp_r, refgrad_r, ref_local)
      deallocate(ref_local)
!
      end subroutine const_diffusive_profiles
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profile_fix_bc                         &
     &        (sph_rj, sc_prop, sph_bc_S, fdm2_center, r_2nd,           &
     &         band_s00_poisson, i_temp, i_source, rj_fld, file_name,   &
     &         reftemp_rj, reftemp_local)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: i_temp, i_source
!
      real(kind = kreal), intent(inout)                                 &
     &                :: reftemp_rj(0:sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(inout)                                 &
     &                :: reftemp_local(0:sph_rj%nidx_rj(1),0:1)
!
!
      call const_diffusive_profile_fixS(i_temp, i_source, sph_rj,       &
     &    r_2nd, sc_prop, sph_bc_S, fdm2_center, band_s00_poisson,      &
     &    rj_fld, reftemp_rj, reftemp_local)
!
      if(iflag_debug .gt. 0) then
        call write_diffusive_profile_file(file_name, sph_rj,            &
     &                                    reftemp_rj)
      end if
!
      end subroutine const_diffusive_profile_fix_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_diffusive_profile_file(file_name, sph_rj,        &
     &                                        reftemp_rj)
!
      character(len=kchara), intent(in) :: file_name
      type(sph_rj_grid), intent(in) :: sph_rj
!
      real(kind = kreal), intent(inout)                                 &
     &                :: reftemp_rj(0:sph_rj%nidx_rj(1),0:1)
!
      integer(kind = kint) :: k
!
!
      open(52,file=file_name, position='append')
      write(52,'(a)')                                                   &
     &         'Id, radius, reference_scalar, reference_grad_r'
      write(52,'(i6,1p3E25.15e3)')  0, zero, reftemp_rj(0,0:1)
      do k = 1, sph_rj%nidx_rj(1)
        write(52,'(i6,1p3E25.15e3)') k, sph_rj%radius_1d_rj_r(k),       &
     &                                  reftemp_rj(k,0:1)
      end do
      close(52)
!
      end subroutine write_diffusive_profile_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profile(is_source, sph_rj, r_2nd,      &
     &          sc_prop, sph_bc, bcs_S, fdm2_center,                    &
     &          band_s00_poisson, rj_fld, reftemp_r, refgrad_r,         &
     &          ref_local)
!
      use calypso_mpi
      use calypso_mpi_real
      use const_sph_radial_grad
      use fill_scalar_field
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
!      use cal_sol_reftemp_BiCGSTAB
!
      integer(kind = kint), intent(in) :: is_source
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrix_type), intent(in) :: band_s00_poisson
      type(phys_data), intent(in) :: rj_fld
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
        if(is_source .gt. 0) then
          call copy_degree0_comps_to_sol                                &
     &      (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                      &
     &       sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,          &
     &       is_source, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, &
     &       ref_local(0,0))
!$omp parallel workshare
          ref_local(0:sph_rj%nidx_rj(1),0)                              &
     &       = (sc_prop%coef_source / sc_prop%coef_diffuse)             &
     &        * ref_local(0:sph_rj%nidx_rj(1),0)
!$omp end parallel workshare
          else
!$omp parallel workshare
            ref_local(0:sph_rj%nidx_rj(1),0) = 0.0d0
!$omp end parallel workshare
        end if
!
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
      num64 = sph_rj%nidx_rj(1) + 1
      call calypso_mpi_allreduce_real(ref_local(0,0), reftemp_r,        &
     &                                num64, MPI_SUM)
      call calypso_mpi_allreduce_real(ref_local(0,1), refgrad_r,        &
     &                                num64, MPI_SUM)
!
      end subroutine const_diffusive_profile
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profile_fixS(is_scalar, is_source,     &
     &          sph_rj, r_2nd, sc_prop, sph_bc, fdm2_center,            &
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
          call copy_degree0_comps_to_sol                                &
     &      (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                      &
     &       sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,          &
     &       is_source, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, &
     &       reftemp_local(0,0))
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
        if(    sph_bc%iflag_icb .eq. iflag_sph_fill_center              &
     &    .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
          inod = sph_rj%inod_rj_center
          reftemp_local(0,0) = rj_fld%d_fld(inod,is_scalar)
        else
          inod = sph_rj%idx_rj_degree_zero                              &
     &          + (sph_bc%kr_in-1) * sph_rj%nidx_rj(2)
          reftemp_local(sph_bc%kr_in,0) = rj_fld%d_fld(inod,is_scalar)
        end if
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
      end module t_radial_references
