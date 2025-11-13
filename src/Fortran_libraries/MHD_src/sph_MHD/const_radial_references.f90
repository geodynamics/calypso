!>@file   const_radial_references.f90
!!@brief  module const_radial_references
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Refelence scalar by diffusive profile
!!
!!@verbatim
!!      subroutine const_diffusive_profiles                             &
!!     &         (irank_reference, sph_params, sph_rj, sc_prop, k_ratio, dk_dr, &
!!     &          sph_bc_S, bcs_S, fdm2_center, r_2nd, mat_name,        &
!!     &          iref_source, iref_scalar, iref_grad, ref_field)
!!        integer, intent(in) :: irank_reference
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_S
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        character(len=kchara), intent(in) :: mat_name
!!        integer(kind = kint), intent(in) :: iref_source
!!        integer(kind = kint), intent(in) :: iref_scalar, iref_grad
!!        type(phys_data), intent(inout) :: ref_field
!!      subroutine const_diffusive_profile_fix_bc                       &
!!     &        (sph_rj, sc_prop, sph_bc_S, fdm2_center, bcs_S, r_2nd,  &
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
!!      subroutine const_grad_diffusive_prof                            &
!!     &         (irank_reference, ref_file_IO, phys_name,              &
!!     &          sph_params, sph_rj, sc_prop, k_ratio, dk_dr, sph_bc_S, bcs_S,           &
!!     &          r_2nd, fdm2_center, mat_name, iref_radius,            &
!!     &          iref_scalar, iref_grad, iref_source, ref_field, r_itp)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(band_matrix_type), intent(in) :: band_s00_poisson
!!        character(len=kchara), intent(in) :: mat_name
!!        integer(kind = kint), intent(in) :: iref_scalar
!!        integer(kind = kint), intent(in) :: iref_grad
!!        integer(kind = kint), intent(in) :: iref_source
!!        type(phys_data), intent(inout) :: ref_field
!!@endverbatim
      module const_radial_references
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_work_4_sph_trans
      use t_schmidt_poly_on_rtm
!
      use t_control_parameter
      use t_scalar_property
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      use t_fdm_coefs
      use t_sph_matrix
      use t_sph_center_matrix
!
      implicit none
!
      private :: write_diffusive_profile_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profiles                               &
     &         (irank_reference, sph_params, sph_rj, sc_prop, k_ratio, dk_dr, &
     &          sph_bc_S, bcs_S, fdm2_center, r_2nd, mat_name,          &
     &          iref_source, iref_scalar, iref_grad, ref_field)
!
      use calypso_mpi_real
      use const_sph_r_mat_ref_scalar
      use const_diffusive_profile
      use fill_scalar_field
      use transfer_to_long_integers
!
      integer, intent(in) :: irank_reference
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
!
      character(len=kchara), intent(in) :: mat_name
      integer(kind = kint), intent(in) :: iref_source
      integer(kind = kint), intent(in) :: iref_scalar, iref_grad
!
      type(phys_data), intent(inout) :: ref_field
!
      type(band_matrix_type) :: band_s00_poisson
      integer(kind = kint_gl) :: num64
!
!
      if(iref_scalar .le. 0) return
      if(my_rank .eq. irank_reference) then
        if(iref_source .gt. 0) then
!$omp parallel workshare
          ref_field%d_fld(1:ref_field%n_point,iref_scalar)              &
     &       = ref_field%d_fld(1:ref_field%n_point,iref_source)
!$omp end parallel workshare
        end if
!
        call s_const_sph_r_mat_ref_scalar((my_rank+50), mat_name,       &
     &      sc_prop%flag_val_diffuse, k_ratio, dk_dr, sph_rj,           &
     &      r_2nd, sph_bc_S, fdm2_center, band_s00_poisson)
        call cal_diffusive_profile                                      &
     &     (sph_rj, sc_prop, sph_bc_S, bcs_S, r_2nd, fdm2_center,       &
     &      band_s00_poisson, ref_field%d_fld(1,iref_scalar))
        call fill_scalar_1d_external(sph_bc_S, sph_rj%inod_rj_center,   &
     &      sph_rj%nidx_rj(1), ref_field%d_fld(1,iref_scalar))
        call dealloc_band_matrix(band_s00_poisson)
!
        if(iref_grad .gt. 0) then
          call gradient_of_radial_reference(sph_rj, sph_bc_S, bcs_S,    &
     &        r_2nd, fdm2_center, ref_field%d_fld(1,iref_scalar),       &
     &        ref_field%d_fld(1,iref_grad))
        end if
      end if
!
      num64 = cast_long(ref_field%n_point * n_scalar)
      call calypso_mpi_bcast_real(ref_field%d_fld(1,iref_scalar),       &
     &                            num64, irank_reference)
      if(iref_grad .gt. 0) then
        call calypso_mpi_bcast_real(ref_field%d_fld(1,iref_grad),       &
     &                              num64, irank_reference)
      end if
!
      end subroutine const_diffusive_profiles
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_diffusive_prof                              &
     &         (irank_reference, ref_file_IO, phys_name,                &
     &          sph_params, sph_rj, sc_prop, k_ratio, dk_dr, sph_bc_S, bcs_S,           &
     &          r_2nd, fdm2_center, mat_name, iref_radius,              &
     &          iref_scalar, iref_grad, iref_source, ref_field, r_itp)
!
      use t_file_IO_parameter
      use t_sph_radial_interpolate
      use calypso_mpi_int
      use calypso_mpi_real
      use fill_scalar_field
      use const_diffusive_profile
      use const_sph_r_mat_ref_scalar
      use radial_reference_field_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: irank_reference
      type(field_IO_params), intent(in) :: ref_file_IO
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
!
      character(len=kchara), intent(in) :: phys_name, mat_name
      integer(kind = kint), intent(in) :: iref_radius, iref_scalar
      integer(kind = kint), intent(in) :: iref_grad, iref_source
!
      type(phys_data), intent(inout) :: ref_field
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      type(band_matrix_type) :: band_s00_poisson
      integer(kind = kint_gl) :: num64
!
!
      if(iref_scalar .le. 0) return
      if(my_rank .eq. irank_reference) then
        call load_sph_reference_one_field(iref_radius, phys_name,       &
     &      iref_scalar, n_scalar, ref_file_IO, r_itp, ref_field)
        call fill_scalar_1d_external(sph_bc_S, sph_rj%inod_rj_center,   &
     &      sph_rj%nidx_rj(1), ref_field%d_fld(1,iref_scalar))
!
        if(iref_grad .gt. 0) then
          call gradient_of_radial_reference(sph_rj, sph_bc_S, bcs_S,    &
     &        r_2nd, fdm2_center, ref_field%d_fld(1,iref_scalar),       &
     &        ref_field%d_fld(1,iref_grad))
        end if
!
        if(iref_source .gt. 0) then
          call s_const_sph_r_mat_ref_scalar((my_rank+50), mat_name,     &
     &        sc_prop%flag_val_diffuse, k_ratio, dk_dr, sph_rj,         &
     &        r_2nd, sph_bc_S, fdm2_center, band_s00_poisson)
          call cal_reference_source(sph_rj, sc_prop, band_s00_poisson,  &
     &        ref_field%d_fld(1,iref_scalar),                           &
     &        ref_field%d_fld(1,iref_source))
          call dealloc_band_matrix(band_s00_poisson)
        end if
      end if
!
      num64 = cast_long(ref_field%n_point * n_scalar)
      call calypso_mpi_bcast_int(ref_field%iflag_update(iref_scalar),   &
     &                           cast_long(n_scalar), irank_reference)
      call calypso_mpi_bcast_real(ref_field%d_fld(1,iref_scalar),       &
     &                            num64, irank_reference)
!
      if(iref_grad .gt. 0) then
        call calypso_mpi_bcast_real(ref_field%d_fld(1,iref_grad),       &
     &                              num64, irank_reference)
      end if
      if(iref_grad .gt. 0) then
        call calypso_mpi_bcast_real(ref_field%d_fld(1,iref_source),     &
     &                              num64, irank_reference)
      end if
!
      end subroutine const_grad_diffusive_prof
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profile_fix_bc                         &
     &        (sph_rj, sc_prop, sph_bc_S, bcs_S, fdm2_center, r_2nd,    &
     &         band_s00_poisson, i_temp, i_source, rj_fld, file_name,   &
     &         reftemp_rj, reftemp_local)
!
      use const_diffusive_profile
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
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
     &    r_2nd, sc_prop, sph_bc_S, bcs_S, fdm2_center,                 &
     &    band_s00_poisson, rj_fld, reftemp_rj, reftemp_local)
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
!
      end module const_radial_references
