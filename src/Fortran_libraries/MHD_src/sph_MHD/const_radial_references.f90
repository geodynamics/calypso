!>@file   const_radial_references.f90
!!@brief  module const_radial_references
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Refelence scalar by diffusive profile
!!
!!@verbatim
!!      subroutine const_diffusive_profiles(sph_rj, sc_prop,            &
!!     &          sph_bc_S, bcs_S, fdm2_center, r_2nd, band_s00_poisson,&
!!     &          iref_scalar, iref_grad, iref_source, ref_field)
!!        integer(kind = kint), intent(in) :: iref_scalar, iref_grad
!!        integer(kind = kint), intent(in) :: iref_source
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
!!@endverbatim
      module const_radial_references
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
      private :: write_diffusive_profile_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profiles(sph_rj, sc_prop,              &
     &          sph_bc_S, bcs_S, fdm2_center, r_2nd, band_s00_poisson,  &
     &          iref_scalar, iref_grad, iref_source, ref_field)
!
      use const_diffusive_profile
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrix_type), intent(in) :: band_s00_poisson
!
      integer(kind = kint), intent(in) :: iref_scalar, iref_grad
      integer(kind = kint), intent(in) :: iref_source
!
      type(phys_data), intent(inout) :: ref_field
!
      real(kind = kreal), allocatable :: ref_local(:,:)
!
!
      allocate(ref_local(0:sph_rj%nidx_rj(1),0:1))
!
      if(sph_rj%idx_rj_degree_zero.gt.0 .and. iref_source.gt.0) then
!$omp parallel workshare
        ref_local(0:sph_rj%nidx_rj(1),0)                                &
     &             = ref_field%d_fld(1:sph_rj%nidx_rj(1)+1,iref_source) &
     &              * (sc_prop%coef_source / sc_prop%coef_diffuse)
        ref_local(0:sph_rj%nidx_rj(1),1) = 0.0d0
!$omp end parallel workshare
      else
!$omp parallel workshare
        ref_local(0:sph_rj%nidx_rj(1),0:1) = 0.0d0
!$omp end parallel workshare
      end if
!
      call s_const_diffusive_profile(sph_rj, r_2nd,                     &
     &     sph_bc_S, bcs_S, fdm2_center, band_s00_poisson,              &
     &    ref_field%d_fld(1,iref_scalar), ref_field%d_fld(1,iref_grad), &
     &    ref_local)
      deallocate(ref_local)
!
      end subroutine const_diffusive_profiles
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
