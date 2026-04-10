!>@file   const_sph_r_mat_ref_scalar.f90
!!@brief  module const_sph_r_mat_ref_scalar
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine s_const_sph_r_mat_ref_scalar(id_file, mat_name,      &
!!     &          flag_val_diffuse, k_ratio, dk_dr, sph_rj, r_2nd,      &
!!     &          sph_bc, fdm2_center, band_s00_poisson)
!!      subroutine const_r_mat00_poisson_fixS(id_file, mat_name,        &
!!     &          flag_val_diffuse, k_ratio, dk_dr, sph_rj, r_2nd,      &
!!     &          sph_bc, fdm2_center, band_s00_poisson)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        character(len=kchara), intent(in) :: mat_name
!!        real(kind = kreal), intent(in) :: diffuse_reduction_ratio_ICB
!!        type(band_matrix_type), intent(inout) :: band_s00_poisson
!!@endverbatim
!
      module const_sph_r_mat_ref_scalar
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_scalar_property
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_sph_matrices
      use t_sph_center_matrix
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_sph_r_mat_ref_scalar(id_file, mat_name,        &
     &          flag_val_diffuse, k_ratio, dk_dr, sph_rj, r_2nd,        &
     &          sph_bc, fdm2_center, band_s00_poisson)
!
      use m_ludcmp_3band
      use set_sph_unit_radial_mat
      use add_sph_scalar_radial_mat
      use sel_sph_r_mat_ref_scalar_bc
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      character(len=kchara), intent(in) :: mat_name
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
      logical, intent(in) :: flag_val_diffuse
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
!      integer :: i
!
!
      band_s00_poisson%mat_name = mat_name
      call alloc_ctr_band_mat(ithree, sph_rj, band_s00_poisson)
!
      if(sph_rj%idx_rj_degree_zero .le. 0) return
      call set_unit_mat_4_poisson00(sph_rj%nidx_rj(1),                  &
     &    sph_bc%kr_in, sph_bc%kr_out, band_s00_poisson%mat)
!
      if(flag_val_diffuse) then
        call add_sph_ref_val_diffusion_mat                              &
     &     (sph_rj%nidx_rj(1), sph_rj%ar_1d_rj,                         &
     &      sph_bc%kr_in, sph_bc%kr_out, one, k_ratio, dk_dr,           &
     &      r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s00_poisson%mat)
      else
        call add_sph_ref_poisson_mat                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%ar_1d_rj,                         &
     &      sph_bc%kr_in, sph_bc%kr_out, one,                           &
     &      r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s00_poisson%mat)
      end if
!
      call sel_sph_r_mat_ref_scl_bc(flag_val_diffuse, sph_rj, sph_bc,   &
     &    fdm2_center, one, k_ratio, dk_dr, band_s00_poisson)
!
!      write(*,*) 0, 'band_s00_poisson%mat(2,i)', &
!     &          1e30, band_s00_poisson%mat(2,0), &
!     &          band_s00_poisson%mat(1,1)
!      do i = 1, sph_rj%nidx_rj(1)
!        write(*,*) i, 'band_s00_poisson%mat(2,i)', &
!     &          band_s00_poisson%mat(3,i-1),   &
!     &          band_s00_poisson%mat(2,i),     &
!     &          band_s00_poisson%mat(1,1)
!      end do
!      write(*,*) 0, 'band_s00_poisson%mat(2,i)', &
!     &    band_s00_poisson%mat(3,sph_rj%nidx_rj(1)-1), &
!     &    band_s00_poisson%mat(2,sph_rj%nidx_rj(1)), 1e30
!
      call ludcmp_3band_ctr(band_s00_poisson)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_center_band_matrix(id_file, sph_rj,                  &
     &                                band_s00_poisson)
      end if
!
      end subroutine s_const_sph_r_mat_ref_scalar
!
! -----------------------------------------------------------------------
!
      subroutine const_r_mat00_poisson_fixS(id_file, mat_name,          &
     &          flag_val_diffuse, k_ratio, dk_dr, sph_rj, r_2nd,        &
     &          sph_bc, fdm2_center, band_s00_poisson)
!
      use m_ludcmp_3band
      use set_sph_unit_radial_mat
      use add_sph_scalar_radial_mat
      use sel_sph_r_mat_ref_scalar_bc
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      character(len=kchara), intent(in) :: mat_name
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
      logical, intent(in) :: flag_val_diffuse
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
!
      write(band_s00_poisson%mat_name,'(2a)')                           &
     &                              trim(mat_name), '_poisson_l0_fixS'
      call alloc_ctr_band_mat(ithree, sph_rj, band_s00_poisson)
!
      if(sph_rj%idx_rj_degree_zero .le. 0) return
      call set_unit_mat_4_poisson00(sph_rj%nidx_rj(1),                  &
     &    sph_bc%kr_in, sph_bc%kr_out, band_s00_poisson%mat)
!
      if(flag_val_diffuse) then
        call add_sph_ref_val_diffusion_mat                              &
     &     (sph_rj%nidx_rj(1), sph_rj%ar_1d_rj,                         &
     &      sph_bc%kr_in, sph_bc%kr_out, one, k_ratio, dk_dr,           &
     &      r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s00_poisson%mat)
      else
        call add_sph_ref_poisson_mat                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%ar_1d_rj,                         &
     &      sph_bc%kr_in, sph_bc%kr_out, one,                           &
     &      r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s00_poisson%mat)
      end if
!
      call sel_sph_r_mat_poisson_fixBC(flag_val_diffuse, sph_rj,        &
     &    sph_bc, fdm2_center, k_ratio, dk_dr, band_s00_poisson)
!
      call ludcmp_3band_ctr(band_s00_poisson)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_center_band_matrix(id_file, sph_rj,                  &
     &                                band_s00_poisson)
      end if
!
      end subroutine const_r_mat00_poisson_fixS
!
! -----------------------------------------------------------------------
!
      end module const_sph_r_mat_ref_scalar
