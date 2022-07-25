!>@file   const_r_mat_4_scalar_sph.f90
!!@brief  module const_r_mat_4_scalar_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_radial_mat_4_press_sph                         &
!!     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, fdm2_center,        &
!!     &          g_sph_rj, band_p_poisson)
!!      subroutine const_radial_mat_4_scalar_sph(mat_name, coef_advect, &
!!     &          dt, sph_params, sph_rj, r_2nd, property,              &
!!     &          sph_bc, fdm2_center, g_sph_rj, band_s_evo)
!!        type(scalar_property), intent(in) :: property
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!
!!      subroutine const_r_mat00_scalar_sph                             &
!!     &         (mat_name, diffusie_reduction_ICB, sph_params, sph_rj, &
!!     &          r_2nd, sph_bc, fdm2_center, band_s00_poisson)
!!      subroutine const_r_mat00_poisson_fixS                           &
!!     &         (mat_name, diffusie_reduction_ICB, sph_params, sph_rj, &
!!     &          r_2nd, sph_bc, fdm2_center, band_s00_poisson)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        character(len=kchara), intent(in) :: mat_name
!!        real(kind = kreal), intent(in) :: diffusie_reduction_ICB
!!        type(band_matrix_type), intent(inout) :: band_s00_poisson
!!@endverbatim
!
      module const_r_mat_4_scalar_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_sph_matrices
      use t_sph_center_matrix
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_press_sph                           &
     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, fdm2_center,          &
     &          g_sph_rj, band_p_poisson)
!
      use m_ludcmp_3band
      use set_sph_scalar_mat_bc
      use cal_inner_core_rotation
      use center_sph_matrices
      use mat_product_3band_mul
      use set_radial_mat_sph
      use select_r_mat_scalar_bc_sph
      use check_sph_radial_mat
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(band_matrices_type), intent(inout) :: band_p_poisson
!
      real(kind = kreal) :: coef_p
      real(kind = kreal), allocatable :: r_coef(:)
!
!
      coef_p = - fl_prop%coef_press
!
      allocate(r_coef(sph_rj%nidx_rj(1)))
!$omp parallel workshare
      r_coef(1:sph_rj%nidx_rj(1)) = coef_p
!$omp end parallel workshare
!
      call alloc_band_mat_sph(ithree, sph_rj, band_p_poisson)

      call set_unit_mat_4_poisson                                       &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, band_p_poisson%mat)
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_U%kr_in, sph_bc_U%kr_out, r_coef(1),         &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_p_poisson%mat)
!
      call sel_radial_mat_press_bc_sph(sph_rj, sph_bc_U, fdm2_center,   &
     &    g_sph_rj, r_coef, band_p_poisson)
      deallocate(r_coef)
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_p_poisson)
!
      if(i_debug .eq. iflag_full_msg) then
        write(band_p_poisson%mat_name,'(a)') 'pressure_poisson'
        call check_radial_band_mat(my_rank, sph_rj, band_p_poisson)
      end if
!
      end subroutine const_radial_mat_4_press_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_scalar_sph(mat_name, coef_advect,   &
     &          dt, sph_params, sph_rj, r_2nd, property,                &
     &          sph_bc, fdm2_center, g_sph_rj, band_s_evo)
!
      use m_ludcmp_3band
      use center_sph_matrices
      use set_radial_mat_sph
      use set_sph_scalar_mat_bc
      use select_r_mat_scalar_bc_sph
      use check_sph_radial_mat
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: property
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_advect
      character(len=kchara), intent(in) :: mat_name
!
      type(band_matrices_type), intent(inout) :: band_s_evo
!
      real(kind = kreal) :: coef
      real(kind = kreal), allocatable :: r_coef(:)
!
!
      write(band_s_evo%mat_name,'(a)') trim(mat_name)
      call alloc_band_mat_sph(ithree, sph_rj, band_s_evo)
      call set_unit_on_diag(band_s_evo)
!
      if(coef_advect .eq. zero) then
        coef = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, sph_bc%kr_out, band_s_evo%mat)
      else
        coef = property%coef_imp * property%coef_diffuse * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_s_evo%mat)
      end if
!
      allocate(r_coef(sph_rj%nidx_rj(1)))
!$omp parallel workshare
      r_coef(1:sph_rj%nidx_rj(1)) = coef
!$omp end parallel workshare
!
      if(property%diffusie_reduction_ICB .lt. one) then
        r_coef(sph_params%nlayer_ICB) = property%diffusie_reduction_ICB &
     &                                 * r_coef(sph_params%nlayer_ICB)
        if(my_rank .eq. 0) write(*,*) 'reduction of diffusivity at',    &
     &    sph_params%nlayer_ICB, ' to ', r_coef(sph_params%nlayer_ICB), &
     &                        ' from ' , coef
      end if
!
!
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc%kr_in, sph_bc%kr_out, r_coef(1),             &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s_evo%mat)
!
      call sel_radial_mat_scalar_bc_sph(sph_rj, sph_bc, fdm2_center,    &
     &    g_sph_rj, r_coef, band_s_evo)
      deallocate(r_coef)
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_s_evo)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_radial_band_mat(my_rank, sph_rj, band_s_evo)
      end if
!
      end subroutine const_radial_mat_4_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_r_mat00_scalar_sph                               &
     &         (mat_name, diffusie_reduction_ICB, sph_params, sph_rj,   &
     &          r_2nd, sph_bc, fdm2_center, band_s00_poisson)
!
      use m_ludcmp_3band
      use set_radial_mat_sph
      use select_r_mat_scalar_bc_sph
      use check_sph_radial_mat
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      character(len=kchara), intent(in) :: mat_name
      real(kind = kreal), intent(in) :: diffusie_reduction_ICB
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
      real(kind = kreal), allocatable :: r_coef(:)
!      integer :: i
!
!
      write(band_s00_poisson%mat_name,'(a)') trim(mat_name)
      call alloc_ctr_band_mat(ithree, sph_rj, band_s00_poisson)
!
      if(sph_rj%idx_rj_degree_zero .le. 0) return
      call set_unit_mat_4_poisson00(sph_rj%nidx_rj(1),                  &
     &    sph_bc%kr_in, sph_bc%kr_out, band_s00_poisson%mat)
!
      allocate(r_coef(0:sph_rj%nidx_rj(1)))
!$omp parallel workshare
      r_coef(0:sph_rj%nidx_rj(1)) = one
!$omp end parallel workshare
!
      if(diffusie_reduction_ICB .lt. one) then
        r_coef(sph_params%nlayer_ICB) = diffusie_reduction_ICB
        if(my_rank .eq. 0) write(*,*) 'reduction of diffusivity at',    &
     &    sph_params%nlayer_ICB, ' to ', r_coef(sph_params%nlayer_ICB)
      end if
!
      call add_scalar_poisson00_mat_sph                                 &
     &   (sph_rj%nidx_rj(1), sph_rj%ar_1d_rj,                           &
     &    sph_bc%kr_in, sph_bc%kr_out, r_coef(1),                       &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s00_poisson%mat)
!
      call sel_radial_mat00_scalar_bc_sph                               &
     &   (sph_rj, sph_bc, fdm2_center, r_coef, band_s00_poisson)
      deallocate(r_coef)
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
        call check_center_band_matrix(my_rank, sph_rj, band_s00_poisson)
      end if
!
      end subroutine const_r_mat00_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_r_mat00_poisson_fixS                             &
     &         (mat_name, diffusie_reduction_ICB, sph_params, sph_rj,   &
     &          r_2nd, sph_bc, fdm2_center, band_s00_poisson)
!
      use m_ludcmp_3band
      use set_radial_mat_sph
      use select_r_mat_scalar_bc_sph
      use check_sph_radial_mat
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      character(len=kchara), intent(in) :: mat_name
      real(kind = kreal), intent(in) :: diffusie_reduction_ICB
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
      real(kind = kreal), allocatable :: r_coef(:)
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
      allocate(r_coef(sph_rj%nidx_rj(1)))
!$omp parallel workshare
      r_coef(1:sph_rj%nidx_rj(1)) = one
!$omp end parallel workshare
!
      if(diffusie_reduction_ICB .lt. one) then
        r_coef(sph_params%nlayer_ICB) = diffusie_reduction_ICB
        if(my_rank .eq. 0) write(*,*) 'reduction of diffusivity at',    &
     &    sph_params%nlayer_ICB, ' to ', r_coef(sph_params%nlayer_ICB)
      end if
!
!
      call add_scalar_poisson00_mat_sph                                 &
     &   (sph_rj%nidx_rj(1), sph_rj%ar_1d_rj,                           &
     &    sph_bc%kr_in, sph_bc%kr_out, r_coef(1),                       &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s00_poisson%mat)
!
      call sel_r_mat_poisson_fixBC_sph                                  &
     &   (sph_rj, sph_bc, fdm2_center, band_s00_poisson)
      deallocate(r_coef)
!
      if(i_debug .gt. 0) write(*,*) 'const_radial_mat_scalar00_sph'
!
!
      call ludcmp_3band_ctr(band_s00_poisson)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_center_band_matrix(my_rank, sph_rj, band_s00_poisson)
      end if
!
      end subroutine const_r_mat00_poisson_fixS
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_scalar_sph
