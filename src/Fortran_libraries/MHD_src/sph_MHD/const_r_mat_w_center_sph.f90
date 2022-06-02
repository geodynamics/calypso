!>@file   const_r_mat_w_center_sph.f90
!!@brief  module const_r_mat_w_center_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_radial_mat_press00_sph                         &
!!     &         (mat_name, sph_rj, fl_prop, sph_bc_U, fdm2_center,     &
!!     &          band_p_poisson, band_p00_poisson)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(band_matrices_type), intent(in) :: band_p_poisson
!!        type(band_matrix_type), intent(inout) :: band_p00_poisson
!!      subroutine const_radial_mat_scalar00_sph(mat_name, sph_rj,      &
!!     &          prop, sph_bc, fdm2_center, evo_mat, band_s00_evo)
!!      subroutine const_rmat_poisson00_sph(mat_name, sph_rj,           &
!!     &          poisson_mat, band_s00_poisson)
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(band_matrices_type), save :: evo_mat
!!        type(band_matrix_type), intent(inout) :: band_s00_evo
!!@endverbatim
!
      module const_r_mat_w_center_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_spheric_rj_data
      use t_sph_center_matrix
      use t_physical_property
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
      private :: const_rmat_press00_sph, const_rmat_scalar00_sph
      private :: copy_radial_mat_scalar00_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_press00_sph                           &
     &         (mat_name, sph_rj, fl_prop, sph_bc_U, fdm2_center,       &
     &          band_p_poisson, band_p00_poisson)
!
      use t_sph_matrices
      use t_sph_matrix
!
      character(len=kchara), intent(in) :: mat_name
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrices_type), intent(in) :: band_p_poisson
!
      type(band_matrix_type), intent(inout) :: band_p00_poisson
!
!
      if (fl_prop%iflag_scheme .lt. id_Crank_nicolson) return
!
      if(i_debug .gt. 0) write(*,*) 'const_rmat_press00_sph'
      write(band_p00_poisson%mat_name,'(a)') trim(mat_name)
      call const_rmat_press00_sph                                       &
     &   (sph_rj, sph_bc_U, fdm2_center, fl_prop,                       &
     &    band_p_poisson%n_vect, band_p_poisson%n_comp,                 &
     &    band_p_poisson%mat, band_p00_poisson)
!
      end subroutine const_radial_mat_press00_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_scalar00_sph(mat_name, dt, sph_rj,    &
     &          prop, sph_bc, fdm2_center, evo_mat, band_s00_evo)
!
      use t_sph_matrices
      use t_sph_matrix
!
      character(len=kchara), intent(in) :: mat_name
      real(kind = kreal), intent(in) :: dt
      type(scalar_property), intent(in) :: prop
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrices_type), intent(in) :: evo_mat
!
      type(band_matrix_type), intent(inout) :: band_s00_evo
!
      real(kind = kreal) :: coef
!
!
      if (prop%iflag_scheme .lt. id_Crank_nicolson) return
!
      if(prop%coef_advect .eq. zero) then
        coef = one
      else  
        coef = prop%coef_imp * prop%coef_diffuse * dt
      end if
!
      if(i_debug .gt. 0) write(*,*) 'const_rmat_scalar00_sph'
      write(band_s00_evo%mat_name,'(2a)') trim(mat_name), '_evo_l0'
      call const_rmat_scalar00_sph(sph_rj, fdm2_center,                 &
     &    sph_bc%iflag_icb, sph_bc%r_ICB, prop%coef_advect, coef,       &
     &    evo_mat%n_vect, evo_mat%n_comp, evo_mat%mat, band_s00_evo)
!
      end subroutine const_radial_mat_scalar00_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_rmat_poisson00_sph(mat_name, sph_rj,             &
     &          sph_bc, fdm2_center, poisson_mat, band_s00_poisson)
!
      use t_sph_matrices
      use t_sph_matrix
!
      character(len=kchara), intent(in) :: mat_name
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrices_type), intent(in) :: poisson_mat
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
!
      write(band_s00_poisson%mat_name,'(2a)')                           &
     &                                trim(mat_name), '_poisson_l0'
      if(sph_rj%idx_rj_degree_zero .eq. 0) then
        if(i_debug .gt. 0) write(*,*) 'copy_radial_mat_scalar00_sph'
        call copy_radial_mat_scalar00_sph(sph_rj,                       &
     &      poisson_mat%n_vect, poisson_mat%n_comp, poisson_mat%mat,    &
     &      band_s00_poisson)
      else
        if(i_debug .gt. 0) write(*,*) 'const_radial_mat_scalar00_sph'
        call const_rmat_scalar00_sph(sph_rj, fdm2_center,               &
     &      sph_bc%iflag_icb, sph_bc%r_ICB, zero, one,                  &
     &      poisson_mat%n_vect, poisson_mat%n_comp, poisson_mat%mat,    &
     &      band_s00_poisson)
      end if
      call check_center_band_matrix(6, sph_rj, band_s00_poisson)
!
      end subroutine const_rmat_poisson00_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_rmat_press00_sph                                 &
     &         (sph_rj, sph_bc_U, fdm2_center, fl_prop,                 &
     &           n_vect, n_comp, p_poisson_mat, band_p00_poisson)
!
      use m_ludcmp_3band
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      integer(kind= kint), intent(in) :: n_vect, n_comp
      real(kind = kreal), intent(in) :: p_poisson_mat(3,n_vect,n_comp)
!
      type(band_matrix_type), intent(inout) :: band_p00_poisson
!
      real(kind = kreal) :: coef_p
!
!
      coef_p = - fl_prop%coef_press
!
!      write(*,*) 'alloc_ctr_band_mat'
      call alloc_ctr_band_mat(ithree, sph_rj, band_p00_poisson)
!
!      write(*,*) 'copy_to_band3_mat_w_center'
      call copy_to_band3_mat_w_center(sph_rj%nidx_rj(1), zero,          &
      &   p_poisson_mat(1,1,sph_rj%idx_rj_degree_zero),                 &
      &   band_p00_poisson%mat)
!
!      write(*,*) 'add_scalar_poisson_mat_fill_ctr'
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_fill_ctr                            &
     &     (sph_rj%nidx_rj(1), sph_bc_U%r_ICB,                          &
     &      fdm2_center%dmat_fix_dr, fdm2_center%dmat_fix_fld,          &
     &      coef_p, band_p00_poisson%mat)
      else
        call add_scalar_poisson_mat_no_fld                              &
     &     (sph_rj%nidx_rj(1), band_p00_poisson%mat)
      end if
!
!      write(*,*) 'ludcmp_3band_ctr'
      call ludcmp_3band_ctr(band_p00_poisson)
!
      if(i_debug .ne. iflag_full_msg) return
      call check_center_band_matrix(my_rank, sph_rj, band_p00_poisson)
!
      end subroutine const_rmat_press00_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_rmat_scalar00_sph(sph_rj, fdm2_center,           &
     &          iflag_icb, r_ICB, coef_advect, coef, n_vect, n_comp,    &
     &          evo_mat, band_s00_evo)
!
      use m_ludcmp_3band
      use t_boundary_params_sph_MHD
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      integer(kind= kint), intent(in) :: iflag_icb
      real(kind= kreal), intent(in) :: r_ICB(0:2)
      integer(kind= kint), intent(in) :: n_vect, n_comp
      real(kind = kreal), intent(in) :: coef_advect
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: evo_mat(3,n_vect,n_comp)
!
      type(band_matrix_type), intent(inout) :: band_s00_evo
!
!
!
      call alloc_ctr_band_mat(ithree, sph_rj, band_s00_evo)
      call set_unit_ctr_single_mat(band_s00_evo)
!
      call copy_to_band3_mat_w_center(sph_rj%nidx_rj(1), coef_advect,   &
     &   evo_mat(1,1,sph_rj%idx_rj_degree_zero), band_s00_evo%mat)
!
      if     (iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_fill_ctr(sph_rj%nidx_rj(1), r_ICB,  &
     &      fdm2_center%dmat_fix_dr, fdm2_center%dmat_fix_fld,          &
     &      coef, band_s00_evo%mat)
      else if(iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_fix_ctr(sph_rj%nidx_rj(1), r_ICB,   &
     &      fdm2_center%dmat_fix_fld, coef, band_s00_evo%mat(1,0))
      else
        call add_scalar_poisson_mat_no_fld                              &
     &     (sph_rj%nidx_rj(1), band_s00_evo%mat)
      end if
!
      call ludcmp_3band_ctr(band_s00_evo)
!
      if(i_debug .ne. iflag_full_msg) return
      call check_center_band_matrix(my_rank, sph_rj, band_s00_evo)
!
      end subroutine const_rmat_scalar00_sph
!
! -----------------------------------------------------------------------
!
      subroutine copy_radial_mat_scalar00_sph                           &
     &         (sph_rj, n_vect, n_comp, evo_mat, band_s00_poisson)
!
      use m_ludcmp_3band
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind= kint), intent(in) :: n_vect, n_comp
      real(kind = kreal), intent(in) :: evo_mat(3,n_vect,n_comp)
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
!
      call alloc_ctr_band_mat(ithree, sph_rj, band_s00_poisson)
      call copy_to_band3_mat_no_center(sph_rj%nidx_rj(1),               &
     &   evo_mat(1,1,sph_rj%idx_rj_degree_zero), band_s00_poisson%mat)
!
      call ludcmp_3band_ctr(band_s00_poisson)
!
      if(i_debug .ne. iflag_full_msg) return
      call check_center_band_matrix(my_rank, sph_rj, band_s00_poisson)
!
      end subroutine copy_radial_mat_scalar00_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_w_center_sph
