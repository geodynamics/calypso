!>@file   cal_heat_source_Nu.f90
!!@brief      module cal_heat_source_Nu
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Evaluate Nusselt number without heat source
!!
!!@verbatim
!!      subroutine init_poisson_matrix_for_Nu                           &
!!     &         (mat_name, sph, r_2nd, sc_prop, sph_bc_S, fdm2_center, &
!!     &          band_s00_poisson_fixS, Nu_type)
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(band_matrix_type), intent(inout) :: band_s00_poisson_fixS
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!      subroutine sel_Nusselt_routine(is_scalar, is_source, is_grad_s, &
!!     &          sph, r_2nd, sc_prop, sph_bc_S, sph_bc_U, bcs_S,       &
!!     &          fdm2_center, band_s00_poisson_fixS, rj_fld, Nu_type)
!!        integer(kind = kint), intent(in) :: is_scalar, is_source
!!        integer(kind = kint), intent(in) :: is_grad_s
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), (in) :: sph_bc_S
!!        type(sph_boundary_type), (in) :: sph_bc_U
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(phys_data), intent(in) :: rj_fld
!!        type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!@endverbatim
!
      module cal_heat_source_Nu
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_no_heat_Nusselt
      use t_physical_property
      use t_fdm_coefs
      use t_sph_matrix
      use pickup_sph_spectr
!
      implicit  none
!
      private :: cal_no_heat_source_Nu, s_cal_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_poisson_matrix_for_Nu                             &
     &         (mat_name, sph, r_2nd, sc_prop, sph_bc_S, fdm2_center,   &
     &          band_s00_poisson_fixS, Nu_type)
!
      use t_fdm_coefs
      use t_coef_fdm2_MHD_boundaries
      use const_r_mat_4_scalar_sph
!
      character(len=kchara), intent(in) :: mat_name
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson_fixS
      type(nusselt_number_data), intent(inout) :: Nu_type
!
!
      if(Nu_type%iflag_Nusselt .eq. iflag_no_source_Nu) return
        call alloc_Nu_radial_reference(sph%sph_rj, Nu_type)
        call const_r_mat00_scalar_sph                                   &
     &     (mat_name, sc_prop%diffusie_reduction_ICB,                   &
     &      sph%sph_params, sph%sph_rj, r_2nd, sph_bc_S, fdm2_center,   &
     &      band_s00_poisson_fixS)
!
      end subroutine init_poisson_matrix_for_Nu
!
! -----------------------------------------------------------------------
!
      subroutine sel_Nusselt_routine(is_scalar, is_source, is_grad_s,   &
     &          sph, r_2nd, sc_prop, sph_bc_S, sph_bc_U, bcs_S,         &
     &          fdm2_center, band_s00_poisson_fixS, rj_fld, Nu_type)
!
      use t_fdm_coefs
      use t_coef_fdm2_MHD_boundaries
      use const_radial_references
!
      integer(kind = kint), intent(in) :: is_scalar, is_source
      integer(kind = kint), intent(in) :: is_grad_s
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S, sph_bc_U
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!
      type(nusselt_number_data), intent(inout) :: Nu_type
!
!
      if(Nu_type%iflag_Nusselt .eq. iflag_no_source_Nu) then
        if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
        call cal_no_heat_source_Nu(is_scalar, is_grad_s,                &
     &      sph%sph_rj, sph_bc_U, rj_fld, Nu_type)
      else if(Nu_type%iflag_Nusselt .eq. iflag_source_Nu) then
        call s_cal_heat_source_Nu(is_scalar, is_source, is_grad_s,      &
     &      sph%sph_rj, r_2nd, sc_prop, sph_bc_S, sph_bc_U, bcs_S,      &
     &      fdm2_center, rj_fld, band_s00_poisson_fixS, Nu_type)
      end if
!!
      end subroutine sel_Nusselt_routine
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_no_heat_source_Nu(is_scalar, is_grad_s,            &
     &          sph_rj, sph_bc_U, rj_fld, Nu_type)
!
      integer(kind = kint), intent(in) :: is_scalar, is_grad_s
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(in) :: rj_fld
!
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      real(kind = kreal) :: temp_ICB, temp_CMB
!      real(kind = kreal) :: dTdr_ICB, dTdr_CMB
      real(kind = kreal) :: c1, c2
!      real(kind = kreal) :: dTdr_diff_ICB, dTdr_diff_CMB
      integer(kind = kint) :: inod_ICB, inod_CMB
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
      Nu_type%kr_ICB_Nu = sph_bc_U%kr_in
      Nu_type%kr_CMB_Nu = sph_bc_U%kr_out
      Nu_type%r_ICB_Nu =  sph_bc_U%r_ICB(0)
      Nu_type%r_CMB_Nu =  sph_bc_U%r_CMB(0)
!
      inod_ICB = sph_rj%idx_rj_degree_zero                              &
     &          + (sph_bc_U%kr_in-1) * sph_rj%nidx_rj(2)
      temp_ICB = rj_fld%d_fld(inod_ICB,is_scalar)
!      dTdr_ICB = half*rj_fld%d_fld(inod_ICB,is_grad_s)                 &
!     &           * a_r_1d_rj_r(sph_bc_U%kr_in)**2
!
      inod_CMB = sph_rj%idx_rj_degree_zero                              &
     &          + (sph_bc_U%kr_out-1) * sph_rj%nidx_rj(2)
      temp_CMB = rj_fld%d_fld(inod_CMB,is_scalar)
!      dTdr_CMB = half*rj_fld%d_fld(inod_CMB,is_grad_s)                 &
!     &          * a_r_1d_rj_r(sph_bc_U%kr_out)**2
!
      c1 = (Nu_type%r_CMB_Nu*temp_CMB - Nu_type%r_ICB_Nu*temp_ICB)      &
     &    / ( Nu_type%r_CMB_Nu - Nu_type%r_ICB_Nu )
      c2 =  Nu_type%r_CMB_Nu * Nu_type%r_ICB_Nu * (temp_ICB - temp_CMB) &
     &    / ( Nu_type%r_CMB_Nu - Nu_type%r_ICB_Nu )
!
!      dTdr_diff_ICB = - c2 * a_r_1d_rj_r(sph_bc_U%kr_in)**2
!      dTdr_diff_CMB = - c2 * a_r_1d_rj_r(sph_bc_U%kr_out)**2
!      Nu_type%Nu_ICB = dTdr_ICB / dTdr_diff_ICB
!      Nu_type%Nu_CMB = dTdr_CMB / dTdr_diff_CMB
!
      Nu_type%Nu_ICB = - half*rj_fld%d_fld(inod_ICB,is_grad_s) / c2
      Nu_type%Nu_CMB = - half*rj_fld%d_fld(inod_CMB,is_grad_s) / c2
!
      end subroutine cal_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_heat_source_Nu(is_scalar, is_source, is_grad_s,  &
     &          sph_rj, r_2nd, sc_prop, sph_bc_S, sph_bc_U, bcs_S,      &
     &          fdm2_center, rj_fld, band_s00_poisson_fixS, Nu_type)
!
      use t_coef_fdm2_MHD_boundaries
      use const_radial_references
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: is_scalar, is_source
      integer(kind = kint), intent(in) :: is_grad_s
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S, sph_bc_U
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      integer(kind = kint) :: inod_ICB, inod_CMB
      character(len=kchara) :: file_name
!
!
!
      file_name = add_dat_extension(band_s00_poisson_fixS%mat_name)
      call const_diffusive_profile_fix_bc                               &
     &   (sph_rj, sc_prop, sph_bc_S, bcs_S, fdm2_center, r_2nd,         &
     &    band_s00_poisson_fixS, is_scalar, is_source, rj_fld,          &
     &    file_name, Nu_type%ref_global, Nu_type%ref_local)
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
      if(sph_bc_U%kr_in .eq. 0) then
        inod_ICB = sph_rj%inod_rj_center
        Nu_type%r_ICB_Nu = zero
      else
        inod_ICB = sph_rj%idx_rj_degree_zero                            &
     &            + (sph_bc_U%kr_in-1) * sph_rj%nidx_rj(2)
        Nu_type%r_ICB_Nu = sph_bc_U%r_ICB(0)
      end if
      inod_CMB = sph_rj%idx_rj_degree_zero                              &
     &          + (sph_bc_U%kr_out-1) * sph_rj%nidx_rj(2)
      Nu_type%r_CMB_Nu = sph_bc_U%r_CMB(0)
!
      if(Nu_type%ref_global(sph_bc_U%kr_in,1) .eq. zero) then
        Nu_type%Nu_ICB = -1.0d0
      else
        Nu_type%Nu_ICB =   half*rj_fld%d_fld(inod_ICB,is_grad_s)        &
     &                    * sph_rj%a_r_1d_rj_r(sph_bc_U%kr_in)**2       &
     &                    / Nu_type%ref_global(sph_bc_U%kr_in,1)
      end if
!
      if(Nu_type%ref_global(sph_bc_U%kr_out,1) .eq. zero) then
        Nu_type%Nu_CMB = -1.0d0
      else
        Nu_type%Nu_CMB =   half*rj_fld%d_fld(inod_CMB,is_grad_s)        &
     &                    * sph_rj%a_r_1d_rj_r(sph_bc_U%kr_out)**2      &
     &                    / Nu_type%ref_global(sph_bc_U%kr_out,1)
      end if
!
      end subroutine s_cal_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      end module cal_heat_source_Nu
