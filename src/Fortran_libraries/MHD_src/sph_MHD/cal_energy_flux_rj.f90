!> @file  cal_energy_flux_rj.f90
!!      module cal_energy_flux_rj
!!
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!!@n      Modified in July, 2021
!!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_energy_flux_rj                                 &
!!     &         (ltr_crust, sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_energy_flux_rj
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
!
      implicit  none
!
      private :: truncate_magnetic_field_4_view
      private :: copy_poloidal_to_streamfunc_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rj                                   &
     &         (ltr_crust, sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!
      use sph_radial_grad_4_velocity
      use copy_nodal_fields
!
      integer(kind = kint), intent(in) :: ltr_crust
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%rot_forces%i_Coriolis .gt. 0) then
        call const_grad_poloidal_moment                                 &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      ipol%rot_forces%i_Coriolis, rj_fld)
      end if
!
!
      if(ipol%prod_fld%i_geostrophic .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%forces%i_coriolis, ipol%forces%i_press_grad,   &
     &      ipol%prod_fld%i_geostrophic)
      end if
      if(ipol%prod_fld%i_truncated_B .gt. 0) then
        call truncate_magnetic_field_4_view(ltr_crust, sph_rj, rj_fld,  &
     &      ipol%base%i_magne, ipol%prod_fld%i_truncated_B)
      end if
!
!
      if(ipol%prod_fld%i_stream_pol_u .gt. 0) then
        call copy_poloidal_to_streamfunc_rj(sph_rj, rj_fld,             &
     &      ipol%base%i_velo, ipol%prod_fld%i_stream_pol_u)
      end if
!
!
      if(ipol%prod_fld%i_stream_pol_b .gt. 0) then
        call copy_poloidal_to_streamfunc_rj(sph_rj, rj_fld,             &
    &       ipol%base%i_magne, ipol%prod_fld%i_stream_pol_b)
      end if
!
      end subroutine s_cal_energy_flux_rj
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine truncate_magnetic_field_4_view(ltr_crust, sph_rj,      &
     &          rj_fld, i_magne, i_truncated_B)
!
      integer(kind = kint), intent(in) :: ltr_crust
      integer(kind = kint), intent(in) :: i_magne, i_truncated_B
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl
!
!$omp parallel do private(inod, l_gl)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        if(l_gl .le. ltr_crust) then
          rj_fld%d_fld(inod,i_truncated_B  )                            &
     &          = rj_fld%d_fld(inod,i_magne  )
          rj_fld%d_fld(inod,i_truncated_B+1)                            &
     &          = rj_fld%d_fld(inod,i_magne+1)
          rj_fld%d_fld(inod,i_truncated_B+2)                            &
     &          = rj_fld%d_fld(inod,i_magne+2)
        else
          rj_fld%d_fld(inod,i_truncated_B  ) = zero
          rj_fld%d_fld(inod,i_truncated_B+1) = zero
          rj_fld%d_fld(inod,i_truncated_B+2) = zero
        end if
      end do
!$omp end parallel do
!
      end subroutine truncate_magnetic_field_4_view
!
!-----------------------------------------------------------------------
!
      subroutine copy_poloidal_to_streamfunc_rj(sph_rj, rj_fld,         &
     &                                          is_fld, is_stream)
!
      integer(kind = kint), intent(in) :: is_fld, is_stream
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod
!
!$omp parallel do private(inod)
      do inod = 1, sph_rj%nnod_rj
        rj_fld%d_fld(inod,is_stream  ) = zero
        rj_fld%d_fld(inod,is_stream+1) = zero
        rj_fld%d_fld(inod,is_stream+2) = rj_fld%d_fld(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine copy_poloidal_to_streamfunc_rj
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_rj
