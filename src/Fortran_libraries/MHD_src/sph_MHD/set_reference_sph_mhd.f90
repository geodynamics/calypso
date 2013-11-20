!> @file  set_reference_sph_mhd.f90
!!      module set_reference_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Sep., 2007
!
!> @brief Convert temperature data using reference temperature
!!
!!@verbatim
!!      subroutine adjust_by_ave_pressure_on_CMB(kr_out)
!!
!!      subroutine set_ref_temp_sph_mhd(sph_bc_T)
!!      subroutine adjust_sph_temp_bc_by_reftemp(sph_bc_T)
!!
!!      subroutine sync_temp_by_per_temp_sph
!!        d_rj(inod,ipol%i_temp):        T => \Theta = T - T0
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      T => d \Theta / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!
!!
!!      subroutine trans_per_temp_to_temp_sph
!!        d_rj(inod,ipol%i_temp):        \Theta = T - T0 => T
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      d \Theta / dr   => dT / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!
!!      subroutine delete_zero_degree_comp(is_fld)
!!@endverbatim
!!
!!@param sph_bc_T  Structure for basic boundary condition parameters
!!                 for temperature
!!@n @param is_fld Address of poloidal component
!
      module set_reference_sph_mhd
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_control_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine adjust_by_ave_pressure_on_CMB(kr_out)
!
      use m_sph_phys_address
!
      integer(kind = kint), intent(in) :: kr_out
!
      integer(kind = kint) :: k, inod
      real(kind = kreal) :: ref_p
!
!
      if (idx_rj_degree_zero .eq. 0) return
!
      inod = idx_rj_degree_zero + (kr_out-1)*nidx_rj(2)
      ref_p = d_rj(inod,ipol%i_press)
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,ipol%i_press) = d_rj(inod,ipol%i_press) - ref_p
      end do
!
      end subroutine adjust_by_ave_pressure_on_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ref_temp_sph_mhd(sph_bc_T)
!
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
!
      integer (kind = kint) :: k
!
! set reference temperature (for spherical shell)
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        do k = 1, sph_bc_T%kr_in-1
          reftemp_rj(k,0) = high_temp
          reftemp_rj(k,1) = zero
        end do
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          reftemp_rj(k,0) = (depth_high_t*depth_low_t*ar_1d_rj(k,1)     &
     &                   * (high_temp - low_temp)                       &
     &                    - depth_high_t*high_temp                      &
     &                    + depth_low_t* low_temp )                     &
     &                     / (depth_low_t - depth_high_t)
          reftemp_rj(k,1) = - depth_high_t*depth_low_t*ar_1d_rj(k,2)    &
     &                   * (high_temp - low_temp)                       &
     &                     / (depth_low_t - depth_high_t)
        end do
        do k = sph_bc_T%kr_out+1, nidx_rj(1)
          reftemp_rj(k,0) = low_temp
          reftemp_rj(k,1) = zero
        end do
      else
        reftemp_rj(1:nidx_rj(1),0) = zero
        reftemp_rj(1:nidx_rj(1),1) = zero
        depth_high_t = r_ICB
        depth_low_t =  r_CMB
      end if
!
      end subroutine set_ref_temp_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_temp_bc_by_reftemp(sph_bc_T)
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
!
!
      if(idx_rj_degree_zero .gt. 0                                      &
     &      .and. iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        sph_bc_T%ICB_fld(idx_rj_degree_zero)                            &
     &   = sph_bc_T%ICB_fld(idx_rj_degree_zero)                         &
     &    - reftemp_rj(sph_bc_T%kr_in,0)
        sph_bc_T%CMB_fld(idx_rj_degree_zero)                            &
     &   = sph_bc_T%CMB_fld(idx_rj_degree_zero)                         &
     &     - reftemp_rj(sph_bc_T%kr_out,0)
        sph_bc_T%ICB_flux(idx_rj_degree_zero)                           &
     &   = sph_bc_T%ICB_flux(idx_rj_degree_zero)                        &
     &    - reftemp_rj(sph_bc_T%kr_in,1)
        sph_bc_T%CMB_flux(idx_rj_degree_zero)                           &
     &   = sph_bc_T%CMB_flux(idx_rj_degree_zero)                        &
     &    - reftemp_rj(sph_bc_T%kr_out,1)
      end if
!
      end subroutine adjust_sph_temp_bc_by_reftemp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sync_temp_by_per_temp_sph
!
      use m_sph_phys_address
!
      integer(kind = kint) :: k, inod
!
!
      if (iflag_4_ref_temp .ne. id_sphere_ref_temp) return
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp)               &
     &                    - reftemp_rj(k,0)
          d_rj(inod,ipol%i_grad_t) = d_rj(inod,ipol%i_grad_t)           &
     &                    - reftemp_rj(k,1) * radius_1d_rj_r(k)**2
          d_rj(inod,idpdr%i_grad_t) = d_rj(inod,ipol%i_temp)
        end do
      end if
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_par_temp) =    d_rj(inod,ipol%i_temp)
        d_rj(inod,ipol%i_grad_part_t) = d_rj(inod,ipol%i_grad_t)
      end do
!$omp end parallel do
!
      end subroutine sync_temp_by_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine trans_per_temp_to_temp_sph
!
      use m_sph_phys_address
!
      integer(kind = kint) :: k, inod
!
!
      if (iflag_4_ref_temp .ne. id_sphere_ref_temp) return
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_par_temp) =    d_rj(inod,ipol%i_temp)
        d_rj(inod,ipol%i_grad_part_t) = d_rj(inod,ipol%i_grad_t)
      end do
!$omp end parallel do
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp)               &
     &                            + reftemp_rj(k,0)
          d_rj(inod,ipol%i_grad_t) = d_rj(inod,ipol%i_grad_part_t)      &
     &                 + reftemp_rj(k,1) * radius_1d_rj_r(k)**2
          d_rj(inod,idpdr%i_grad_t) = d_rj(inod,ipol%i_temp)
        end do
      end if
!
      end subroutine trans_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine delete_zero_degree_comp(is_fld)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint) :: k, inod
!
!
      if (idx_rj_degree_zero .eq. 0) return
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+2) = zero
      end do
!
      end subroutine delete_zero_degree_comp
!
! -----------------------------------------------------------------------
!
      end module set_reference_sph_mhd

