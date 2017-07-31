!> @file  set_reference_sph_mhd.f90
!!      module set_reference_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Sep., 2007
!
!> @brief Convert temperature data using reference temperature
!!
!!@verbatim
!!      subroutine adjust_by_ave_pressure_on_CMB(kr_in, kr_out,         &
!!     &          idx_rj_degree_zero, nidx_rj, i_press,                 &
!!     &          n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine adjust_sph_temp_bc_by_reftemp                        &
!!     &         (idx_rj_degree_zero, nri, reftemp_rj, sph_bc_S)
!!
!!      subroutine chenge_temp_to_per_temp_sph(idx_rj_degree_zero,      &
!!     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                  &
!!     &          is_temp, is_grad_t, ids_grad_t,                       &
!!     &          is_par_temp, is_grad_part_t, ids_grad_part_t,         &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        d_rj(inod,ipol%i_temp):        T => \Theta = T - T0
!!        d_rj(inod,is_par_temp):    \Theta = T - T0
!!        d_rj(inod,is_grad_t):      T => d \Theta / dr
!!        d_rj(inod,is_grad_part_t): d \Theta / dr
!!
!!      subroutine transfer_per_temp_to_temp_sph(idx_rj_degree_zero,    &
!!     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                  &
!!     &          is_temp, is_grad_t, ids_grad_t,                       &
!!     &          is_par_temp, is_grad_part_t, ids_grad_part_t,         &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol, idpdr
!!        d_rj(inod,ipol%i_temp):        \Theta = T - T0 => T
!!        d_rj(inod,is_par_temp):    \Theta = T - T0
!!        d_rj(inod,is_grad_t):      d \Theta / dr   => dT / dr
!!        d_rj(inod,is_grad_part_t): d \Theta / dr
!!
!!      subroutine delete_zero_degree_vect(is_fld, idx_rj_degree_zero,  &
!!     &          n_point, nidx_rj, ntot_phys_rj, d_rj)
!!      subroutine delete_zero_degree_comp(is_fld, idx_rj_degree_zero,  &
!!     &          n_point, nidx_rj, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@param sph_bc_S  Structure for basic boundary condition parameters
!!                 for scalar
!!@n @param is_fld Address of poloidal component
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_reference_sph_mhd
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use t_phys_address
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine adjust_by_ave_pressure_on_CMB(kr_in, kr_out,           &
     &          idx_rj_degree_zero, nidx_rj, i_press,                   &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: i_press
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
      real(kind = kreal) :: ref_p
!
!
      if (idx_rj_degree_zero .eq. 0) return
!
      inod = idx_rj_degree_zero + (kr_out-1)*nidx_rj(2)
      ref_p = d_rj(inod,i_press)
!
      do k = kr_in, kr_out
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,i_press) = d_rj(inod,i_press) - ref_p
      end do
!
      end subroutine adjust_by_ave_pressure_on_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_temp_bc_by_reftemp                          &
     &         (idx_rj_degree_zero, nri, reftemp_rj, sph_bc_S)
!
      use t_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri
      real(kind=kreal), intent(in) :: reftemp_rj(nri,0:2)
!
      type(sph_boundary_type), intent(inout) :: sph_bc_S
!
!
      if(idx_rj_degree_zero .gt. 0) then
        sph_bc_S%ICB_fld(idx_rj_degree_zero)                            &
     &   = sph_bc_S%ICB_fld(idx_rj_degree_zero)                         &
     &    - reftemp_rj(sph_bc_S%kr_in,0)
        sph_bc_S%CMB_fld(idx_rj_degree_zero)                            &
     &   = sph_bc_S%CMB_fld(idx_rj_degree_zero)                         &
     &     - reftemp_rj(sph_bc_S%kr_out,0)
        sph_bc_S%ICB_flux(idx_rj_degree_zero)                           &
     &   = sph_bc_S%ICB_flux(idx_rj_degree_zero)                        &
     &    - reftemp_rj(sph_bc_S%kr_in,1)
        sph_bc_S%CMB_flux(idx_rj_degree_zero)                           &
     &   = sph_bc_S%CMB_flux(idx_rj_degree_zero)                        &
     &    - reftemp_rj(sph_bc_S%kr_out,1)
      end if
!
      end subroutine adjust_sph_temp_bc_by_reftemp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine chenge_temp_to_per_temp_sph(idx_rj_degree_zero,        &
     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                    &
     &          is_temp, is_grad_t, ids_grad_t,                         &
     &          is_par_temp, is_grad_part_t, ids_grad_part_t,           &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      integer(kind = kint), intent(in) :: is_temp, is_par_temp
      integer(kind = kint), intent(in) :: is_grad_t, is_grad_part_t
      integer(kind = kint), intent(in) :: ids_grad_t, ids_grad_part_t
!
      real(kind=kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:2)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,is_temp) = d_rj(inod,is_temp) - reftemp_rj(k,0)
          d_rj(inod,is_grad_t) = d_rj(inod,is_grad_t)                   &
     &                 - two*reftemp_rj(k,1) * radius_1d_rj_r(k)**2
          d_rj(inod,ids_grad_t) = d_rj(inod,ids_grad_t)                 &
     &                 - (reftemp_rj(k,2) * radius_1d_rj_r(k)           &
     &                  + two*reftemp_rj(k,1)) * two*radius_1d_rj_r(k)
        end do
      end if
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,is_par_temp) =     d_rj(inod,is_temp)
        d_rj(inod,is_grad_part_t) =  d_rj(inod,is_grad_t)
        d_rj(inod,ids_grad_part_t) = d_rj(inod,ids_grad_t)
      end do
!$omp end parallel do
!
      end subroutine chenge_temp_to_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine transfer_per_temp_to_temp_sph(idx_rj_degree_zero,      &
     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                    &
     &          is_temp, is_grad_t, ids_grad_t,                         &
     &          is_par_temp, is_grad_part_t, ids_grad_part_t,           &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      integer(kind = kint), intent(in) :: is_temp, is_par_temp
      integer(kind = kint), intent(in) :: is_grad_t, is_grad_part_t
      integer(kind = kint), intent(in) :: ids_grad_t, ids_grad_part_t
!
      real(kind=kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:2)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,is_par_temp) =     d_rj(inod,is_temp)
        d_rj(inod,is_grad_part_t) =  d_rj(inod,is_grad_t)
        d_rj(inod,ids_grad_part_t) = d_rj(inod,ids_grad_t)
      end do
!$omp end parallel do
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,is_temp) = d_rj(inod,is_temp) + reftemp_rj(k,0)
          d_rj(inod,is_grad_t) = d_rj(inod,is_grad_part_t)              &
     &                 + two*reftemp_rj(k,1) * radius_1d_rj_r(k)**2
          d_rj(inod,ids_grad_t) = d_rj(inod,ids_grad_part_t)            &
     &                 + (reftemp_rj(k,2) * radius_1d_rj_r(k)           &
     &                  + two*reftemp_rj(k,1)) * two*radius_1d_rj_r(k)
        end do
      end if
!
      end subroutine transfer_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine delete_zero_degree_vect(is_fld, idx_rj_degree_zero,    &
     &          n_point, nidx_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, nidx_rj(2)
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
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
      end subroutine delete_zero_degree_vect
!
! -----------------------------------------------------------------------
!
      subroutine delete_zero_degree_comp(is_fld, idx_rj_degree_zero,    &
     &          n_point, nidx_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, nidx_rj(2)
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (idx_rj_degree_zero .eq. 0) return
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,is_fld  ) = zero
      end do
!
      end subroutine delete_zero_degree_comp
!
! -----------------------------------------------------------------------
!
      end module set_reference_sph_mhd

