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
!!     &         (idx_rj_degree_zero, nri, reftemp_rj, sph_bc,          &
!!     &          ICB_Sspec, CMB_Sspec, ICB_Sevo, CMB_Sevo)
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(inout) :: ICB_Sspec, CMB_Sspec
!!        type(sph_scalar_BC_evo), intent(inout) :: ICB_Sevo, CMB_Sevo
!!
!!      subroutine chenge_temp_to_per_temp_sph                          &
!!     &         (idx_rj_degree_zero, inod_rj_center,                   &
!!     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                  &
!!     &          is_temp, is_grad_t, ids_grad_t,                       &
!!     &          is_par_temp, is_grad_part_t, ids_grad_part_t,         &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        d_rj(inod,is_tempp):        T => \Theta = T - T0
!!        d_rj(inod,is_par_temp):    \Theta = T - T0
!!        d_rj(inod,is_grad_t):      T => d \Theta / dr
!!        d_rj(inod,is_grad_part_t): d \Theta / dr
!!
!!      subroutine transfer_per_temp_to_temp_sph                        &
!!     &         (idx_rj_degree_zero, inod_rj_center,                   &
!!     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                  &
!!     &          is_temp, is_grad_t, ids_grad_t,                       &
!!     &          is_par_temp, is_grad_part_t, ids_grad_part_t,         &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        d_rj(inod,is_temp):        \Theta = T - T0 => T
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
     &         (idx_rj_degree_zero, nri, reftemp_rj, sph_bc,            &
     &          ICB_Sspec, CMB_Sspec, ICB_Sevo, CMB_Sevo)
!
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri
      real(kind=kreal), intent(in) :: reftemp_rj(0:nri,0:1)
      type(sph_boundary_type), intent(in) :: sph_bc
!
      type(sph_scalar_BC_coef), intent(inout) :: ICB_Sspec, CMB_Sspec
      type(sph_scalar_BC_evo), intent(inout) :: ICB_Sevo, CMB_Sevo
!
      integer(kind = kint) :: j
!
!
      if(idx_rj_degree_zero .eq. 0) return
      j = idx_rj_degree_zero
!
!      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
      if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        ICB_Sspec%S_CTR = ICB_Sspec%S_CTR - reftemp_rj(0,0)
      else if(sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        ICB_Sevo%S_BC_mag(j) = ICB_Sevo%S_BC_mag(j)                     &
     &                        - reftemp_rj(sph_bc%kr_in,1)
      else if(sph_bc%iflag_icb .eq. iflag_evolve_field) then
        ICB_Sevo%S_BC_mag(j) = ICB_Sevo%S_BC_mag(j)                     &
     &                        - reftemp_rj(sph_bc%kr_in,0)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        ICB_Sspec%S_BC(j) = ICB_Sspec%S_BC(j)                           &
     &                        - reftemp_rj(sph_bc%kr_in,1)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_field) then
        ICB_Sspec%S_BC(j) = ICB_Sspec%S_BC(j)                           &
     &                        - reftemp_rj(sph_bc%kr_in,0)
      end if
!
!
      if(sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        CMB_Sevo%S_BC_mag(j) = CMB_Sevo%S_BC_mag(j)                     &
     &                        - reftemp_rj(sph_bc%kr_out,1)
      else if(sph_bc%iflag_cmb .eq. iflag_evolve_field) then
        CMB_Sevo%S_BC_mag(j) = CMB_Sevo%S_BC_mag(j)                     &
     &                        - reftemp_rj(sph_bc%kr_out,0)
      else if(sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        CMB_Sspec%S_BC(j) = CMB_Sspec%S_BC(j)                           &
     &                        - reftemp_rj(sph_bc%kr_out,1)
      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field) then
        CMB_Sspec%S_BC(j) = CMB_Sspec%S_BC(j)                           &
     &                        - reftemp_rj(sph_bc%kr_out,0)
      end if
!
      end subroutine adjust_sph_temp_bc_by_reftemp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine chenge_temp_to_per_temp_sph                            &
     &         (idx_rj_degree_zero, inod_rj_center,                     &
     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                    &
     &          is_temp, is_grad_t, ids_grad_t,                         &
     &          is_par_temp, is_grad_part_t, ids_grad_part_t,           &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      integer(kind = kint), intent(in) :: is_temp, is_par_temp
      integer(kind = kint), intent(in) :: is_grad_t, is_grad_part_t
      integer(kind = kint), intent(in) :: ids_grad_t, ids_grad_part_t
!
      real(kind=kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind=kreal), intent(in) :: reftemp_rj(0:nidx_rj(1),0:1)
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
      if(inod_rj_center .gt. 0) then
        d_rj(inod_rj_center,is_temp) = d_rj(inod_rj_center,is_temp)     &
     &                                - reftemp_rj(0,0)
        d_rj(inod_rj_center,is_par_temp) = d_rj(inod_rj_center,is_temp)
      end if
!
      end subroutine chenge_temp_to_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine transfer_per_temp_to_temp_sph                          &
     &         (idx_rj_degree_zero, inod_rj_center,                     &
     &          nidx_rj, radius_1d_rj_r, reftemp_rj,                    &
     &          is_temp, is_grad_t, ids_grad_t,                         &
     &          is_par_temp, is_grad_part_t, ids_grad_part_t,           &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      integer(kind = kint), intent(in) :: is_temp, is_par_temp
      integer(kind = kint), intent(in) :: is_grad_t, is_grad_part_t
      integer(kind = kint), intent(in) :: ids_grad_t, ids_grad_part_t
!
      real(kind=kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind=kreal), intent(in) :: reftemp_rj(0:nidx_rj(1),0:1)
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
        end do
      end if
!
      if(inod_rj_center .gt. 0) then
        d_rj(inod_rj_center,is_par_temp) = d_rj(inod_rj_center,is_temp)
        d_rj(inod_rj_center,is_temp) = d_rj(inod_rj_center,is_temp)     &
     &                                + reftemp_rj(0,0)
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

