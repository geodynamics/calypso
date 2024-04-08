!> @file  cal_lorentz_by_dipole.f90
!!      module cal_lorentz_by_dipole
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate work of Lorentz force by dipole magnetic field
!!
!!@verbatim
!!      subroutine s_cal_lorentz_by_dipole(sph_rtp, fl_prop,            &
!!     &          bs_trns, fs_trns, trns_b_snap, trns_f_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: bs_trns, fs_trns
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(inout) :: trns_f_snap
!!@endverbatim
!
      module cal_lorentz_by_dipole
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_physical_property
      use t_addresses_sph_transform

      implicit  none
!
      private :: lorentz_frc_and_work_by_dipole
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_lorentz_by_dipole(sph_rtp, fl_prop,              &
     &          bs_trns, fs_trns, trns_b_snap, trns_f_snap)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: bs_trns, fs_trns
      type(spherical_transform_data), intent(in) :: trns_b_snap
!
      type(spherical_transform_data), intent(inout) :: trns_f_snap
!
!
      call lorentz_frc_and_work_by_dipole(fl_prop, bs_trns%base,        &
     &    bs_trns%prod_fld, fs_trns%prod_fld, sph_rtp%nnod_rtp,         &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp)
!
      call lorentz_frc_and_work_by_dipole(fl_prop, bs_trns%base,        &
     &      bs_trns%prod_fld, fs_trns%prod_fld, sph_rtp%nnod_pole,      &
     &      trns_b_snap%ncomp, trns_b_snap%fld_pole,                    &
     &      trns_f_snap%ncomp, trns_f_snap%fld_pole)
!
      end subroutine s_cal_lorentz_by_dipole
!
!-----------------------------------------------------------------------
!
      subroutine lorentz_frc_and_work_by_dipole                         &
     &         (fl_prop, bs_trns_base, bs_trns_prod, fs_trns_prod,      &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_flx, frc_rtp)
!
      use cal_products_smp
!
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: bs_trns_base
      type(phys_products_address), intent(in) :: bs_trns_prod
      type(phys_products_address), intent(in) :: fs_trns_prod
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod,ntot_comp_flx)
!
!$omp parallel
      if(fs_trns_prod%i_dipole_Lorentz .gt. 0) then
        call cal_cross_prod_w_coef_smp(nnod, fl_prop%coef_lor,          &
     &      fld_rtp(1,bs_trns_prod%i_dipole_J),                         &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      frc_rtp(1,fs_trns_prod%i_dipole_Lorentz) )
      end if
!
      if(fs_trns_prod%i_dipole_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,fs_trns_prod%i_dipole_Lorentz),                   &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      frc_rtp(1,fs_trns_prod%i_dipole_ujb) )
      end if
!$omp end parallel
!
      end subroutine lorentz_frc_and_work_by_dipole
!
!-----------------------------------------------------------------------
!
      end module cal_lorentz_by_dipole
