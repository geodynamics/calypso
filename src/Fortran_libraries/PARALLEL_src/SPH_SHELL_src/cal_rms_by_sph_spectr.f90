!>@file   cal_rms_by_sph_spectr.f90
!!@brief  module cal_rms_by_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Evaluate mean square data for each spherical harmonics mode
!!
!!@verbatim
!!      subroutine cal_rms_sph_spec_one_mode                            &
!!     &         (j, sph_rj, ipol, ncomp_rj, g_sph_rj, icomp_rj,        &
!!     &          n_point, ntot_phys_rj, d_rj, rms_sph_r)
!!      subroutine cal_rms_sph_spec_one_field(sph_rj, ncomp_rj,         &
!!     &          g_sph_rj, icomp1_rj, icomp2_rj,                       &
!!     &          n_point, ntot_phys_rj, d_rj, rms_sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!        (1/4\pi) \int (\bf{u}_{l}^{m})^2 sin \theta d\theta d\phi
!!          = r^{-2} [ l(l+1) / (2l+1) 
!!           ( l(l+1)/r^2 (S_{l}^{m})^2 + (dS_{l}^{m}/dr)^2)
!!            + (T_{l}^{m})^2 ) ]
!!
!!      subroutine cvt_mag_or_kin_ene_one_point                         &
!!     &         (ipol_base, i_fld, rms_out)
!!      subroutine cvt_mag_or_kin_ene_one_mode                          &
!!     &         (sph_rj, ipol_base, icomp_rj, rms_sph_r)
!!      subroutine cvt_mag_or_kin_ene_spectr                            &
!!     &         (sph_rj, ipol_base, icomp_rj, rms_sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!@endverbatim
!!
!!@n @param  d_rj         spectrum data
!!@n @param  rms_sph_rj   mean square data
!
      module cal_rms_by_sph_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_one_mode                              &
     &         (j, sph_rj, ipol, ncomp_rj, g_sph_rj, icomp_rj,          &
     &          n_point, ntot_phys_rj, d_rj, rms_sph_r)
!
      use t_spheric_rj_data
      use t_phys_address
      use m_phys_constants
      use cal_sph_mean_square
!
      integer(kind = kint), intent(in) :: j
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: ncomp_rj, icomp_rj
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_r(0:sph_rj%nidx_rj(1),ncomp_rj)
!
!
      if     (ncomp_rj .eq. n_scalar) then
        call one_mode_scalar_mean_square                                &
     &     (j, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                    &
     &      sph_rj%inod_rj_center, sph_rj%radius_1d_rj_r,               &
     &      g_sph_rj(j,11), n_point, d_rj, rms_sph_r(0,1))
      else if(ncomp_rj .eq. n_vector) then
        if(j .eq. sph_rj%idx_rj_degree_zero) then
          call degree_zero_vector_mean_square                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        sph_rj%a_r_1d_rj_r, n_point, d_rj, rms_sph_r(0,1))
        else
          call one_mode_vector_mean_square                              &
     &       (j, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                  &
     &        sph_rj%a_r_1d_rj_r, g_sph_rj(j,3), g_sph_rj(j,12),        &
     &        n_point, d_rj(1,icomp_rj), rms_sph_r(0,1))
        end if
!
        call cvt_mag_or_kin_ene_one_mode                                &
     &     (sph_rj, ipol%base, icomp_rj, rms_sph_r(0,1))
      end if
!
      end subroutine cal_rms_sph_spec_one_mode
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_one_field(sph_rj, ncomp_rj,           &
     &          g_sph_rj, icomp1_rj, icomp2_rj,                         &
     &          n_point, ntot_phys_rj, d_rj, rms_sph_rj)
!
      use t_spheric_rj_data
      use t_phys_address
      use m_phys_constants
      use cal_sph_mean_square
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: ncomp_rj
      integer(kind = kint), intent(in) :: icomp1_rj, icomp2_rj
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),ncomp_rj)
!
!
      if     (ncomp_rj .eq. n_scalar) then
        call each_scalar_sph_spec(sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), &
     &      sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,           &
     &      sph_rj%radius_1d_rj_r, g_sph_rj, n_point,                   &
     &      d_rj(1,icomp1_rj), d_rj(1,icomp2_rj), rms_sph_rj(0,1,1))
      else if(ncomp_rj .eq. n_vector) then
        call each_vector_sph_spec(sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), &
     &      sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,           &
     &      sph_rj%a_r_1d_rj_r, g_sph_rj, n_point,                      &
     &      d_rj(1,icomp1_rj), d_rj(1,icomp2_rj), rms_sph_rj(0,1,1))
      end if
!
      end subroutine cal_rms_sph_spec_one_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_mag_or_kin_ene_one_point                           &
     &         (ipol_base, i_fld, rms_out)
!
      use t_base_field_labels
      use single_pt_sph_mean_square
!
      integer(kind = kint), intent(in) :: i_fld
      type(base_field_address), intent(in) :: ipol_base
!
      real(kind=kreal), intent(inout) :: rms_out(3)
!
!
        if (     i_fld .eq. ipol_base%i_velo                            &
     &      .or. i_fld .eq. ipol_base%i_magne) then
          call one_point_mean_sq_to_energy(rms_out(1))
        end if
!
      end subroutine cvt_mag_or_kin_ene_one_point
!
! -----------------------------------------------------------------------
!
      subroutine cvt_mag_or_kin_ene_one_mode                            &
     &         (sph_rj, ipol_base, icomp_rj, rms_sph_r)
!
      use t_spheric_rj_data
      use t_base_field_labels
      use cal_sph_mean_square
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(in) :: icomp_rj
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_r(0:sph_rj%nidx_rj(1),3)
!
!
      if (     icomp_rj .eq. ipol_base%i_velo                           &
     &    .or. icomp_rj .eq. ipol_base%i_magne) then
        call one_mode_mean_sq_to_energy                                 &
     &     (sph_rj%nidx_rj(1), rms_sph_r(0,1))
      end if
!
      end subroutine cvt_mag_or_kin_ene_one_mode
!
! -----------------------------------------------------------------------
!
      subroutine cvt_mag_or_kin_ene_spectr                              &
     &         (sph_rj, ipol_base, icomp_rj, rms_sph_rj)
!
      use t_spheric_rj_data
      use t_base_field_labels
      use cal_sph_mean_square
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(in) :: icomp_rj
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),3)
!
!
      if (     icomp_rj .eq. ipol_base%i_velo                           &
     &    .or. icomp_rj .eq. ipol_base%i_magne) then
        call one_field_mean_sq_to_energy                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), rms_sph_rj(0,1,1))
      end if
!
      end subroutine cvt_mag_or_kin_ene_spectr
!
! -----------------------------------------------------------------------
!
      end module cal_rms_by_sph_spectr
