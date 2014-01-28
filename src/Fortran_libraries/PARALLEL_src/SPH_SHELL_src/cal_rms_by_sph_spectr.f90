!>@file   cal_rms_by_sph_spectr.f90
!!@brief  module cal_rms_by_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Evaluate mean square data for each spherical harmonics mode
!!
!!@verbatim
!!      subroutine cal_rms_each_scalar_sph_spec(icomp, icou)
!!      subroutine cal_rms_each_vector_sph_spec(icomp, icou)
!!        (1/4\pi) \int (\bf{u}_{l}^{m})^2 sin \theta d\theta d\phi
!!          = r^{-2} [ l(l+1) / (2l+1) 
!!           ( l(l+1)/r^2 (S_{l}^{m})^2 + (dS_{l}^{m}/dr)^2)
!!            + (T_{l}^{m})^2 ) ]
!!      subroutine set_sph_energies_by_rms(icou)
!!
!!      subroutine cal_ave_scalar_sph_spectr(icomp, icou)
!!      subroutine cal_ave_vector_sph_spectr(icou)
!!@endverbatim
!!
!!@n @param  icomp   Address of spectrum data
!!@n @param  icou    Address of mean square data
!
      module cal_rms_by_sph_spectr
!
      use m_precision
!
      use m_constants
      use m_rms_4_sph_spectr
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_each_scalar_sph_spec(icomp, icou)
!
      integer(kind = kint), intent(in) :: icomp, icou
      integer(kind = kint) :: k, j, idx
!
!
!$omp parallel do private(k,j,idx)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          idx = j + (k-1) * nidx_rj(2)
          rms_sph_dat(icou,idx)                                         &
     &        = d_rj(idx,icomp)*d_rj(idx,icomp)*g_sph_rj(j,11)          &
     &         * radius_1d_rj_r(k) * radius_1d_rj_r(k)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rms_each_scalar_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_each_vector_sph_spec(icomp, icou)
!
      integer(kind = kint), intent(in) :: icomp, icou
      integer(kind = kint) :: k, j, idx
!
!
!$omp parallel do private(k,j,idx)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          idx = j + (k-1) * nidx_rj(2)
          rms_sph_dat(icou,  idx) = g_sph_rj(j,12)                      &
     &                        * ( g_sph_rj(j,3)                         &
     &                          * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)         &
     &                          * d_rj(idx,icomp  )*d_rj(idx,icomp  )   &
     &                         +  d_rj(idx,icomp+1)*d_rj(idx,icomp+1))
          rms_sph_dat(icou+1,idx) = g_sph_rj(j,12)                      &
     &                          * d_rj(idx,icomp+2)*d_rj(idx,icomp+2)
          rms_sph_dat(icou+2,idx) =  rms_sph_dat(icou,  idx)            &
     &                             + rms_sph_dat(icou+1,idx)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rms_each_vector_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_energies_by_rms(icou)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint) :: j, k, idx
!
!
!$omp parallel do private(k,j,idx)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          idx = j + (k-1) * nidx_rj(2)
          rms_sph_dat(icou,  idx) = half * rms_sph_dat(icou,  idx)
          rms_sph_dat(icou+1,idx) = half * rms_sph_dat(icou+1,idx)
          rms_sph_dat(icou+2,idx) = half * rms_sph_dat(icou+2,idx)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sph_energies_by_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_ave_scalar_sph_spectr(icomp, icou)
!
      integer(kind = kint), intent(in) :: icomp, icou
      integer(kind = kint) :: k, kg, inod
!
!
      if(idx_rj_degree_zero .eq. izero) then
        do k = 1, nidx_rj(1)
          kg = idx_gl_1d_rj_r(k)
          ave_sph_lc(icou,kg) = zero
        end do
      else
        do k = 1, nidx_rj(1)
          kg = idx_gl_1d_rj_r(k)
          inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
          ave_sph_lc(icou,kg) = d_rj(inod,icomp)                        &
     &         * radius_1d_rj_r(kg) * radius_1d_rj_r(kg)
        end do
      end if
!
      end subroutine cal_ave_scalar_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine cal_ave_vector_sph_spectr(icomp, icou)
!
      integer(kind = kint), intent(in) :: icomp, icou
      integer(kind = kint) :: k, kg, inod
!
!
      if(idx_rj_degree_zero .eq. izero) then
        do k = 1, nidx_rj(1)
          kg = idx_gl_1d_rj_r(k)
          ave_sph_lc(icou,  kg) = zero
          ave_sph_lc(icou+1,kg) = zero
          ave_sph_lc(icou+2,kg) = zero
        end do
      else
        do k = 1, nidx_rj(1)
          kg = idx_gl_1d_rj_r(k)
          inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
          ave_sph_lc(icou,  kg) = d_rj(inod,icomp)                      &
     &         * radius_1d_rj_r(kg) * radius_1d_rj_r(kg)
          ave_sph_lc(icou+1,kg) = zero
          ave_sph_lc(icou+2,kg) = d_rj(inod,icomp)                      &
     &         * radius_1d_rj_r(kg) * radius_1d_rj_r(kg)
        end do
      end if
!
      end subroutine cal_ave_vector_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module cal_rms_by_sph_spectr
