!>@file   merge_polidal_toroidal_v.f90
!!@brief  module merge_polidal_toroidal_v
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Take products with radius for spherical transform
!!
!!@verbatim
!!      subroutine clear_bwd_legendre_trans(ncomp)
!!      subroutine clear_fwd_legendre_trans(ncomp)
!!
!!      subroutine prod_r_vect_sph_f_trans(ncomp, nvector)
!!      subroutine const_vect_sph_b_trans(ncomp, nvector)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!
       module merge_polidal_toroidal_v
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine clear_bwd_legendre_trans(ncomp)
!
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint) :: ip, ist, ied, i_rtm
!
!
!$omp parallel do private(ip,i_rtm,ist,ied)
      do ip = 1, np_smp
        ist = ncomp*inod_rtm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rtm_smp_stack(ip)
        do i_rtm = ist, ied
          vr_rtm(i_rtm) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_bwd_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine clear_fwd_legendre_trans(ncomp)
!
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint) :: ip, ist, ied, i_rlm
!
!
!$omp parallel do private(ip,i_rlm,ist,ied)
      do ip = 1, np_smp
        ist = ncomp*inod_rlm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rlm_smp_stack(ip)
        do i_rlm = ist, ied
            sp_rlm(i_rlm) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_fwd_legendre_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine prod_r_vect_sph_f_trans(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd, inod, lnod, i_rtm_0
!
!$omp  parallel do                                                      &
!$omp& private(ip,ist,ied,inum,k_rtm,l_rtm,nd,inod,lnod,m_rtm,i_rtm_0)
      do ip = 1, np_smp
        ist = nvector*inod_rtm_smp_stack(ip-1) + 1
        ied = nvector*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd = 1 + mod(inum-1,nvector)
          inod = 1 + (inum - nd) / nvector
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = 3*nd + (l_rtm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm(i_rtm_0-2)                                             &
     &              = radius_1d_rlm_r(k_rtm)*radius_1d_rlm_r(k_rtm)     &
     &               * vr_rtm(i_rtm_0-2) 
          vr_rtm(i_rtm_0-1) = radius_1d_rlm_r(k_rtm)*vr_rtm(i_rtm_0-1)
          vr_rtm(i_rtm_0  ) = radius_1d_rlm_r(k_rtm)*vr_rtm(i_rtm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine prod_r_vect_sph_f_trans
!
! -----------------------------------------------------------------------
!
      subroutine const_vect_sph_b_trans(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd, inod, lnod, i_rtm_0
!
!$omp  parallel do                                                      &
!$omp& private(ip,ist,ied,inum,k_rtm,l_rtm,nd,inod,lnod,m_rtm,i_rtm_0)
      do ip = 1, np_smp
        ist = nvector*inod_rtm_smp_stack(ip-1) + 1
        ied = nvector*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd = 1 + mod(inum-1,nvector)
          inod = 1 + (inum - nd) / nvector
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = 3*nd + (l_rtm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm(i_rtm_0-2) = a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)   &
     &                       * vr_rtm(i_rtm_0-2) 
          vr_rtm(i_rtm_0-1) = a_r_1d_rlm_r(k_rtm)*vr_rtm(i_rtm_0-1)
          vr_rtm(i_rtm_0  ) = a_r_1d_rlm_r(k_rtm)*vr_rtm(i_rtm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine const_vect_sph_b_trans
!
! -----------------------------------------------------------------------
!
      end module merge_polidal_toroidal_v
