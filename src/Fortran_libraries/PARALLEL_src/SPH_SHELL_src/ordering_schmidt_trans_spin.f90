!>@file   ordering_schmidt_trans_spin.f90
!!@brief  module ordering_schmidt_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine order_b_trans_vector_spin(ncomp, nvector, sp_rlm_spin)
!!      subroutine order_b_trans_scalar_spin(ncomp, nvector, nscalar,   &
!!     &          sp_rlm_spin)
!!      subroutine order_f_trans_vector_spin(ncomp, nvector, vr_rtm_spin)
!!      subroutine order_f_trans_scalar_spin(ncomp, nvector, nscalar,   &
!!     &          vr_rtm_spin)
!!
!!      subroutine back_f_trans_vector_spin(ncomp, nvector, sp_rlm_spin)
!!      subroutine back_f_trans_scalar_spin(ncomp, nvector, nscalar,    &
!!     &          sp_rlm_spin)
!!      subroutine back_b_trans_vector_spin(ncomp, nvector, vr_rtm_spin)
!!      subroutine back_b_trans_scalar_spin(ncomp, nvector, nscalar,    &
!!     &          vr_rtm_spin)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module ordering_schmidt_trans_spin
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
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
      subroutine order_b_trans_vector_spin(ncomp, nvector, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &                   :: sp_rlm_spin(nnod_rlm,nvector,3)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, nd, i_rlm_0
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = 3*nd + (inod-1) * ncomp
!
          sp_rlm_spin(inod,nd,1) = sp_rlm(i_rlm_0-2)
          sp_rlm_spin(inod,nd,2) = sp_rlm(i_rlm_0-1)
          sp_rlm_spin(inod,nd,3) = sp_rlm(i_rlm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_scalar_spin(ncomp, nvector, nscalar,     &
     &          sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout) :: sp_rlm_spin(nnod_rlm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, nd, i_rlm_0
!
!
!$omp  parallel do  private(ip,ist,ied,inum,inod,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = nd + 3*nvector + (inod-1) * ncomp
!
          sp_rlm_spin(inod,nd+3*nvector) = sp_rlm(i_rlm_0)
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_vector_spin(ncomp, nvector, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &                   :: vr_rtm_spin(nnod_rtm,nvector,3)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd, kr_nd, i_rtm_spin
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,i_rtm_0,nd,kr_nd,       &
!$omp&                     k_rtm,l_rtm,m_rtm,i_rtm_spin)
      do ip = 1, np_smp
        ist = nvector*inod_rtm_smp_stack(ip-1) + 1
        ied = nvector*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          l_rtm = 1 + mod((inum-1),nidx_rtm(2))
          inod =  1 + (inum - l_rtm) / nidx_rtm(2)
          m_rtm = 1 + mod( (inod-1),nidx_rtm(3))
          kr_nd = 1 + (inod - m_rtm) / nidx_rtm(3)
          k_rtm = 1 + mod( (kr_nd-1),nidx_rtm(1))
          nd =    1 + (kr_nd-k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = 3*nd + (l_rtm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
          i_rtm_spin = 1 + mod((inum-1),nnod_rtm)
!
          vr_rtm_spin(i_rtm_spin,nd,1  ) = vr_rtm(i_rtm_0-2)            &
     &                 * radius_1d_rlm_r(k_rtm)*radius_1d_rlm_r(k_rtm)
          vr_rtm_spin(i_rtm_spin,nd,2  ) = vr_rtm(i_rtm_0-1)            &
     &                 * radius_1d_rlm_r(k_rtm)
          vr_rtm_spin(i_rtm_spin,nd,3) = vr_rtm(i_rtm_0  )              &
     &                 * radius_1d_rlm_r(k_rtm)
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_scalar_spin(ncomp, nvector, nscalar,     &
     &          vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout) :: vr_rtm_spin(nnod_rtm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd, kr_nd, i_rtm_spin
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,i_rtm_0,nd,kr_nd,       &
!$omp&                     k_rtm,l_rtm,m_rtm,i_rtm_spin)
      do ip = 1, np_smp
        ist = nscalar*inod_rtm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          l_rtm = 1 + mod((inum-1),nidx_rtm(2))
          inod =  1 + (inum - l_rtm) / nidx_rtm(2)
          m_rtm = 1 + mod( (inod-1),nidx_rtm(3))
          kr_nd = 1 + (inod - m_rtm) / nidx_rtm(3)
          k_rtm = 1 + mod( (kr_nd-1),nidx_rtm(1))
          nd =    1 + (kr_nd-k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = nd + 3*nvector + (l_rtm-1) * ncomp                  &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
          i_rtm_spin = 1 + mod((inum-1),nnod_rtm)
!
          vr_rtm_spin(i_rtm_spin,nd+3*nvector) = vr_rtm(i_rtm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_spin(ncomp, nvector, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in) :: sp_rlm_spin(nnod_rlm,nvector,3)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rlm_0, nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
        do inum = ist, ied
          nd =    1 + mod( (inum-1),nvector)
          inod =  1 + (inum - nd) / nvector
!          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
!          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm_0 = 3*nd + (inod-1) * ncomp
!
          sp_rlm(i_rlm_0-2) = sp_rlm_spin(inod,nd,1)
          sp_rlm(i_rlm_0-1) = sp_rlm_spin(inod,nd,2)
          sp_rlm(i_rlm_0  ) = sp_rlm_spin(inod,nd,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_spin(ncomp, nvector, nscalar,      &
     &          sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: sp_rlm_spin(nnod_rlm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rlm_0, nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
        do inum = ist, ied
          nd =    1 + mod( (inum-1),nscalar)
          inod =  1 + (inum - nd) / nscalar
!
          i_rlm_0 = nd + 3*nvector + (inod-1) * ncomp
!
          sp_rlm(i_rlm_0  ) = sp_rlm_spin(inod,nd+3*nvector)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_spin(ncomp, nvector, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in) :: vr_rtm_spin(nnod_rtm,nvector,3)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm, i_rtm_spin
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,inod,lnod,  &
!$omp&                    m_rtm,inum,i_rtm_spin)
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
          i_rtm_0 = 3*nd + (inod-1) * ncomp
          i_rtm_spin = l_rtm + (m_rtm-1) * nidx_rtm(2)                  &
     &                       + (k_rtm-1) * nidx_rtm(2)*nidx_rtm(3)
!
          vr_rtm(i_rtm_0-2) = vr_rtm_spin(i_rtm_spin,nd,1)              &
     &                       * a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)
          vr_rtm(i_rtm_0-1) = vr_rtm_spin(i_rtm_spin,nd,2)              &
     &                       * a_r_1d_rlm_r(k_rtm)
          vr_rtm(i_rtm_0  ) = vr_rtm_spin(i_rtm_spin,nd,3)              &
     &                       * a_r_1d_rlm_r(k_rtm)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_spin(ncomp, nvector, nscalar,      &
     &          vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm_spin(nnod_rtm, ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, lnod, nd
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm, i_rtm_spin
!
!
!$omp parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,inod,lnod,  &
!$omp&                    m_rtm,inum,i_rtm_spin)
      do ip = 1, np_smp
        ist = nscalar*inod_rtm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd = 1 + mod(inum-1,nscalar)
          inod = 1 + (inum - nd) / nscalar
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = nd + 3*nvector + (inod-1) * ncomp
          i_rtm_spin = l_rtm + (m_rtm-1) * nidx_rtm(2)                  &
     &                       + (k_rtm-1) * nidx_rtm(2)*nidx_rtm(3)
!
          vr_rtm(i_rtm_0  ) = vr_rtm_spin(i_rtm_spin,nd+3*nvector)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_spin

