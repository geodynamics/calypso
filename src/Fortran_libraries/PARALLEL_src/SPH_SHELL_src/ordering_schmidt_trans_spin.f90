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
!!      subroutine order_b_trans_fields_spin(ncomp, nvector, nscalar,   &
!!     &          sp_rlm, sp_rlm_spin)
!!      subroutine order_f_trans_fields_spin(ncomp, nvector, nscalar,   &
!!     &          vr_rtm, vr_rtm_spin)
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
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_fields_spin(ncomp, nvector, nscalar,     &
     &          sp_rlm, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, nd, i_rlm_0
      integer(kind = kint) :: j_rlm, k_rlm
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,nd,i_rlm_0,j_rlm,k_rlm)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = 3*nd + (inod-1) * ncomp
!
          sp_rlm_spin(j_rlm,k_rlm,nd          ) = sp_rlm(i_rlm_0-2)
          sp_rlm_spin(j_rlm,k_rlm,nd+nvector  ) = sp_rlm(i_rlm_0-1)
          sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector) = sp_rlm(i_rlm_0  )
        end do
!
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = nd + 3*nvector + (inod-1) * ncomp
!
          sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector) = sp_rlm(i_rlm_0)
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_spin(ncomp, nvector, nscalar,     &
     &          vr_rtm, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1),ncomp,nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,i_rtm_0,nd,kr_nd,       &
!$omp&                     k_rtm,l_rtm,m_rtm)
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
!
          vr_rtm_spin(l_rtm,k_rtm,nd,m_rtm)                             &
     &            = vr_rtm(i_rtm_0-2)
          vr_rtm_spin(l_rtm,k_rtm,nd+nvector,m_rtm)                     &
     &            = vr_rtm(i_rtm_0-1)
          vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)                   &
     &            = vr_rtm(i_rtm_0  )
        end do
!
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
!
          vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)                   &
     &            = vr_rtm(i_rtm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_spin
