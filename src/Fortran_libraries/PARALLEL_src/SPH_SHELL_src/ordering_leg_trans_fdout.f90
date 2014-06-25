!>@file   ordering_leg_trans_fdout.f90
!!@brief  module ordering_leg_trans_fdout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine order_b_trans_fields_fdout(ncomp,                    &
!!     &          sp_rlm, sp_rlm_fdout)
!!      subroutine order_f_trans_fields_fdout(ncomp,                    &
!!     &          vr_rtm, vr_rtm_fdout)
!!
!!      subroutine back_f_trans_fields_fdout(ncomp, sp_rlm_fdout,sp_rlm)
!!      subroutine back_b_trans_fields_fdout(ncomp, vr_rtm_fdout, vr_rtm)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module ordering_leg_trans_fdout
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
      subroutine order_b_trans_fields_fdout(ncomp,                      &
     &          sp_rlm, sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout) :: sp_rlm_fdout(nnod_rlm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inod
      integer(kind = kint) :: k_rlm, j_rlm, nd, kr_nd
      integer(kind = kint) :: i_rlm_0
!
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(ip,ist,ied,inod,k_rlm,j_rlm,kr_nd,i_rlm_0)
        do ip = 1, np_smp
          ist = inod_rlm_smp_stack(ip-1) + 1
          ied = inod_rlm_smp_stack(ip)
          do inod = ist, ied
            j_rlm = 1 + mod( (inod-1),nidx_rlm(2))
            kr_nd = 1 + (inod - j_rlm) / nidx_rlm(2)
            k_rlm = 1 + mod( (kr_nd-1),nidx_rtm(1))
!
            i_rlm_0 = nd + (j_rlm-1) * ncomp                            &
     &                   + (k_rlm-1) * ncomp * nidx_rlm(2)
!
            sp_rlm_fdout(inod,nd) = sp_rlm(i_rlm_0)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine order_b_trans_fields_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_fdout(ncomp,                      &
     &          vr_rtm, vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout) :: vr_rtm_fdout(nnod_rtm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(ist,ied,i_rtm_0,k_rtm,l_rtm,inod,lnod,m_rtm)
        do ip = 1, np_smp
          ist = inod_rtm_smp_stack(ip-1) + 1
          ied = inod_rtm_smp_stack(ip)
          do inod = ist, ied
            l_rtm = 1 + mod((inod-1),nidx_rtm(2))
            lnod =  1 + (inod - l_rtm) / nidx_rtm(2)
            k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
            m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
            i_rtm_0 = nd + (l_rtm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
            vr_rtm_fdout(inod,nd) = vr_rtm(i_rtm_0)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine order_f_trans_fields_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_fields_fdout(ncomp, sp_rlm_fdout,sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: sp_rlm_fdout(nnod_rlm,ncomp)
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: k_rlm, j_rlm, i_rlm_0
      integer(kind = kint) :: nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,k_rlm,j_rlm,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = ncomp*inod_rlm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rlm_smp_stack(ip)
        do inum = ist, ied
          nd =    1 + mod( (inum-1),ncomp)
          inod =  1 + (inum - nd) / ncomp
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm_0 = nd + (j_rlm-1) * ncomp                              &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          sp_rlm(i_rlm_0  ) = sp_rlm_fdout(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_fields_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_fields_fdout(ncomp, vr_rtm_fdout, vr_rtm)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vr_rtm_fdout(nnod_rtm,ncomp)
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,inod,lnod,  &
!$omp&                    m_rtm,inum)
      do ip = 1, np_smp
        ist = ncomp*inod_rtm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd = 1 + mod(inum-1,ncomp)
          inod = 1 + (inum - nd) / ncomp
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod =  1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = nd + (l_rtm-1) * ncomp                              &
     &                 + (k_rtm-1) * ncomp*nidx_rtm(2)                  &
     &                 + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm(i_rtm_0)  = vr_rtm_fdout(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_fields_fdout
!
! -----------------------------------------------------------------------
!
      end module ordering_leg_trans_fdout
