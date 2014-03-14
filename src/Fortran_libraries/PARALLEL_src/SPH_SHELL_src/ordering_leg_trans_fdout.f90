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
!!      subroutine order_b_trans_vector_fdout(ncomp, nvector,           &
!!     &          sp_rlm_fdout)
!!      subroutine order_b_trans_scalar_fdout(ncomp, nvector, nscalar,  &
!!     &          sp_rlm_fdout)
!!      subroutine order_f_trans_vector_fdout(ncomp, nvector,           &
!!     &          vr_rtm_fdout)
!!      subroutine order_f_trans_scalar_fdout(ncomp, nvector, nscalar,  &
!!     &          vr_rtm_fdout)
!!
!!      subroutine back_f_trans_vector_fdout(ncomp, nvector             &
!!     &          sp_rlm_fdout)
!!      subroutine back_f_trans_scalar_fdout(ncomp, nvector, nscalar,   &
!!     &          sp_rlm_fdout)
!!      subroutine back_b_trans_vector_fdout(ncomp, nvector,            &
!!     &          vr_rtm_fdout)
!!      subroutine back_b_trans_scalar_fdout(ncomp, nvector, nscalar,   &
!!     &          vr_rtm_fdout)
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
      subroutine order_b_trans_vector_fdout(ncomp, nvector,             &
     &          sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_fdout(nnod_rlm,3*nvector)
!
      integer(kind = kint) :: ip, ist, ied, inod
      integer(kind = kint) :: k_rlm, j_rlm, nd, kr_nd
      integer(kind = kint) :: i_rlm_0
!
!
!$omp parallel private(nd)
      do nd = 1, nvector
!$omp do private(ip,ist,ied,inod,k_rlm,j_rlm,kr_nd,i_rlm_0)
        do ip = 1, np_smp
          ist = inod_rlm_smp_stack(ip-1) + 1
          ied = inod_rlm_smp_stack(ip)
          do inod = ist, ied
            j_rlm = 1 + mod( (inod-1),nidx_rlm(2))
            kr_nd = 1 + (inod - j_rlm) / nidx_rlm(2)
            k_rlm = 1 + mod( (kr_nd-1),nidx_rtm(1))
!
            i_rlm_0 = 3*nd + (j_rlm-1) * ncomp                          &
     &                     + (k_rlm-1) * ncomp * nidx_rlm(2)
!
            sp_rlm_fdout(inod,3*nd-2) = sp_rlm(i_rlm_0-2)
            sp_rlm_fdout(inod,3*nd-1) = sp_rlm(i_rlm_0-1)
            sp_rlm_fdout(inod,3*nd  ) = sp_rlm(i_rlm_0  )
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine order_b_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_scalar_fdout(ncomp, nvector, nscalar,    &
     &          sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_fdout(nnod_rlm,nscalar)
!
      integer(kind = kint) :: ip, ist, ied, inod
      integer(kind = kint) :: k_rlm, j_rlm, nd, kr_nd
      integer(kind = kint) :: i_rlm_0
!
!
!$omp parallel private(nd)
      do nd = 1, nscalar
!$omp do private(ip,ist,ied,inod,k_rlm,j_rlm,kr_nd,i_rlm_0)
        do ip = 1, np_smp
          ist = inod_rlm_smp_stack(ip-1) + 1
          ied = inod_rlm_smp_stack(ip)
          do inod = ist, ied
            j_rlm = 1 + mod( (inod-1),nidx_rlm(2))
            kr_nd = 1 + (inod - j_rlm) / nidx_rlm(2)
            k_rlm = 1 + mod( (kr_nd-1),nidx_rtm(1))
!
            i_rlm_0 = nd + 3*nvector + (j_rlm-1) * ncomp                &
     &                   + (k_rlm-1) * ncomp * nidx_rlm(2)
!
            sp_rlm_fdout(inod,nd) = sp_rlm(i_rlm_0)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine order_b_trans_scalar_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_vector_fdout(ncomp, nvector,             &
     &          vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_fdout(nnod_rtm,3*nvector)
!
      integer(kind = kint) :: ip, ist, ied, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel private(nd)
      do nd = 1, nvector
!$omp do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,inod,lnod,m_rtm)
        do ip = 1, np_smp
          ist = inod_rtm_smp_stack(ip-1) + 1
          ied = inod_rtm_smp_stack(ip)
          do inod = ist, ied
            l_rtm = 1 + mod((inod-1),nidx_rtm(2))
            lnod =  1 + (inod - l_rtm) / nidx_rtm(2)
            k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
            m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
            i_rtm_0 = 3*nd + (l_rtm-1) * ncomp                          &
     &                     + (k_rtm-1) * ncomp*nidx_rtm(2)              &
     &                     + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
              vr_rtm_fdout(inod,3*nd-2) = vr_rtm(i_rtm_0-2)             &
     &                 * radius_1d_rlm_r(k_rtm)*radius_1d_rlm_r(k_rtm)
              vr_rtm_fdout(inod,3*nd-1) = vr_rtm(i_rtm_0-1)             &
     &                 * radius_1d_rlm_r(k_rtm)
              vr_rtm_fdout(inod,3*nd  ) = vr_rtm(i_rtm_0  )             &
     &                 * radius_1d_rlm_r(k_rtm)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine order_f_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_scalar_fdout(ncomp, nvector, nscalar,    &
     &          vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_fdout(nnod_rtm,nscalar)
!
      integer(kind = kint) :: ip, ist, ied, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel private(nd)
      do nd = 1, nscalar
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
            i_rtm_0 = nd + 3*nvector + (l_rtm-1) * ncomp                &
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
      end subroutine order_f_trans_scalar_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_fdout(ncomp, nvector,             &
     &          sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_fdout(nnod_rlm,3*nvector)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: k_rlm, j_rlm, i_rlm_0
      integer(kind = kint) :: nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,k_rlm,j_rlm,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
        do inum = ist, ied
          nd =    1 + mod( (inum-1),nvector)
          inod =  1 + (inum - nd) / nvector
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm_0 = 3*nd + (j_rlm-1) * ncomp                            &
     &                   + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          sp_rlm(i_rlm_0-2) = sp_rlm_fdout(inod,3*nd-2)
          sp_rlm(i_rlm_0-1) = sp_rlm_fdout(inod,3*nd-1)
          sp_rlm(i_rlm_0  ) = sp_rlm_fdout(inod,3*nd  )
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_fdout(ncomp, nvector, nscalar,     &
     &          sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: sp_rlm_fdout(nnod_rlm,nscalar)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: k_rlm, j_rlm, i_rlm_0
      integer(kind = kint) :: nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,k_rlm,j_rlm,nd,i_rlm_0)
      do ip = 1, np_smp
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
        do inum = ist, ied
          nd =    1 + mod( (inum-1),nscalar)
          inod =  1 + (inum - nd) / nscalar
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm_0 = nd + 3*nvector + (j_rlm-1) * ncomp                  &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          sp_rlm(i_rlm_0  ) = sp_rlm_fdout(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_fdout(ncomp, nvector,              &
     &          vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_fdout(nnod_rtm,3*nvector)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,inod,lnod,  &
!$omp&                    m_rtm,inum)
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
          vr_rtm(i_rtm_0-2)  = vr_rtm_fdout(inod,3*nd-2)                &
     &                       * a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)
          vr_rtm(i_rtm_0-1)  = vr_rtm_fdout(inod,3*nd-1)                &
     &                       * a_r_1d_rlm_r(k_rtm)
          vr_rtm(i_rtm_0  )  = vr_rtm_fdout(inod,3*nd  )                &
     &                       * a_r_1d_rlm_r(k_rtm)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_fdout(ncomp, nvector, nscalar,     &
     &          vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm_fdout(nnod_rtm,nscalar)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, lnod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,inod,lnod,  &
!$omp&                    m_rtm,inum)
      do ip = 1, np_smp
        ist = nscalar*inod_rtm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd = 1 + mod(inum-1,nscalar)
          inod = 1 + (inum - nd) / nscalar
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod =  1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = nd + 3*nvector + (l_rtm-1) * ncomp                  &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm(i_rtm_0)  = vr_rtm_fdout(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module ordering_leg_trans_fdout
