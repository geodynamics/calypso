!>@file   legendre_bwd_trans_fdout.f90
!!@brief  module legendre_bwd_trans_fdout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Original version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_fdout(nvector,               &
!!     &          sp_rlm_fdout, vr_rtm_fdout)
!!      subroutine legendre_b_trans_scalar_fdout(nscalar,               &
!!     &          sp_rlm_fdout, vr_rtm_fdout)
!!        Input:  vr_rtm_fdout
!!        Output: sp_rlm_fdout
!!@endverbatim
!!
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_fdout
!
      use m_precision
!
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
      subroutine legendre_b_trans_vector_fdout(nvector,                 &
     &          sp_rlm_fdout, vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_fdout(nnod_rlm,3*nvector)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_fdout(nnod_rtm,3*nvector)
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: inum, nd
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
!$omp parallel
      do nd = 1, nvector
!$omp do private(inum,j_rlm,k_rlm,l_rtm,ip_rtm,in_rtm,i_rlm,            &
!$omp&           pg_tmp,dp_tmp)
        do l_rtm = 1, nidx_rtm(2)
          do inum = 1, nnod_rlm
            k_rlm = 1 + mod( (inum-1),nidx_rlm(1))
            j_rlm = 1 + (inum - k_rlm) / nidx_rlm(1)
!
            i_rlm = j_rlm + (k_rlm-1) * nidx_rlm(2)
!
            pg_tmp = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
            dp_tmp = dPdt_rtm(l_rtm,j_rlm)
            ip_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                    &
     &                       + (mdx_p_rlm_rtm(j_rlm)-1)                 &
     &                       * nidx_rtm(1)*nidx_rtm(2)
!
!
            vr_rtm_fdout(ip_rtm,3*nd-2) = vr_rtm_fdout(ip_rtm,3*nd-2)   &
     &                     + sp_rlm_fdout(i_rlm,3*nd-2) * pg_tmp
            vr_rtm_fdout(ip_rtm,3*nd-1) = vr_rtm_fdout(ip_rtm,3*nd-1)   &
     &                     + sp_rlm_fdout(i_rlm,3*nd-1) * dp_tmp
            vr_rtm_fdout(ip_rtm,3*nd  ) = vr_rtm_fdout(ip_rtm,3*nd  )   &
     &                     - sp_rlm_fdout(i_rlm,3*nd  ) * dp_tmp
!
!
            pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)      &
     &              * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
            in_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                    &
     &                       + (mdx_n_rlm_rtm(j_rlm)-1)                 &
     &                       * nidx_rtm(1)*nidx_rtm(2)
!
            vr_rtm_fdout(in_rtm,3*nd-1) = vr_rtm_fdout(in_rtm,3*nd-1)   &
     &                       + sp_rlm_fdout(i_rlm,3*nd  ) * pg_tmp
            vr_rtm_fdout(in_rtm,3*nd  ) = vr_rtm_fdout(in_rtm,3*nd  )   &
     &                       + sp_rlm_fdout(i_rlm,3*nd-1) * pg_tmp
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine legendre_b_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_fdout(nscalar,                 &
     &          sp_rlm_fdout, vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_fdout(nnod_rlm,nscalar)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_fdout(nnod_rtm,nscalar)
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, inum
!
!
!$omp parallel
     do nd = 1, nscalar
!$omp do private(inum,j_rlm,l_rtm,k_rlm,ip_rtm,in_rtm,i_rlm)
        do l_rtm = 1, nidx_rtm(2)
          do inum = 1, nnod_rlm
            k_rlm = 1 + mod( (inum-1),nidx_rlm(1))
            j_rlm = 1 + (inum - k_rlm) / nidx_rlm(1)
!
            ip_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                    &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1)                    &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
            i_rlm = j_rlm + (k_rlm-1) * nidx_rlm(2)
!
            vr_rtm_fdout(ip_rtm,nd) = vr_rtm_fdout(ip_rtm,nd)           &
     &                  + sp_rlm_fdout(i_rlm,nd) * P_rtm(l_rtm,j_rlm)
!
          end do
!
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine legendre_b_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_fdout
