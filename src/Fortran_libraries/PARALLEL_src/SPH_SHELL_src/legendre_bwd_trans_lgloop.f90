!>@file   legendre_bwd_trans_lgloop.f90
!!@brief  module legendre_bwd_trans_lgloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (longest loop version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_long(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_long(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_lgloop
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
      subroutine legendre_b_trans_vector_long(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, kr_nd, inum
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
!$omp  parallel do private(inum,k_rlm,j_rlm,nd,kr_nd,                   &
!$omp&                     i_rlm,l_rtm,ip_rtm,in_rtm,pg_tmp,dp_tmp)
      do l_rtm = 1, nidx_rtm(2)
        do inum = 1, nvector*nnod_rlm
          nd =    1 + mod( (inum-1),nvector)
          kr_nd = 1 + mod( (inum-1),(nvector*nidx_rlm(1)))
          k_rlm = 1 + (kr_nd - nd) / nvector
          j_rlm = 1 + (inum - kr_nd) / (nvector*nidx_rlm(1))
!
          pg_tmp = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
          dp_tmp = dPdt_rtm(l_rtm,j_rlm)
!
          ip_rtm = 3*nd + (l_rtm-1) * ncomp                             &
     &                  + (k_rlm-1) * ncomp*nidx_rtm(2)                 &
     &                  + (mdx_p_rlm_rtm(j_rlm)-1) * ncomp              &
     &                   * nidx_rtm(1)*nidx_rtm(2)
!
          i_rlm = 3*nd + (j_rlm-1) * ncomp                              &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          vr_rtm(ip_rtm-2) = vr_rtm(ip_rtm-2)                           &
     &                     + sp_rlm(i_rlm-2) * pg_tmp
!
          vr_rtm(ip_rtm-1) = vr_rtm(ip_rtm-1)                           &
     &                     + sp_rlm(i_rlm-1) * dp_tmp
!
          vr_rtm(ip_rtm  ) = vr_rtm(ip_rtm  )                           &
     &                     - sp_rlm(i_rlm  ) * dp_tmp
!
        end do
!
        do inum = 1, nvector*nnod_rlm
          nd =    1 + mod( (inum-1),nvector)
          kr_nd = 1 + mod( (inum-1),(nvector*nidx_rlm(1)))
          k_rlm = 1 + (kr_nd - nd) / nvector
          j_rlm = 1 + (inum - kr_nd) / (nvector*nidx_rlm(1))
!
          pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)        &
     &            * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
!
          in_rtm = 3*nd + (l_rtm-1) * ncomp                             &
     &                  + (k_rlm-1) * ncomp*nidx_rtm(2)                 &
     &                  + (mdx_n_rlm_rtm(j_rlm)-1) * ncomp              &
     &                   * nidx_rtm(1)*nidx_rtm(2)
!
          i_rlm = 3*nd + (j_rlm-1) * ncomp                              &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1)                           &
     &                       + sp_rlm(i_rlm  ) * pg_tmp
!
          vr_rtm(in_rtm  ) = vr_rtm(in_rtm  )                           &
     &                       + sp_rlm(i_rlm-1) * pg_tmp
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_long(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, l_rtm
      integer(kind = kint) :: ip_rtm
      integer(kind = kint) :: inum, nd, kr_nd
!
!
!$omp  parallel do private(inum,k_rlm,j_rlm,nd,kr_nd,                   &
!$omp&                     i_rlm,l_rtm,ip_rtm)
      do l_rtm = 1, nidx_rtm(2)
        do inum = 1, nscalar*nnod_rlm
          nd =    1 + mod( (inum-1),nscalar)
          kr_nd = 1 + mod( (inum-1),(nscalar*nidx_rlm(1)))
          k_rlm = 1 + (kr_nd - nd) / nscalar
          j_rlm = 1 + (inum - kr_nd) / (nscalar*nidx_rlm(1))
!
          ip_rtm = nd + 3*nvector + (l_rtm-1) * ncomp                   &
     &                + (k_rlm-1) * ncomp*nidx_rtm(2)                   &
     &                + (mdx_p_rlm_rtm(j_rlm)-1) * ncomp                &
     &                 * nidx_rtm(1)*nidx_rtm(2)
!
          i_rlm = nd + 3*nvector + (j_rlm-1) * ncomp                    &
     &                           + (k_rlm-1) * ncomp*nidx_rlm(2)
!
          vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                               &
     &                    + sp_rlm(i_rlm) * P_rtm(l_rtm,j_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_lgloop
