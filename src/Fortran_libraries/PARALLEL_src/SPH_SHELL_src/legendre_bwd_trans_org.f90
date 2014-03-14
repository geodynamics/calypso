!>@file   legendre_bwd_trans_org.f90
!!@brief  module legendre_bwd_trans_org
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Original version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_org(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_org(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_org
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
      subroutine legendre_b_trans_vector_org(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
!$omp parallel do private(j_rlm,l_rtm,nd,ip_rtm,in_rtm,i_rlm,           &
!$omp&               pg_tmp,dp_tmp)
      do k_rtm = 1,  nidx_rtm(1)
        do j_rlm = 1, nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
            pg_tmp = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
            dp_tmp = dPdt_rtm(l_rtm,j_rlm)
!cdir nodep
            do nd = 1, nvector
              ip_rtm = 3*nd + (l_rtm-1) * ncomp                         &
     &                      + (k_rtm-1) * ncomp*nidx_rtm(2)             &
     &                      + (mdx_p_rlm_rtm(j_rlm)-1) * ncomp          &
     &                       * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = 3*nd + (j_rlm-1) * ncomp                          &
     &                     + (k_rtm-1) * ncomp * nidx_rlm(2)
!
              vr_rtm(ip_rtm-2) = vr_rtm(ip_rtm-2)                       &
     &                     + sp_rlm(i_rlm-2) * pg_tmp
!
              vr_rtm(ip_rtm-1) = vr_rtm(ip_rtm-1)                       &
     &                     + sp_rlm(i_rlm-1) * dp_tmp
!
              vr_rtm(ip_rtm  ) = vr_rtm(ip_rtm  )                       &
     &                     - sp_rlm(i_rlm  ) * dp_tmp
            end do
          end do
!
          do l_rtm = 1, nidx_rtm(2)
            pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)      &
     &              * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
!cdir nodep
            do nd = 1, nvector
              in_rtm = 3*nd + (l_rtm-1) * ncomp                         &
     &                      + (k_rtm-1) * ncomp*nidx_rtm(2)             &
     &                      + (mdx_n_rlm_rtm(j_rlm)-1) * ncomp          &
     &                       * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = 3*nd + (j_rlm-1) * ncomp                          &
     &                     + (k_rtm-1) * ncomp * nidx_rlm(2)
!
              vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1)                       &
     &                       + sp_rlm(i_rlm  ) * pg_tmp
!
              vr_rtm(in_rtm  ) = vr_rtm(in_rtm  )                       &
     &                       + sp_rlm(i_rlm-1) * pg_tmp
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_org(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,l_rtm,nd,ip_rtm,in_rtm,i_rlm)
      do k_rtm = 1,  nidx_rtm(1)
        do j_rlm = 1, nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
!cdir nodep
            do nd = 1, nscalar
              ip_rtm = nd + 3*nvector + (l_rtm-1) * ncomp               &
     &                    + (k_rtm-1) * ncomp*nidx_rtm(2)               &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1) * ncomp            &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = nd + 3*nvector + (j_rlm-1) * ncomp                &
     &                               + (k_rtm-1) * ncomp * nidx_rlm(2)
!
!              vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                          &
              vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                           &
     &                        + sp_rlm(i_rlm) * P_rtm(l_rtm,j_rlm)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_org
