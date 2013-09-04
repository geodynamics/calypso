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
!!      subroutine legendre_b_trans_vector_org(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_org(nb)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
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
      subroutine legendre_b_trans_vector_org(nb)
!
      integer(kind = kint), intent(in) :: nb
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
            do nd = 1, nb
              ip_rtm = nd + (l_rtm-1) * nb                              &
     &                    + (k_rtm-1) * nb*nidx_rtm(2)                  &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1) * nb               &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
!
!              vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                          &
              vr_rtm(3*ip_rtm-2) = vr_rtm(3*ip_rtm-2)                   &
     &                     + sp_rlm(3*i_rlm-2) * pg_tmp
!
!              vt_rtm(ip_rtm) = vt_rtm(ip_rtm)                          &
              vr_rtm(3*ip_rtm-1) = vr_rtm(3*ip_rtm-1)                   &
     &                     + sp_rlm(3*i_rlm-1) * dp_tmp
!
!              vp_rtm(ip_rtm) = vp_rtm(ip_rtm)                          &
              vr_rtm(3*ip_rtm  ) = vr_rtm(3*ip_rtm  )                   &
     &                     - sp_rlm(3*i_rlm  ) * dp_tmp
!
            end do
          end do
!
          do l_rtm = 1, nidx_rtm(2)
            pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)      &
     &              * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
!cdir nodep
            do nd = 1, nb
              in_rtm = nd + (l_rtm-1) * nb                              &
     &                    + (k_rtm-1) * nb*nidx_rtm(2)                  &
     &                    + (mdx_n_rlm_rtm(j_rlm)-1) * nb               &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
!
!              vt_rtm(in_rtm) = vt_rtm(in_rtm)                          &
              vr_rtm(3*in_rtm-1) = vr_rtm(3*in_rtm-1)                   &
     &                       + sp_rlm(3*i_rlm  ) * pg_tmp
!
!              vp_rtm(in_rtm) = vp_rtm(in_rtm)                          &
              vr_rtm(3*in_rtm  ) = vr_rtm(3*in_rtm  )                   &
     &                       + sp_rlm(3*i_rlm-1) * pg_tmp
!
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
      subroutine legendre_b_trans_scalar_org(nb)
!
      integer(kind = kint), intent(in) :: nb
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
            do nd = 1, nb
              ip_rtm = nd + (l_rtm-1) * nb                              &
     &                    + (k_rtm-1) * nb*nidx_rtm(2)                  &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1) * nb               &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
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
