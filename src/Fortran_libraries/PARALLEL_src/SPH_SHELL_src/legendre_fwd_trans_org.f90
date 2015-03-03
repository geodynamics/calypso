!>@file   legendre_fwd_trans_org.f90
!!@brief  module legendre_fwd_trans_org
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Original version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_org                          &
!!     &         (ncomp, nvector, vr_rtm, sp_rlm)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_org                          &
!!     &         (ncomp, nvector, nscalar, vr_rtm, sp_rlm)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_org
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
      subroutine legendre_f_trans_vector_org                            &
     &         (ncomp, nvector, vr_rtm, sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: i_rlm, k_rtm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, ip, kst, ked
      real(kind = kreal) :: sp1, sp2, sp3, r2_1d_rlm_r
      real(kind = kreal) :: Pvw_l(nidx_rtm(2))
      real(kind = kreal) :: dPvw_l(nidx_rtm(2))
      real(kind = kreal) :: Pgvw_l(nidx_rtm(2))
!
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,l_rtm,j_rlm,k_rtm,nd,              &
!$omp&                    i_rlm,ip_rtm,in_rtm,r2_1d_rlm_r,sp1,sp2,sp3,  &
!$omp&                    Pvw_l,dPvw_l,Pgvw_l)
      do ip = 1, np_smp
        kst = idx_rlm_smp_stack(ip-1,1) + 1
        ked = idx_rlm_smp_stack(ip,  1)
!
        do k_rtm = kst, ked
          r2_1d_rlm_r = radius_1d_rlm_r(k_rtm)*radius_1d_rlm_r(k_rtm)
!
          do j_rlm = 1, nidx_rlm(2)
            do l_rtm = 1, nidx_rtm(2)
              Pvw_l(l_rtm) = P_rtm(l_rtm,j_rlm)                         &
     &                       * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
              dPvw_l(l_rtm) = dPdt_rtm(l_rtm,j_rlm)                     &
     &                       * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
              Pgvw_l(l_rtm) = P_rtm(l_rtm,j_rlm)                        &
     &                       * dble(idx_gl_1d_rlm_j(j_rlm,3))           &
     &                        * asin_theta_1d_rtm(l_rtm)                &
     &                        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
            end do
!
            do nd = 1, nvector
              sp1 = 0.0d0
              sp2 = 0.0d0
              sp3 = 0.0d0
              do l_rtm = 1, nidx_rtm(2)
                ip_rtm = 3*nd  + ncomp*((l_rtm-1) *    istep_rtm(2)     &
     &                    + (k_rtm-1) *                istep_rtm(1)     &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1) * istep_rtm(3))
                in_rtm = 3*nd  + ncomp*((l_rtm-1) *    istep_rtm(2)     &
     &                    + (k_rtm-1) *                istep_rtm(1)     &
     &                    + (mdx_n_rlm_rtm(j_rlm)-1) * istep_rtm(3))
!
                sp1 = sp1 + vr_rtm(ip_rtm-2) * Pvw_l(l_rtm)
                sp2 = sp2 + ( vr_rtm(ip_rtm-1) * dPvw_l(l_rtm)          &
     &                        - vr_rtm(in_rtm  ) * Pgvw_l(l_rtm))
                sp3 = sp3 - ( vr_rtm(in_rtm-1) * Pgvw_l(l_rtm)          &
     &                        + vr_rtm(ip_rtm  ) * dPvw_l(l_rtm))
              end do
!
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rtm-1) * istep_rlm(1))
              sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) + sp1 * r2_1d_rlm_r
              sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                         &
     &                           + sp2 * radius_1d_rlm_r(k_rtm)
              sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                         &
     &                           + sp3 * radius_1d_rlm_r(k_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_org                            &
     &         (ncomp, nvector, nscalar, vr_rtm, sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: l_rtm, ip_rtm, i_rlm, k_rtm, j_rlm
      integer(kind = kint) :: nd, ip, kst, ked
      real(kind = kreal) :: sp1
      real(kind = kreal) :: Pws_l(nidx_rtm(2))
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,j_rlm,k_rtm,nd,                    &
!$omp&                    i_rlm,ip_rtm,l_rtm,sp1,Pws_l)
      do ip = 1, np_smp
        kst = idx_rlm_smp_stack(ip-1,1) + 1
        ked = idx_rlm_smp_stack(ip,  1)
!
        do k_rtm = kst, ked
          do j_rlm = 1, nidx_rlm(2)
            do l_rtm = 1, nidx_rtm(2)
              Pws_l(l_rtm) = P_rtm(l_rtm,j_rlm)                         &
     &                        * g_sph_rlm(j_rlm,6)*weight_rtm(l_rtm)
            end do
!
            do nd = 1, nscalar
              sp1 = 0.0d0
              do l_rtm = 1, nidx_rtm(2)
                ip_rtm =  nd + 3*nvector                                &
     &                + ncomp*((l_rtm-1) *             istep_rtm(2)     &
     &                    + (k_rtm-1) *                istep_rtm(1)     &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1) * istep_rtm(3))
!
                sp1 = sp1 + vr_rtm(ip_rtm) * Pws_l(l_rtm)
              end do
!
              i_rlm = nd + 3*nvector                                    &
     &                   + ncomp * ((j_rlm-1) * istep_rlm(2)            &
     &                            + (k_rtm-1) * istep_rlm(1))
              sp_rlm(i_rlm) = sp_rlm(i_rlm)  + sp1
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_org
