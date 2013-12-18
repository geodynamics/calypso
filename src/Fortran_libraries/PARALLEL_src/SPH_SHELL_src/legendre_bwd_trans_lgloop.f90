!>@file   legendre_bwd_trans_lgloop.f90
!!@brief  module legendre_bwd_trans_lgloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Original version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_long(nfld)
!!        Output:  vr_rtm   (Order: radius,theta,phi)
!!        Input:   sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_long(nfld)
!!        Output:  vr_rtm
!!        Input:   sp_rlm
!!@endverbatim
!!
!!@n @param  nfld  number of fields to be transformed
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
      use m_work_4_sph_trans_lgloop
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_vector_long(nfld)
!
      integer(kind = kint), intent(in) :: nfld
!
      integer(kind = kint) :: ip, ist, ied, irt, irt_b
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: m, nd, mst_rtm
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,j_rlm,ip_rtm,in_rtm,         &
!$omp&               k_rtm,l_rtm,m,nd,irt,irt_b,mst_rtm)
      do ip = 1, np_smp
        ist = nfld*irt_rtm_smp_stack(ip-1) + 1
        ied = nfld*irt_rtm_smp_stack(ip)
        do j_rlm = 1, nidx_rlm(2)
          m = idx_gl_1d_rlm_j(j_rlm,3)
          mst_rtm = (mdx_p_rlm_rtm(j_rlm)-1) * nfld                     &
     &             * nidx_rtm(1)*nidx_rtm(2)
!
!cdir nodep
          do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nfld)
              irt = 1 + (irt_b - nd) / nfld
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              ip_rtm = irt_b + mst_rtm
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nfld                                 &
     &               + (k_rtm-1) * nfld * nidx_rlm(2)
!
!              vr_rtm_long(ip_rtm) = vr_rtm_long(ip_rtm)                &
              vr_rtm_long(3*ip_rtm-2) = vr_rtm_long(3*ip_rtm-2)         &
     &                + sp_rlm_long(3*i_rlm-2) * P_rtm(l_rtm,j_rlm)     &
     &                      * g_sph_rlm(j_rlm,3)
!
!              vt_rtm(ip_rtm) = vt_rtm(ip_rtm)                          &
              vr_rtm_long(3*ip_rtm-1) = vr_rtm_long(3*ip_rtm-1)         &
     &                + sp_rlm_long(3*i_rlm-1) * dPdt_rtm(l_rtm,j_rlm)
!
!              vp_rtm(ip_rtm) = vp_rtm(ip_rtm)                          &
              vr_rtm_long(3*ip_rtm  ) = vr_rtm_long(3*ip_rtm  )         &
     &                - sp_rlm_long(3*i_rlm  ) * dPdt_rtm(l_rtm,j_rlm)
!
          end do
!
!
          mst_rtm = (mdx_n_rlm_rtm(j_rlm)-1)                            &
     &             * nfld * nidx_rtm(1)*nidx_rtm(2)
!cdir nodep
          do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nfld)
              irt = 1 + (irt_b - nd) / nfld
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              in_rtm = irt_b + mst_rtm
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nfld                                 &
     &               + (k_rtm-1) * nfld * nidx_rlm(2)
!
!              vt_rtm(in_rtm) = vt_rtm(in_rtm)                          &
              vr_rtm_long(3*in_rtm-1) = vr_rtm_long(3*in_rtm-1)         &
     &                   + sp_rlm_long(3*i_rlm  ) * P_rtm(l_rtm,j_rlm)  &
     &                    * dble(-m) * asin_theta_1d_rtm(l_rtm)
!
!              vp_rtm(in_rtm) = vp_rtm(in_rtm)                          &
              vr_rtm_long(3*in_rtm  ) = vr_rtm_long(3*in_rtm  )         &
     &                   + sp_rlm_long(3*i_rlm-1) * P_rtm(l_rtm,j_rlm)  &
     &                    * dble(-m) * asin_theta_1d_rtm(l_rtm)
!
           end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_long(nfld)
!
      integer(kind = kint), intent(in) :: nfld
!
      integer(kind = kint) :: ip, ist, ied, irt, irt_b
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,j_rlm,i_rtm,k_rtm,l_rtm,     &
!$omp&                    nd,irt,irt_b)
      do ip = 1, np_smp
        ist = nfld*irt_rtm_smp_stack(ip-1) + 1
        ied = nfld*irt_rtm_smp_stack(ip)
        do j_rlm = 1, nidx_rlm(2)
!cdir nodep
            do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nfld)
              irt = 1 + (irt_b - nd) / nfld
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              i_rtm = irt_b                                             &
     &               + (mdx_p_rlm_rtm(j_rlm)-1)                         &
     &                * nfld * nidx_rtm(1) * nidx_rtm(2)
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nfld                                 &
     &               + (k_rtm-1) * nfld * nidx_rlm(2)
              vr_rtm_long(i_rtm) = vr_rtm_long(i_rtm)                   &
     &                      + sp_rlm_long(i_rlm) * P_rtm(l_rtm,j_rlm)
            end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_lgloop
