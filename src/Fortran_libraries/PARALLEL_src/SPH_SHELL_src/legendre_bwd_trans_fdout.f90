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
!!      subroutine legendre_b_trans_vector_fdout(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm_fdout   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_fdout(nb)
!!        Input:  vr_rtm
!!        Output: sp_rlm_fdout
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
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
      use m_work_4_sph_trans_fldout
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_vector_fdout(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, nd_kr
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
!$omp parallel do private(nd_kr,k_rtm,j_rlm,l_rtm,nd,                   &
!$omp&                    ip_rtm,in_rtm,i_rlm,pg_tmp,dp_tmp)
      do nd_kr = 1, nb*nidx_rtm(1)
        k_rtm = mod(nd_kr-1,nidx_rtm(1)) + 1
        nd = (nd_kr - k_rtm) / nidx_rtm(1) + 1
        do j_rlm = 1, nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
            pg_tmp = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
            dp_tmp = dPdt_rtm(l_rtm,j_rlm)
!
            ip_rtm = l_rtm + (k_rtm-1) * nidx_rtm(2)                    &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1)                    &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
            i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            vr_rtm_fdout(ip_rtm,3*nd-2) = vr_rtm_fdout(ip_rtm,3*nd-2)   &
     &                     + sp_rlm_fdout(i_rlm,3*nd-2) * pg_tmp
!
!              vt_rtm(ip_rtm) = vt_rtm(ip_rtm)                          &
            vr_rtm_fdout(ip_rtm,3*nd-1) = vr_rtm_fdout(ip_rtm,3*nd-1)   &
     &                     + sp_rlm_fdout(i_rlm,3*nd-1) * dp_tmp
!
!              vp_rtm(ip_rtm) = vp_rtm(ip_rtm)                          &
            vr_rtm_fdout(ip_rtm,3*nd  ) = vr_rtm_fdout(ip_rtm,3*nd  )   &
     &                     - sp_rlm_fdout(i_rlm,3*nd  ) * dp_tmp
!
          end do
!
          do l_rtm = 1, nidx_rtm(2)
            pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)      &
     &              * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
            in_rtm = l_rtm-1 + (k_rtm-1) * nidx_rtm(2)                  &
     &                    + (mdx_n_rlm_rtm(j_rlm)-1)                    &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
            i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
!            vt_rtm(in_rtm) = vt_rtm(in_rtm)                            &
            vr_rtm_fdout(in_rtm,3*nd-1) = vr_rtm_fdout(in_rtm,3*nd-1)   &
     &                       + sp_rlm_fdout(i_rlm,3*nd  ) * pg_tmp
!
!            vp_rtm(in_rtm) = vp_rtm(in_rtm)                            &
            vr_rtm_fdout(in_rtm,3*nd  ) = vr_rtm_fdout(in_rtm,3*nd  )   &
     &                       + sp_rlm_fdout(i_rlm,3*nd-1) * pg_tmp
!
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_fdout(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, nd_kr
!
!
!$omp parallel do private(nd_kr,k_rtm,nd,j_rlm,l_rtm,ip_rtm,in_rtm,i_rlm)
      do nd_kr = 1, nb*nidx_rtm(1)
        k_rtm = mod(nd_kr-1,nidx_rtm(1)) + 1
        nd = (nd_kr - k_rtm) / nidx_rtm(1) + 1
        do j_rlm = 1, nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
            ip_rtm = l_rtm + (k_rtm-1) * nidx_rtm(2)                    &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1)                    &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
            i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            vr_rtm_fdout(ip_rtm,nd) = vr_rtm_fdout(ip_rtm,nd)           &
     &                  + sp_rlm_fdout(i_rlm,nd) * P_rtm(l_rtm,j_rlm)
!
            end do
          end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_fdout
