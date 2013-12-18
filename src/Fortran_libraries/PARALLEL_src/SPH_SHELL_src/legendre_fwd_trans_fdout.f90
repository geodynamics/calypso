!>@file   legendre_fwd_trans_fdout.f90
!!@brief  module legendre_fwd_trans_fdout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (outmost field loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_fdout(nvector)
!!        Input:  vr_rtm_fdout   (Order: radius,theta,phi)
!!        Output: sp_rlm_fdout   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_fdout(nvector)
!!        Input:  vr_rtm_fdout
!!        Output: sp_rlm_fdout
!!@endverbatim
!!
!!@n @param  nvector  number of fields to be transformed
!
      module legendre_fwd_trans_fdout
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
      subroutine legendre_f_trans_vector_fdout(nvector)
!
      integer(kind = kint), intent(in) :: nvector
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, nd_kr
      real(kind = kreal) :: pwt_tmp, dpwt_tmp, pgwt_tmp
!
!
!$omp parallel do private(nd_kr,j_rlm,k_rlm,i_rlm,ip_rtm,in_rtm,        &
!$omp&               pwt_tmp,dpwt_tmp,pgwt_tmp)
      do nd_kr = 1, nvector*nidx_rlm(1)
        k_rlm = mod(nd_kr-1,nidx_rlm(1)) + 1
        nd = (nd_kr - k_rlm) / nidx_rlm(1) + 1
        do l_rtm = 1, nidx_rtm(2)
!
          do j_rlm = 1, nidx_rlm(2)
            pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
            dpwt_tmp = dPdt_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
            pgwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)           &
     &                 * dble( idx_gl_1d_rlm_j(j_rlm,3) )               &
     &                 * asin_theta_1d_rtm(l_rtm)
!
            i_rlm = j_rlm + (k_rlm-1) *nidx_rlm(2)
            ip_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                    &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                   * nidx_rtm(1)*nidx_rtm(2)
            in_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                    &
     &                 + (mdx_n_rlm_rtm(j_rlm)-1)                       &
     &                   * nidx_rtm(1) * nidx_rtm(2)
!
            sp_rlm_fdout(i_rlm,3*nd-2) = sp_rlm_fdout(i_rlm,3*nd-2)     &
     &                     + vr_rtm_fdout(ip_rtm,3*nd-2) * pwt_tmp
!
            sp_rlm_fdout(i_rlm,3*nd-1) = sp_rlm_fdout(i_rlm,3*nd-1)     &
     &                 + ( vr_rtm_fdout(ip_rtm,3*nd-1) * dpwt_tmp       &
     &                   - vr_rtm_fdout(in_rtm,3*nd  ) * pgwt_tmp)
!
            sp_rlm_fdout(i_rlm,3*nd  ) = sp_rlm_fdout(i_rlm,3*nd  )     &
     &                 - ( vr_rtm_fdout(in_rtm,3*nd-1) * pgwt_tmp       &
     &                   + vr_rtm_fdout(ip_rtm,3*nd  ) * dpwt_tmp )
          end do
        end do
!
        do j_rlm = 1, nidx_rlm(2)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
          sp_rlm_fdout(i_rlm,3*nd-2) = sp_rlm_fdout(i_rlm,3*nd-2)       &
     &                            * g_sph_rlm(j_rlm,7)
          sp_rlm_fdout(i_rlm,3*nd-1) = sp_rlm_fdout(i_rlm,3*nd-1)       &
     &                            * g_sph_rlm(j_rlm,7)
          sp_rlm_fdout(i_rlm,3*nd  ) = sp_rlm_fdout(i_rlm,3*nd  )       &
     &                            * g_sph_rlm(j_rlm,7)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_fdout(nvector)
!
      integer(kind = kint), intent(in) :: nvector
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm
      integer(kind = kint) :: nd, nd_kr
      real(kind = kreal) :: pwt_tmp
!
!
!$omp parallel do private(nd_kr,j_rlm,k_rlm,i_rlm,ip_rtm,pwt_tmp)
      do nd_kr = 1, nvector*nidx_rlm(1)
        k_rlm = mod(nd_kr-1,nidx_rlm(1)) + 1
        nd = (nd_kr - k_rlm) / nidx_rlm(1) + 1
        do l_rtm = 1, nidx_rtm(2)
!
          do j_rlm = 1, nidx_rlm(2)
            pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
            i_rlm = j_rlm + (k_rlm-1) * nidx_rlm(2)
            ip_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                    &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * nidx_rtm(1) * nidx_rtm(2)
!
            sp_rlm_fdout(i_rlm,nd) = sp_rlm_fdout(i_rlm,nd)             &
     &                          + vr_rtm_fdout(ip_rtm,nd) * pwt_tmp
          end do
        end do
!
        do j_rlm = 1, nidx_rlm(2)
          i_rlm = j_rlm + (k_rlm-1) * nidx_rlm(2)
!
          sp_rlm_fdout(i_rlm,nd) = sp_rlm_fdout(i_rlm,nd)               &
     &                            * g_sph_rlm(j_rlm,6)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_fdout
