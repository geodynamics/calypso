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
!!      subroutine legendre_f_trans_vector_org(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_org(ncomp, nvector, nscalar)
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
      subroutine legendre_f_trans_vector_org(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd
      real(kind = kreal) :: pwt_tmp, dpwt_tmp, pgwt_tmp
!
!
!$omp parallel do private(l_rtm,j_rlm,k_rlm,nd,i_rlm,ip_rtm,in_rtm,     &
!$omp&               pwt_tmp,dpwt_tmp,pgwt_tmp)
      do j_rlm = 1, nidx_rlm(2)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nvector
            i_rlm = 3*nd + (j_rlm-1) * ncomp                            &
     &                 + (k_rlm-1) * ncomp*nidx_rlm(2)
!
            do l_rtm = 1, nidx_rtm(2)
              ip_rtm = 3*nd + (l_rtm-1)  * ncomp                        &
     &                 + (k_rlm-1)  * ncomp * nidx_rtm(2)               &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * ncomp * nidx_rtm(1) * nidx_rtm(2)
              in_rtm = 3*nd + (l_rtm-1)  * ncomp                        &
     &                 + (k_rlm-1)  * ncomp * nidx_rtm(2)               &
     &                 + (mdx_n_rlm_rtm(j_rlm)-1)                       &
     &                  * ncomp * nidx_rtm(1) * nidx_rtm(2)
!
              pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              dpwt_tmp = dPdt_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              pgwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)         &
     &                  * dble( idx_gl_1d_rlm_j(j_rlm,3) )              &
     &                  * asin_theta_1d_rtm(l_rtm)
!
              sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2)                         &
     &                     + vr_rtm(ip_rtm-2) * pwt_tmp
!
              sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                         &
     &                 + ( vr_rtm(ip_rtm-1) * dpwt_tmp                  &
     &                   - vr_rtm(in_rtm  ) * pgwt_tmp)
!
              sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                         &
     &                 - ( vr_rtm(in_rtm-1) * pgwt_tmp                  &
     &                   + vr_rtm(ip_rtm  ) * dpwt_tmp )
            end do
!
            sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) * g_sph_rlm(j_rlm,7)
            sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1) * g_sph_rlm(j_rlm,7)
            sp_rlm(i_rlm  ) = sp_rlm(i_rlm  ) * g_sph_rlm(j_rlm,7)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_org(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm
      integer(kind = kint) :: nd
      real(kind = kreal) :: pwt_tmp
!
!
!$omp parallel do private(j_rlm,k_rlm,nd,i_rlm,ip_rtm,l_rtm,pwt_tmp)
      do j_rlm = 1, nidx_rlm(2)
!
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nscalar
            i_rlm = nd + 3*nvector + (j_rlm-1) * ncomp                  &
     &                             + (k_rlm-1) * ncomp*nidx_rlm(2)
!
            do l_rtm = 1, nidx_rtm(2)
              pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              ip_rtm = nd + 3*nvector + (l_rtm-1)  * ncomp              &
     &                + (k_rlm-1)  * ncomp * nidx_rtm(2)                &
     &                + (mdx_p_rlm_rtm(j_rlm)-1)                        &
     &                  * ncomp * nidx_rtm(1) * nidx_rtm(2)
!
!              sp_rlm(i_rlm) = sp_rlm(i_rlm)                            &
              sp_rlm(i_rlm) = sp_rlm(i_rlm) + vr_rtm(ip_rtm) * pwt_tmp
            end do
!
            sp_rlm(i_rlm) = sp_rlm(i_rlm) * g_sph_rlm(j_rlm,6)
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
