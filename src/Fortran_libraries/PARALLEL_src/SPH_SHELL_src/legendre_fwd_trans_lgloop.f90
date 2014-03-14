!>@file   legendre_fwd_trans_lgloop.f90
!!@brief  module legendre_fwd_trans_lgloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (longest loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_long(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_long(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_lgloop
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
      subroutine legendre_f_trans_vector_long(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd
      real(kind = kreal) :: pwt_tmp, dpwt_tmp, pgwt_tmp
!
!
!$omp  parallel do private(ip,ist,ied,inum,k_rlm,j_rlm,nd,inod,i_rlm,   &
!$omp&            l_rtm,ip_rtm,in_rtm,pwt_tmp,dpwt_tmp,pgwt_tmp)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd =    1 + mod( (inum-1),nvector)
          inod  = 1 + (inum - nd) / nvector
          j_rlm = 1 + mod( (inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm = 3*nd + (j_rlm-1) * ncomp                              &
     &                 + (k_rlm-1) * ncomp*nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
            ip_rtm = 3*nd + (l_rtm-1)  * ncomp                          &
     &                 + (k_rlm-1)  * ncomp * nidx_rtm(2)               &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * ncomp * nidx_rtm(1) * nidx_rtm(2)
            in_rtm = 3*nd + (l_rtm-1)  * ncomp                          &
     &                 + (k_rlm-1)  * ncomp * nidx_rtm(2)               &
     &                 + (mdx_n_rlm_rtm(j_rlm)-1)                       &
     &                  * ncomp * nidx_rtm(1) * nidx_rtm(2)
!
            pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
            dpwt_tmp = dPdt_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
            pgwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)           &
     &                  * dble( idx_gl_1d_rlm_j(j_rlm,3) )              &
     &                  * asin_theta_1d_rtm(l_rtm)
!
            sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2)                           &
     &                     + vr_rtm(ip_rtm-2) * pwt_tmp
!
            sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                           &
     &                 + ( vr_rtm(ip_rtm-1) * dpwt_tmp                  &
     &                   - vr_rtm(in_rtm  ) * pgwt_tmp)
!
            sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                           &
     &                 - ( vr_rtm(in_rtm-1) * pgwt_tmp                  &
     &                   + vr_rtm(ip_rtm  ) * dpwt_tmp )
          end do
!
          sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) * g_sph_rlm(j_rlm,7)
          sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1) * g_sph_rlm(j_rlm,7)
          sp_rlm(i_rlm  ) = sp_rlm(i_rlm  ) * g_sph_rlm(j_rlm,7)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_long(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, l_rtm
      integer(kind = kint) :: ip_rtm
      integer(kind = kint) :: nd
      real(kind = kreal) :: pwt_tmp
!
!
!$omp  parallel do private(ip,ist,ied,inum,k_rlm,j_rlm,nd,inod,         &
!$omp&                     i_rlm,l_rtm,ip_rtm,pwt_tmp)
      do ip = 1, np_smp
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd =    1 + mod( (inum-1),nscalar)
          inod  = 1 + (inum - nd) / nscalar
          j_rlm = 1 + mod( (inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm = nd + 3*nvector + (j_rlm-1) * ncomp                    &
     &                           + (k_rlm-1) * ncomp*nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
            pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
            ip_rtm = nd + 3*nvector + (l_rtm-1)  * ncomp                &
     &              + (k_rlm-1)  * ncomp * nidx_rtm(2)                  &
     &              + (mdx_p_rlm_rtm(j_rlm)-1)                          &
     &                * ncomp * nidx_rtm(1) * nidx_rtm(2)
!
            sp_rlm(i_rlm) = sp_rlm(i_rlm) + vr_rtm(ip_rtm) * pwt_tmp
          end do
!
          sp_rlm(i_rlm) = sp_rlm(i_rlm) * g_sph_rlm(j_rlm,6)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_lgloop
