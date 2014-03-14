!>@file   legendre_fwd_trans_krin.f90
!!@brief  module legendre_fwd_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_krin(nvector,                &
!!     &          vr_rtm_krin, sp_rlm_krin)
!!      subroutine legendre_f_trans_scalar_krin(nscalar,                &
!!     &          vr_rtm_krin, sp_rlm_krin)
!!        Input:  vr_rtm_krin
!!        Output: sp_rlm_krin
!!@endverbatim
!!
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of scalar to be transformed
!
      module legendre_fwd_trans_krin
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
      subroutine legendre_f_trans_vector_krin(nvector,                  &
     &          vr_rtm_krin, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nnod_rtm,3*nvector)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_krin(nnod_rlm,3*nvector)
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: k_rtm, nd, mst, med, kr_j, kr_l
      real(kind = kreal) :: pwt_tmp, dpwt_tmp, pgwt_tmp
!
!
!$omp parallel do private(j_rlm,l_rtm,kr_j,kr_l,mst,med,ip_rtm,in_rtm,  &
!$omp&               pwt_tmp,dpwt_tmp,pgwt_tmp,mn_rlm,k_rtm,nd)
      do mp_rlm = 1, nidx_rtm(3)
        mn_rlm = nidx_rtm(3) - mp_rlm + 1
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do nd = 1, nvector
          do l_rtm = 1, nidx_rtm(2)
!
            do j_rlm = mst, med
              pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              dpwt_tmp = dPdt_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              pgwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)         &
     &                 * dble( idx_gl_1d_rlm_j(j_rlm,3) )               &
     &                 * asin_theta_1d_rtm(l_rtm)
!
              do k_rtm = 1, nidx_rtm(1)
                kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
                kr_l = k_rtm + (l_rtm-1)*nidx_rtm(1)
                ip_rtm = kr_l + (mp_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
                in_rtm = kr_l + (mn_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
!
                sp_rlm_krin(kr_j,3*nd-2) = sp_rlm_krin(kr_j,3*nd-2)     &
     &                 + vr_rtm_krin(ip_rtm,3*nd-2) * pwt_tmp
                sp_rlm_krin(kr_j,3*nd-1) = sp_rlm_krin(kr_j,3*nd-1)     &
     &               + ( vr_rtm_krin(ip_rtm,3*nd-1) * dpwt_tmp          &
     &                 - vr_rtm_krin(in_rtm,3*nd  ) * pgwt_tmp)
                sp_rlm_krin(kr_j,3*nd  ) = sp_rlm_krin(kr_j,3*nd  )     &
     &               - ( vr_rtm_krin(in_rtm,3*nd-1) * pgwt_tmp          &
     &                 + vr_rtm_krin(ip_rtm,3*nd  ) * dpwt_tmp)
              end do
            end do
          end do
!
          do j_rlm = mst, med
            do k_rtm = 1, nidx_rtm(1)
              kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
              sp_rlm_krin(kr_j,3*nd-2)                                  &
     &           = sp_rlm_krin(kr_j,3*nd-2) * g_sph_rlm(j_rlm,7)
              sp_rlm_krin(kr_j,3*nd-1)                                  &
     &           = sp_rlm_krin(kr_j,3*nd-1) * g_sph_rlm(j_rlm,7)
              sp_rlm_krin(kr_j,3*nd  )                                  &
     &           = sp_rlm_krin(kr_j,3*nd  ) * g_sph_rlm(j_rlm,7)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_krin(nscalar,                  &
     &          vr_rtm_krin, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nnod_rtm,nscalar)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_krin(nnod_rlm,nscalar)
!
      integer(kind = kint) :: j_rlm, l_rtm, mst, med, mp_rlm, ip_rtm
      integer(kind = kint) :: k_rtm, nd, kr_j, kr_l
      real(kind = kreal) :: pwt_tmp
!
!
!$omp parallel do private(j_rlm,k_rtm,kr_j,nd,kr_l,mst,med,l_rtm,       &
!$omp&                   ip_rtm,pwt_tmp)
      do mp_rlm = 1, nidx_rtm(3)
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do nd = 1, nscalar
          do l_rtm = 1, nidx_rtm(2)
            do j_rlm = mst, med
              pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
!
              do k_rtm = 1, nidx_rtm(1)
                kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
                kr_l = k_rtm + (l_rtm-1)*nidx_rtm(1)
                ip_rtm = kr_l + (mp_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
!
                sp_rlm_krin(kr_j,nd) = sp_rlm_krin(kr_j,nd)             &
     &                    + vr_rtm_krin(ip_rtm,nd) * pwt_tmp
              end do
            end do
          end do
!
          do j_rlm = mst, med
            do k_rtm = 1, nidx_rtm(1)
              kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
              sp_rlm_krin(kr_j,nd) = sp_rlm_krin(kr_j,nd)               &
     &                               * g_sph_rlm(j_rlm,6)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_krin
