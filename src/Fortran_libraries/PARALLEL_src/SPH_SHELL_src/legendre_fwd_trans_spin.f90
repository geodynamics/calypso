!>@file   legendre_fwd_trans_spin.f90
!!@brief  module legendre_fwd_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_spin(nb)
!!        Input:  vr_rtm_spin   (Order: radius,theta,phi)
!!        Output: sp_rlm_spin   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_spin(nb)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
!
      module legendre_fwd_trans_spin
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_4_sph_trans_spin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_vector_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, mst, med, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd
      real(kind = kreal) :: pwt_tmp, dpwt_tmp, pgwt_tmp
!
!
      nb_nri = nb*nidx_rlm(1)
!$omp parallel do private(j_rlm,l_rtm,mp_rlm,mn_rlm,                    &
!$omp&               mst,med,pwt_tmp,dpwt_tmp,pgwt_tmp)
!cdir nodep
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              pwt_tmp =  P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              dpwt_tmp = dPdt_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              pgwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)         &
     &                 * dble( idx_gl_1d_rlm_j(j_rlm,3) )               &
     &                 * asin_theta_1d_rtm(l_rtm)
!
              sp_rlm_spin(j_rlm,kr_nd,1) = sp_rlm_spin(j_rlm,kr_nd,1)   &
     &                 + vr_rtm_spin(l_rtm,mp_rlm,kr_nd,1) * pwt_tmp
!
              sp_rlm_spin(j_rlm,kr_nd,2) = sp_rlm_spin(j_rlm,kr_nd,2)   &
     &               + ( vr_rtm_spin(l_rtm,mp_rlm,kr_nd,2) * dpwt_tmp   &
     &                 - vr_rtm_spin(l_rtm,mn_rlm,kr_nd,3) * pgwt_tmp)
!
              sp_rlm_spin(j_rlm,kr_nd,3) = sp_rlm_spin(j_rlm,kr_nd,3)   &
     &               - ( vr_rtm_spin(l_rtm,mn_rlm,kr_nd,2) * pgwt_tmp   &
     &                 + vr_rtm_spin(l_rtm,mp_rlm,kr_nd,3) * dpwt_tmp)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm)
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!cdir nodep
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_spin(j_rlm,kr_nd,1) = sp_rlm_spin(j_rlm,kr_nd,1)     &
     &                           * g_sph_rlm(j_rlm,7)
            sp_rlm_spin(j_rlm,kr_nd,2) = sp_rlm_spin(j_rlm,kr_nd,2)     &
     &                           * g_sph_rlm(j_rlm,7)
            sp_rlm_spin(j_rlm,kr_nd,3) = sp_rlm_spin(j_rlm,kr_nd,3)     &
     &                           * g_sph_rlm(j_rlm,7)
          end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm, mst, med
      integer(kind = kint) :: nb_nri, kr_nd
      real(kind = kreal) :: pwt_tmp
!
!
      nb_nri = nb*nidx_rlm(1)
!$omp parallel do private(j_rlm,l_rtm,pwt_tmp,mp_rlm,mst,med)
!cdir nodep
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!
        do mp_rlm = 1, nidx_rtm(3)
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              pwt_tmp =  P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
!
              sp_rlm_spin(j_rlm,kr_nd,1) = sp_rlm_spin(j_rlm,kr_nd,1)   &
     &                  + vr_rtm_spin(l_rtm,mp_rlm,kr_nd,1) * pwt_tmp
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm)
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!cdir nodep
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_spin(j_rlm,kr_nd,1) = sp_rlm_spin(j_rlm,kr_nd,1)     &
     &                                  * g_sph_rlm(j_rlm,6)
          end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_spin

