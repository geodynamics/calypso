!>@file   legendre_bwd_trans_spin.f90
!!@brief  module legendre_bwd_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_spin(ncomp, nvector,         &
!!     &          sp_rlm_spin, vr_rtm_spin)
!!      subroutine legendre_b_trans_scalar_spin(ncomp, nscalar, nvector,&
!!     &          sp_rlm_spin, vr_rtm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of fields to be transformed
!
      module legendre_bwd_trans_spin
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
      subroutine legendre_b_trans_vector_spin(ncomp, nvector,           &
     &          sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, mst, med, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel do private(kr_nd,j_rlm,l_rtm,mp_rlm,mn_rlm,              &
!$omp&               mst,med,pg_tmp,dp_tmp)
      do kr_nd = 1, nb_nri
!      do nd = 1, nvector
!        do k_rtm = 1,  nidx_rtm(1)
!          kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
            do l_rtm = 1, nidx_rtm(2)
              pg_tmp = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
              dp_tmp = dPdt_rtm(l_rtm,j_rlm)
!
              vr_rtm_spin(l_rtm,mp_rlm,kr_nd         )                  &
     &                     = vr_rtm_spin(l_rtm,mp_rlm,kr_nd         )   &
     &                     + sp_rlm_spin(j_rlm,kr_nd         ) * pg_tmp
!
              vr_rtm_spin(l_rtm,mp_rlm,kr_nd+nb_nri  )                  &
     &                     = vr_rtm_spin(l_rtm,mp_rlm,kr_nd+nb_nri  )   &
     &                     + sp_rlm_spin(j_rlm,kr_nd+nb_nri  ) * dp_tmp
!
              vr_rtm_spin(l_rtm,mp_rlm,kr_nd+2*nb_nri)                  &
     &                     = vr_rtm_spin(l_rtm,mp_rlm,kr_nd+2*nb_nri)   &
     &                     - sp_rlm_spin(j_rlm,kr_nd+2*nb_nri) * dp_tmp
!
            end do
!
            do l_rtm = 1, nidx_rtm(2)
              pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)    &
     &                * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
!
              vr_rtm_spin(l_rtm,mn_rlm,kr_nd+nb_nri)                    &
     &                     = vr_rtm_spin(l_rtm,mn_rlm,kr_nd+nb_nri)     &
     &                     + sp_rlm_spin(j_rlm,kr_nd+2*nb_nri) * pg_tmp
!
              vr_rtm_spin(l_rtm,mn_rlm,kr_nd+2*nb_nri)                  &
     &                     = vr_rtm_spin(l_rtm,mn_rlm,kr_nd+2*nb_nri)   &
     &                     + sp_rlm_spin(j_rlm,kr_nd+nb_nri  ) * pg_tmp
!
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_spin(ncomp, nscalar, nvector,  &
     &          sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nscalar, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm, mst, med
      integer(kind = kint) :: kr_nd, kst, ked
!
!
      kst = 1 + 3*nvector * nidx_rtm(1)
      ked = (nscalar + 3*nvector) * nidx_rtm(1)
!$omp parallel do private(kr_nd,j_rlm,l_rtm,mp_rlm,mst,med)
      do kr_nd = kst, ked
!      do nd = nvector+1, nvector+nscalar
!        do k_rtm = 1,  nidx_rtm(1)
!          kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
!
        do mp_rlm = 1, nidx_rtm(3)
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              vr_rtm_spin(l_rtm,mp_rlm,kr_nd)                           &
     &              = vr_rtm_spin(l_rtm,mp_rlm,kr_nd)                   &
     &               + sp_rlm_spin(j_rlm,kr_nd) * P_rtm(l_rtm,j_rlm)
!
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_spin
