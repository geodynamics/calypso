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
!!      subroutine legendre_b_trans_scalar_spin(ncomp, nvector, nscalar,&
!!     &          sp_rlm_spin, vr_rtm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm_spin(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3)*nidx_rtm(1),ncomp)
!!      real(kind = kreal), allocatable :: vr_rtm_spin(:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm_spin(j_rlm,k_rtm,icomp)
!!        size: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm_spin(:,:)
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
      real(kind = kreal), intent(inout)                                 &
     &    :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &    :: vr_rtm_spin(nidx_rtm(1)*ncomp,nidx_rtm(3),nidx_rtm(2))
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd, k_rlm, jst, jed
      real(kind = kreal) :: a2r_1d_rlm_r
      real(kind = kreal) :: vr1, vr2, vr3
      real(kind = kreal) :: Pg3_j(nidx_rlm(2))
      real(kind = kreal) :: dPdt_j(nidx_rlm(2))
      real(kind = kreal) :: Pgv_j(nidx_rlm(2))
!
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel private(kr_nd,k_rlm,a2r_1d_rlm_r)
      do kr_nd = 1, nb_nri
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!        nd =  1 + (kr_nd - k_rlm) / nidx_rlm(1)
!$omp do private(j_rlm)
        do j_rlm = 1, nidx_rlm(2)
          sp_rlm_spin(j_rlm,kr_nd         )                             &
     &        = sp_rlm_spin(j_rlm,kr_nd         ) * a2r_1d_rlm_r
          sp_rlm_spin(j_rlm,kr_nd+nb_nri  )                             &
     &        = sp_rlm_spin(j_rlm,kr_nd+nb_nri  ) * a_r_1d_rlm_r(k_rlm)
          sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)                             &
     &        = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri) * a_r_1d_rlm_r(k_rlm)
        end do
!$omp end do
      end do
!$omp end parallel
!
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,jst,jed,             &
!$omp&                    vr1,vr2,vr3,Pg3_j,dPdt_j)
      do l_rtm = 1, nidx_rtm(2)
        do j_rlm = 1, nidx_rlm(2)
          Pg3_j(j_rlm) = P_jl(j_rlm,l_rtm) * g_sph_rlm(j_rlm,3)
          dPdt_j(j_rlm) = dPdt_jl(j_rlm,l_rtm)
        end do
!
        do mp_rlm = 1, nidx_rtm(3)
          jst = lstack_rlm(mp_rlm-1) + 1
          jed = lstack_rlm(mp_rlm)
          do kr_nd = 1, nb_nri
            vr1 = 0.0d0
            vr2 = 0.0d0
            vr3 = 0.0d0
            do j_rlm = jst, jed
              vr1 = vr1                                                 &
     &         + sp_rlm_spin(j_rlm,kr_nd         )*Pg3_j(j_rlm)
!
              vr2 = vr2                                                 &
     &         + sp_rlm_spin(j_rlm,kr_nd+nb_nri  )*dPdt_j(j_rlm)
!
              vr3 = vr3                                                 &
     &         - sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)*dPdt_j(j_rlm)
            end do
            vr_rtm_spin(kr_nd,         mp_rlm,l_rtm) = vr1
            vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,l_rtm) = vr2
            vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,l_rtm) = vr3
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,mn_rlm,jst,jed,      &
!$omp&                    vr2,vr3,Pgv_j)
      do l_rtm = 1, nidx_rtm(2)
        do j_rlm = 1, nidx_rlm(2)
          Pgv_j(j_rlm) = -P_jl(j_rlm,l_rtm)                             &
     &        * dble(idx_gl_1d_rlm_j(j_rlm,3))*asin_theta_1d_rtm(l_rtm)
        end do
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = 1 + nidx_rtm(3) - mp_rlm
          jst = lstack_rlm(mp_rlm-1) + 1
          jed = lstack_rlm(mp_rlm)
          do kr_nd = 1, nb_nri
            vr2 = 0.0d0
            vr3 = 0.0d0
            do j_rlm = jst, jed
              vr2 = vr2                                                 &
     &            + sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)*Pgv_j(j_rlm)
!
              vr3 = vr3                                                 &
     &            + sp_rlm_spin(j_rlm,kr_nd+nb_nri  )*Pgv_j(j_rlm)
            end do
            vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,l_rtm)                    &
     &          = vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,l_rtm) + vr2
            vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,l_rtm)                    &
     &          = vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,l_rtm) + vr3
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_spin(ncomp, nvector, nscalar,  &
     &          sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nscalar, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(1)*ncomp,nidx_rtm(3),nidx_rtm(2))
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: kr_nd, kst, ked, jst, jed
      real(kind = kreal) :: vr1
      real(kind = kreal) :: P_j(nidx_rlm(2))
!
!
      kst = 1 + 3*nvector * nidx_rtm(1)
      ked = (nscalar + 3*nvector) * nidx_rtm(1)
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,jst,jed,vr1,P_j)
      do l_rtm = 1, nidx_rtm(2)
        P_j(1:nidx_rlm(2)) =  P_jl(1:nidx_rlm(2),l_rtm)
        do mp_rlm = 1, nidx_rtm(3)
          jst = lstack_rlm(mp_rlm-1) + 1
          jed = lstack_rlm(mp_rlm)
          do kr_nd = kst, ked
!          do nd = nvector+1, nvector+nscalar
!            do k_rtm = 1,  nidx_rtm(1)
!              kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
            vr1 = 0.0d0
            do j_rlm = jst, jed
              vr1 = vr1 + sp_rlm_spin(j_rlm,kr_nd)*P_j(j_rlm)
            end do
            vr_rtm_spin(kr_nd,mp_rlm,l_rtm) = vr1
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_spin
