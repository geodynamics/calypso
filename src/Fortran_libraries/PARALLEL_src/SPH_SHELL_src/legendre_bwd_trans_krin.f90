!>@file   legendre_bwd_trans_krin.f90
!!@brief  module legendre_bwd_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_krin(ncomp, nvector,         &
!!     &          sp_rlm_krin, vr_rtm_spin)
!!      subroutine legendre_b_trans_scalar_krin(ncomp, nvector, nscalar,&
!!     &          sp_rlm_krin, vr_rtm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_krin
!!
!!     field data for Legendre transform
!!         original layout: vr_rtm_krin(i_fld,k_rtm,l_rtm,m_rtm,nd)
!!         size: vr_rtm_krin(nidx_rtm(1)*nidx_rtm(2)*nidx_rtm(3),nb)
!!      real(kind = kreal), allocatable :: vr_rtm_krin(:,:)
!!
!!     spectr data for Legendre transform
!!        original layout: sp_rlm_krin(k_rtm,j_rlm,i_fld)
!!        size: sp_rlm_krin(nidx_rlm(1)*nidx_rlm(2),nb)
!!      real(kind = kreal), allocatable :: sp_rlm_krin(:,:)
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of fields to be transformed
!
      module legendre_bwd_trans_krin
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
      subroutine legendre_b_trans_vector_krin(ncomp, nvector,           &
     &          sp_rlm_krin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &    :: sp_rlm_krin(nidx_rlm(1)*ncomp,nidx_rlm(2))
      real(kind = kreal), intent(inout)                                 &
     &    :: vr_rtm_spin(nidx_rtm(1)*ncomp,nidx_rtm(3),nidx_rtm(2))
!
      integer(kind = kint) :: ip, lst, led
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd, jst, jed
      real(kind = kreal) :: vr1(nvector*nidx_rtm(1))
      real(kind = kreal) :: vr2(nvector*nidx_rtm(1))
      real(kind = kreal) :: vr3(nvector*nidx_rtm(1))
      real(kind = kreal) :: Pg3_j(nidx_rlm(2))
      real(kind = kreal) :: dPdt_j(nidx_rlm(2))
      real(kind = kreal) :: Pgv_j(nidx_rlm(2))
!
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,lst,led,j_rlm,kr_nd,l_rtm,mp_rlm,jst,jed,  &
!$omp&                    vr1,vr2,vr3,Pg3_j,dPdt_j)
      do ip = 1, np_smp
        lst = idx_rtm_smp_stack(ip-1,2) + 1
        led = idx_rtm_smp_stack(ip,  2)
        do l_rtm = lst, led
          do j_rlm = 1, nidx_rlm(2)
            Pg3_j(j_rlm) = P_jl(j_rlm,l_rtm) * g_sph_rlm(j_rlm,3)
            dPdt_j(j_rlm) = dPdt_jl(j_rlm,l_rtm)
          end do
!
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1) + 1
            jed = lstack_rlm(mp_rlm)
            vr1(1:nb_nri) = 0.0d0
            vr2(1:nb_nri) = 0.0d0
            vr3(1:nb_nri) = 0.0d0
            do j_rlm = jst, jed
              do kr_nd = 1, nb_nri
                vr1(kr_nd) = vr1(kr_nd)                                 &
     &                + sp_rlm_krin(kr_nd,         j_rlm)*Pg3_j(j_rlm)
!
                vr2(kr_nd) = vr2(kr_nd)                                 &
     &                + sp_rlm_krin(kr_nd+nb_nri,  j_rlm)*dPdt_j(j_rlm)
!
                vr3(kr_nd) = vr3(kr_nd)                                 &
     &                - sp_rlm_krin(kr_nd+2*nb_nri,j_rlm)*dPdt_j(j_rlm)
              end do
            end do
            do kr_nd = 1, nb_nri
              vr_rtm_spin(kr_nd,         mp_rlm,l_rtm) = vr1(kr_nd)
              vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,l_rtm) = vr2(kr_nd)
              vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,l_rtm) = vr3(kr_nd)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,lst,led,j_rlm,kr_nd,l_rtm,jst,jed,         &
!$omp&                    mp_rlm,mn_rlm,vr2,vr3,Pgv_j)
      do ip = 1, np_smp
        lst = idx_rtm_smp_stack(ip-1,2) + 1
        led = idx_rtm_smp_stack(ip,  2)
        do l_rtm = lst, led
          do j_rlm = 1, nidx_rlm(2)
            Pgv_j(j_rlm) = -P_jl(j_rlm,l_rtm)                           &
     &        * dble(idx_gl_1d_rlm_j(j_rlm,3))*asin_theta_1d_rtm(l_rtm)
          end do
!
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = 1 + nidx_rtm(3) - mp_rlm
            jst = lstack_rlm(mp_rlm-1) + 1
            jed = lstack_rlm(mp_rlm)
            vr2(1:nb_nri) = 0.0d0
            vr3(1:nb_nri) = 0.0d0
            do j_rlm = jst, jed
              do kr_nd = 1, nb_nri
                vr2(kr_nd) = vr2(kr_nd)                                 &
     &            + sp_rlm_krin(kr_nd+2*nb_nri,j_rlm)*Pgv_j(j_rlm)
!
                vr3(kr_nd) = vr3(kr_nd)                                 &
     &            + sp_rlm_krin(kr_nd+nb_nri,  j_rlm)*Pgv_j(j_rlm)
              end do
            end do
            do kr_nd = 1, nb_nri
              vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,l_rtm)                  &
     &          = vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,l_rtm) + vr2(kr_nd)
              vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,l_rtm)                  &
     &          = vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,l_rtm) + vr3(kr_nd)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_krin(ncomp, nvector, nscalar,  &
     &          sp_rlm_krin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nscalar, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_krin(nidx_rlm(1)*ncomp,nidx_rlm(2))
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(1)*ncomp,nidx_rtm(3),nidx_rtm(2))
!
      integer(kind = kint) :: ip, lst, led
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: kr_nd, nb_nri, kst, jst, jed
      real(kind = kreal) :: vr1(nscalar*nidx_rtm(1))
      real(kind = kreal) :: P_j(nidx_rlm(2))
!
!
      kst = 3*nvector * nidx_rtm(1)
      nb_nri = nscalar*nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,lst,led,j_rlm,kr_nd,l_rtm,mp_rlm,          &
!$omp&                    jst,jed,vr1,P_j)
      do ip = 1, np_smp
        lst = idx_rtm_smp_stack(ip-1,2) + 1
        led = idx_rtm_smp_stack(ip,  2)
        do l_rtm = lst, led
          P_j(1:nidx_rlm(2)) =  P_jl(1:nidx_rlm(2),l_rtm)
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1) + 1
            jed = lstack_rlm(mp_rlm)
            vr1(1:nb_nri) = 0.0d0
            do j_rlm = jst, jed
              do kr_nd = 1, nb_nri
!              do nd = 1, nscalar
!                do k_rtm = 1,  nidx_rtm(1)
!                kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
                vr1(kr_nd) = vr1(kr_nd)                                 &
     &                      + sp_rlm_krin(kr_nd+kst,j_rlm)*P_j(j_rlm)
              end do
            end do
            do kr_nd = 1, nb_nri
              vr_rtm_spin(kr_nd+kst,mp_rlm,l_rtm) = vr1(kr_nd)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_krin
