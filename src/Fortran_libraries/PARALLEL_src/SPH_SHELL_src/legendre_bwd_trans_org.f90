!>@file   legendre_bwd_trans_org.f90
!!@brief  module legendre_bwd_trans_org
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Original version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_org(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_org(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_org
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
      subroutine legendre_b_trans_vector_org(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm, nd
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, jed
      real(kind = kreal) :: a2r_1d_rlm_r
      real(kind = kreal) :: vr1, vr2, vr3
      real(kind = kreal) :: Pg3_j(nidx_rlm(2))
      real(kind = kreal) :: dPdt_j(nidx_rlm(2))
      real(kind = kreal) :: Pgv_j(nidx_rlm(2))
!
!
!$omp parallel do private(j_rlm,l_rtm,nd,i_rlm,a2r_1d_rlm_r)
      do k_rtm = 1,  nidx_rtm(1)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)
        do j_rlm = 1, nidx_rlm(2)
          do nd = 1, nvector
            i_rlm = 3*nd + (j_rlm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp * nidx_rlm(2)
!
            sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) * a2r_1d_rlm_r
            sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1) * a_r_1d_rlm_r(k_rtm)
            sp_rlm(i_rlm  ) = sp_rlm(i_rlm  ) * a_r_1d_rlm_r(k_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm,l_rtm,nd,ip_rtm,in_rtm,i_rlm,jst,jed,   &
!$omp&                    mp_rlm,vr1,vr2,vr3,Pg3_j,dPdt_j)
      do mp_rlm = 1, nidx_rtm(3)
        jst = lstack_rlm(mp_rlm-1) + 1
        jed = lstack_rlm(mp_rlm)
        do k_rtm = 1,  nidx_rtm(1)
          do l_rtm = 1, nidx_rtm(2)
            do j_rlm = jst, jed
              Pg3_j(j_rlm) = P_jl(j_rlm,l_rtm) * g_sph_rlm(j_rlm,3)
              dPdt_j(j_rlm) = dPdt_jl(j_rlm,l_rtm)
            end do
!
            do nd = 1, nvector
              ip_rtm = 3*nd + (l_rtm-1) * ncomp                         &
     &                     + (k_rtm-1) * ncomp*nidx_rtm(2)              &
     &                     + (mp_rlm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
              vr1 = 0.0d0
              vr2 = 0.0d0
              vr3 = 0.0d0
              do j_rlm = jst, jed
                i_rlm = 3*nd + (j_rlm-1) * ncomp                        &
     &                     + (k_rtm-1) * ncomp * nidx_rlm(2)
!
                vr1 = vr1 + sp_rlm(i_rlm-2) * Pg3_j(j_rlm)
                vr2 = vr2 + sp_rlm(i_rlm-1) * dPdt_j(j_rlm)
                vr3 = vr3 - sp_rlm(i_rlm  ) * dPdt_j(j_rlm)
              end do
              vr_rtm(ip_rtm-2) = vr1
              vr_rtm(ip_rtm-1) = vr2
              vr_rtm(ip_rtm  ) = vr3
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm,l_rtm,nd,ip_rtm,in_rtm,i_rlm,jst,jed,   &
!$omp&                    mp_rlm,mn_rlm,vr2,vr3,Pgv_j)
      do mp_rlm = 1, nidx_rtm(3)
        mn_rlm = nidx_rtm(3) - mp_rlm + 1
        jst = lstack_rlm(mp_rlm-1) + 1
        jed = lstack_rlm(mp_rlm)
        do k_rtm = 1,  nidx_rtm(1)
          do l_rtm = 1, nidx_rtm(2)
            do j_rlm = jst, jed
              Pgv_j(j_rlm) = -P_jl(j_rlm,l_rtm)                         &
     &                      * dble(idx_gl_1d_rlm_j(j_rlm,3))            &
     &                       *asin_theta_1d_rtm(l_rtm)
            end do
!
            do nd = 1, nvector
              in_rtm = 3*nd + (l_rtm-1) * ncomp                         &
     &                     + (k_rtm-1) * ncomp*nidx_rtm(2)             &
     &                     + (mn_rlm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
              vr2 = 0.0d0
              vr3 = 0.0d0
              do j_rlm = jst, jed
                i_rlm = 3*nd + (j_rlm-1) * ncomp                        &
     &                       + (k_rtm-1) * ncomp * nidx_rlm(2)
!
                vr2 = vr2 + sp_rlm(i_rlm  ) * Pgv_j(j_rlm)
                vr3 = vr3 + sp_rlm(i_rlm-1) * Pgv_j(j_rlm)
              end do
              vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1) + vr2
              vr_rtm(in_rtm  ) = vr_rtm(in_rtm  ) + vr3
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_org(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, nd
      integer(kind = kint) :: mp_rlm, jst, jed
      real(kind = kreal) :: vr1
      real(kind = kreal) :: P_j(nidx_rlm(2))
!
!
!$omp parallel do private(j_rlm,l_rtm,nd,ip_rtm,i_rlm,mp_rlm,jst,jed,   &
!$omp&                    vr1,P_j)
      do mp_rlm = 1, nidx_rtm(3)
        jst = lstack_rlm(mp_rlm-1) + 1
        jed = lstack_rlm(mp_rlm)
        do k_rtm = 1,  nidx_rtm(1)
!
          do l_rtm = 1, nidx_rtm(2)
            P_j(jst:jed) = P_jl(jst:jed,l_rtm)
            do nd = 1, nscalar
              ip_rtm = nd + 3*nvector + (l_rtm-1) * ncomp               &
     &                    + (k_rtm-1) * ncomp*nidx_rtm(2)               &
     &                    + (mp_rlm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
              vr1 = 0.0d0
              do j_rlm = jst, jed
                i_rlm = nd + 3*nvector + (j_rlm-1) * ncomp              &
     &                                 + (k_rtm-1) * ncomp*nidx_rlm(2)
!
                vr1 = vr1 + sp_rlm(i_rlm) * P_j(j_rlm)
              end do
              vr_rtm(ip_rtm) = vr1
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_org
