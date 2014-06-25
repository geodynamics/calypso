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
!!      subroutine legendre_b_trans_vector_fdout(nvector,               &
!!     &          sp_rlm_fdout, vr_rtm_fdout)
!!      subroutine legendre_b_trans_scalar_fdout(nscalar,               &
!!     &          sp_rlm_fdout, vr_rtm_fdout)
!!        Input:  vr_rtm_fdout
!!        Output: sp_rlm_fdout
!!
!!      field data for Legendre transform  @f$ f(r,\theta,m) @f$ 
!!       Order: vr_rtm_fdout(l_rtm,k_rtm,m_rtm,i_comp,i_fld)
!!       size:  vr_rtm_fdout(nidx_rtm(2)*nidx_rtm(1)*nidx_rtm(3),3*nb)
!!      real(kind = kreal), allocatable :: vr_rtm_fdout(:,:)
!!
!!      Spectr data for Legendre transform  @f$ f(r,l,m) @f$ 
!!        Order: sp_rlm(i_comp,i_fld,j_rlm,k_rtm)
!!        size: sp_rlm(nidx_rlm(2)*nidx_rtm(1),3*nb)
!!      real(kind = kreal), allocatable :: sp_rlm_fdout(:,:)
!!@endverbatim
!!
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
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
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_vector_fdout(nvector,                 &
     &          sp_rlm_fdout, vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: nvector
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_fdout(nnod_rlm,3*nvector)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_fdout(nnod_rtm,3*nvector)
!
      integer(kind = kint) :: i_rlm, j_rlm, jst, jed, mp_rlm, mn_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: inum, nd
      real(kind = kreal) :: vr1, vr2, vr3
      real(kind = kreal) :: Pg3_j(nidx_rlm(2))
      real(kind = kreal) :: dPdt_j(nidx_rlm(2))
      real(kind = kreal) :: Pgv_j(nidx_rlm(2))
!
!
!$omp parallel do private(inum,j_rlm,k_rtm,i_rlm,l_rtm,nd,jst,jed,      &
!$omp&                    ip_rtm,in_rtm,mp_rlm,mn_rlm,vr1,vr2,vr3,      &
!$omp&                    Pg3_j,dPdt_j,Pgv_j)
      do nd = 1, nvector
!
        do inum = 1, nnod_rlm
          k_rtm = 1 + mod( (inum-1),nidx_rlm(1))
          j_rlm = 1 + (inum - k_rtm) / nidx_rlm(1)
!
          i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
          sp_rlm_fdout(i_rlm,3*nd-2) = sp_rlm_fdout(i_rlm,3*nd-2)       &
     &                       * a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)
          sp_rlm_fdout(i_rlm,3*nd-1) = sp_rlm_fdout(i_rlm,3*nd-1)       &
     &                       * a_r_1d_rlm_r(k_rtm)
          sp_rlm_fdout(i_rlm,3*nd  ) = sp_rlm_fdout(i_rlm,3*nd  )       &
     &                       * a_r_1d_rlm_r(k_rtm)
        end do
!
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
              ip_rtm = l_rtm + (k_rtm-1) * nidx_rtm(2)                  &
     &                       + (mp_rlm-1) * nidx_rtm(1)*nidx_rtm(2)
              vr1 = 0.0d0
              vr2 = 0.0d0
              vr3 = 0.0d0
              do j_rlm = jst, jed
                i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
                vr1 = vr1 + sp_rlm_fdout(i_rlm,3*nd-2) * Pg3_j(j_rlm)
                vr2 = vr2 + sp_rlm_fdout(i_rlm,3*nd-1) * dPdt_j(j_rlm)
                vr3 = vr3 - sp_rlm_fdout(i_rlm,3*nd  ) * dPdt_j(j_rlm)
              end do
              vr_rtm_fdout(ip_rtm,3*nd-2) = vr1
              vr_rtm_fdout(ip_rtm,3*nd-1) = vr2
              vr_rtm_fdout(ip_rtm,3*nd  ) = vr3
            end do
          end do
        end do
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst = lstack_rlm(mp_rlm-1) + 1
          jed = lstack_rlm(mp_rlm)
          do k_rtm = 1,  nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              do j_rlm = jst, jed
                Pgv_j(j_rlm) = -P_jl(j_rlm,l_rtm)                       &
     &                      * dble(idx_gl_1d_rlm_j(j_rlm,3))            &
     &                       *asin_theta_1d_rtm(l_rtm)
              end do
!
              in_rtm = l_rtm + (k_rtm-1) * nidx_rtm(2)                  &
     &                       + (mn_rlm-1) * nidx_rtm(1)*nidx_rtm(2)
!
              vr2 = 0.0d0
              vr3 = 0.0d0
              do j_rlm = jst, jed
                i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
                vr2 = vr2 + sp_rlm_fdout(i_rlm,3*nd  ) * Pgv_j(j_rlm)
                vr3 = vr3 + sp_rlm_fdout(i_rlm,3*nd-1) * Pgv_j(j_rlm)
              end do
              vr_rtm_fdout(in_rtm,3*nd-1) = vr_rtm_fdout(in_rtm,3*nd-1) &
      &                                    + vr2
              vr_rtm_fdout(in_rtm,3*nd  ) = vr_rtm_fdout(in_rtm,3*nd  ) &
      &                                    + vr3 
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_fdout(nscalar,                 &
     &          sp_rlm_fdout, vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_fdout(nnod_rlm,nscalar)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_fdout(nnod_rtm,nscalar)
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
      do nd = 1, nscalar
        do mp_rlm = 1, nidx_rtm(3)
          jst = lstack_rlm(mp_rlm-1) + 1
          jed = lstack_rlm(mp_rlm)
          do k_rtm = 1,  nidx_rtm(1)
!
            do l_rtm = 1, nidx_rtm(2)
              P_j(jst:jed) = P_jl(jst:jed,l_rtm)
!
              ip_rtm = l_rtm + (k_rtm-1) * nidx_rtm(2)                  &
     &                       + (mp_rlm-1) * nidx_rtm(1)*nidx_rtm(2)
              vr1 = 0.0d0
              do j_rlm = jst, jed
                i_rlm = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
                vr1 = vr1 + sp_rlm_fdout(i_rlm,nd) * P_j(j_rlm)
              end do
              vr_rtm_fdout(ip_rtm,nd) = vr1
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_fdout
