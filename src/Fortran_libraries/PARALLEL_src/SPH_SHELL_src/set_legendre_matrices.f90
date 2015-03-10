!>@file   set_legendre_matrices.f90
!!@brief  module set_legendre_matrices
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  set legendre polynomials into matrices
!!
!!@verbatim
!!      subroutine set_legendre_hemispher_rtm
!!      subroutine set_trans_legendre_rtm
!!      subroutine cal_legendre_trans_coefs
!!@endverbatim
!
      module set_legendre_matrices
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_legendre_hemispher_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtm, j_rlm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, n_jk_e, n_jk_o, jj
!
!
      do mp_rlm = 1, nidx_rtm(3)
        jst = lstack_rlm(mp_rlm-1)
        nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        lstack_even_rlm(mp_rlm) = jst + (nj_rlm+1) / 2
      end do
!
!$omp parallel do private(jst,nj_rlm,j_rlm,l_rtm,jj,n_jk_e,n_jk_o)
      do mp_rlm = 1, nidx_rtm(3)
        jst = lstack_rlm(mp_rlm-1)
        nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        n_jk_e = (nj_rlm+1) / 2
        n_jk_o =  nj_rlm - n_jk_e
        lstack_even_rlm(mp_rlm) = jst + n_jk_e
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          do l_rtm = 1, nth_hemi_rtm
            Ps_rtm(l_rtm,jj+jst) =     P_rtm(l_rtm,j_rlm)
            dPsdt_rtm(l_rtm,jj+jst) =  dPdt_rtm(l_rtm,j_rlm)
!
            Ps_jl(jj+jst,l_rtm) =     P_rtm(l_rtm,j_rlm)
            dPsdt_jl(jj+jst,l_rtm) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          do l_rtm = 1, nth_hemi_rtm
            Ps_rtm(l_rtm,jj+jst+n_jk_e) =     P_rtm(l_rtm,j_rlm)
            dPsdt_rtm(l_rtm,jj+jst+n_jk_e) =  dPdt_rtm(l_rtm,j_rlm)
!
            Ps_jl(jj+jst+n_jk_e,l_rtm) =     P_rtm(l_rtm,j_rlm)
            dPsdt_jl(jj+jst+n_jk_e,l_rtm) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_legendre_hemispher_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_trans_legendre_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtm, j_rlm
!
!
!$omp parallel do private(j_rlm,l_rtm)
      do j_rlm = 1, nidx_rlm(2)
        do l_rtm = 1, nidx_rtm(2)
          P_jl(j_rlm,l_rtm) =     P_rtm(l_rtm,j_rlm)
          dPdt_jl(j_rlm,l_rtm) =  dPdt_rtm(l_rtm,j_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_trans_legendre_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_legendre_trans_coefs
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtm, j_rlm
!
!
!
!$omp parallel do private(j_rlm,l_rtm)
      do j_rlm = 1, nidx_rlm(2)
        do l_rtm = 1, nidx_rtm(2)
          Pvw_lj(l_rtm,j_rlm) = P_rtm(l_rtm,j_rlm)                      &
     &        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
          dPvw_lj(l_rtm,j_rlm) = dPdt_rtm(l_rtm,j_rlm)                  &
     &        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
          Pgvw_lj(l_rtm,j_rlm) = P_rtm(l_rtm,j_rlm)                     &
     &        * dble(idx_gl_1d_rlm_j(j_rlm,3))*asin_theta_1d_rtm(l_rtm) &
     &        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
!
          Pws_lj(l_rtm,j_rlm) = P_rtm(l_rtm,j_rlm)                      &
     &        * g_sph_rlm(j_rlm,6)*weight_rtm(l_rtm)
!
!
          Pg3_lj(l_rtm,j_rlm) = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
          Pgv_lj(l_rtm,j_rlm) =-P_rtm(l_rtm,j_rlm)                      &
     &        * dble(idx_gl_1d_rlm_j(j_rlm,3))*asin_theta_1d_rtm(l_rtm)
!
!
          Pvw_jl(j_rlm,l_rtm) =  P_jl(j_rlm,l_rtm)                      &
     &        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
          dPvw_jl(j_rlm,l_rtm) = dPdt_jl(j_rlm,l_rtm)                   &
     &        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
          Pgvw_jl(j_rlm,l_rtm) = P_jl(j_rlm,l_rtm)                      &
     &        * dble(idx_gl_1d_rlm_j(j_rlm,3))*asin_theta_1d_rtm(l_rtm) &
     &        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
!
          Pws_jl(j_rlm,l_rtm) = P_jl(j_rlm,l_rtm)                       &
     &        * g_sph_rlm(j_rlm,6)*weight_rtm(l_rtm)
!
!
          Pg3_jl(j_rlm,l_rtm) = P_jl(j_rlm,l_rtm) * g_sph_rlm(j_rlm,3)
          Pgv_jl(j_rlm,l_rtm) = -P_jl(j_rlm,l_rtm)                      &
     &        * dble(idx_gl_1d_rlm_j(j_rlm,3))*asin_theta_1d_rtm(l_rtm)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_legendre_trans_coefs
!
! -----------------------------------------------------------------------
!
      end module set_legendre_matrices
