!>@file   schmidt_poly_on_rtm_grid.f90
!!@brief  module schmidt_poly_on_rtm_grid
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Copy Legendre polynomials for spherical transform
!!
!!
!!@verbatim
!!      subroutine s_cal_schmidt_poly_rtm
!!
!!      subroutine copy_sph_normalization_2_rlm
!!      subroutine copy_sph_normalization_2_rj
!!@endverbatim
!
      module schmidt_poly_on_rtm_grid
!
      use m_precision
!
      implicit none
!
      private :: set_gauss_points_rtm
      private :: set_lagender_4_rlm, set_lagender_pole_rlm
      private :: copy_sph_normalization_2_rlm
      private :: copy_sph_normalization_2_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_schmidt_poly_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_gauss_points
!
!
      call allocate_gauss_points(nidx_rtm(2))
      call allocate_gauss_colatitude
      call allocate_gauss_colat_rtm
!
      call set_gauss_points_rtm
!
      call deallocate_gauss_colatitude
      call deallocate_gauss_points
!
!     set Legendre polynomials
!
      call allocate_schmidt_poly_rtm
!
      call copy_sph_normalization_2_rlm
      call copy_sph_normalization_2_rj
!
      call set_lagender_4_rlm
!
      call allocate_schmidt_p_rtm_pole
      call set_lagender_pole_rlm
!
      end subroutine s_cal_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gauss_points_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_gauss_points
!
      integer(kind = kint) :: i
!
!
!     set gauss colatitudes
!
      call construct_gauss_coefs
!
      call set_gauss_colatitude
!
      do i = 1, n_point
        g_point_rtm(i) = w_point(i)
        g_colat_rtm(i) = w_colat(i)
        weight_rtm(i) =  w_coefs(i)
      end do
!
      end subroutine set_gauss_points_rtm
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_normalization_2_rlm
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use spherical_harmonics
!
      integer(kind = kint) :: j, ll, mm
      integer(kind = kint) :: idx_lm(2)
      real(kind = kreal) :: g_lm(17)
!
!$omp parallel do private(j,ll,mm,idx_lm,g_lm)
      do j = 1, nidx_rlm(2)
        ll = idx_gl_1d_rlm_j(j,2)
        mm = idx_gl_1d_rlm_j(j,3)
!
        call sph_normalizations(ll, mm, idx_lm, g_lm)
        g_sph_rlm(j,1:17) =  g_lm(1:17)
!
        if(ll.eq.0 .and. mm.eq.0) g_sph_rlm(j,3) = half
      end do
!$omp end parallel do 
!
      end subroutine copy_sph_normalization_2_rlm
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_normalization_2_rj
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use spherical_harmonics
!
      integer(kind = kint) :: j, ll, mm
      integer(kind = kint) :: idx_lm(2)
      real(kind = kreal) :: g_lm(17)
!
!
!$omp parallel do private(j,ll,mm,idx_lm,g_lm)
      do j = 1, nidx_rj(2)
        ll = idx_gl_1d_rj_j(j,2)
        mm = idx_gl_1d_rj_j(j,3)
!
        call sph_normalizations(ll, mm, idx_lm, g_lm)
        g_sph_rj(j,1:13) =  g_lm(1:13)
      end do
!$omp end parallel do 
!
      end subroutine copy_sph_normalization_2_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_lagender_4_rlm
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use schmidt_fix_m
      use m_work_4_sph_trans
!
      integer(kind = kint) :: ip, i, j, l, m, mm, jj
      integer(kind = kint) :: jst, jed, lst, led
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
!
!
!$omp parallel do                                                       &
!$omp& private(i,j,l,m,mm,jj,jst,jed,lst,led,p_m,dp_m,pmn1,pmp1,df_m)
      do ip = 1, np_smp
        lst = idx_rtm_smp_stack(ip-1,2) + 1
        led = idx_rtm_smp_stack(ip,  2)
        do i = lst, led
!
          do m = 1, nidx_rtm(3)
            mm = abs(idx_gl_1d_rtm_m(m,2))
            jst = lstack_rlm(m-1) + 1
            jed = lstack_rlm(m)
!
            call schmidt_legendres_m(l_truncation, mm, g_colat_rtm(i),  &
     &          p_m, dp_m, pmn1, pmp1, df_m)
!
            do j = jst, jed
              jj = idx_gl_1d_rlm_j(j,1)
              l =  idx_gl_1d_rlm_j(j,2)
              P_rtm(i,j) =    p_m(l)
              dPdt_rtm(i,j) = dp_m(l)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine set_lagender_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_lagender_pole_rlm
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use schmidt_fix_m
!
      integer(kind = kint) :: j, l, m, mm, jj, jst, jed
      real(kind = kreal) :: pi
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
!
!
      do m = 1, nidx_rtm(3)
        mm = abs(idx_gl_1d_rtm_m(m,2))
        jst = lstack_rlm(m-1) + 1
        jed = lstack_rlm(m)
!
        call schmidt_legendres_m(l_truncation, mm, zero,                &
     &          p_m, dp_m, pmn1, pmp1, df_m)
!
        do j = jst, jed
          jj = idx_gl_1d_rlm_j(j,1)
          l =  idx_gl_1d_rlm_j(j,2)
          P_pole_rtm(1,j) =    p_m(l)
          dPdt_pole_rtm(1,j) = dp_m(l)
        end do
      end do
!
      pi = four * atan(one)
      do m = 1, nidx_rtm(3)
        mm = abs(idx_gl_1d_rtm_m(m,2))
        jst = lstack_rlm(m-1) + 1
        jed = lstack_rlm(m)
!
        call schmidt_legendres_m(l_truncation, mm, pi,                  &
     &          p_m, dp_m, pmn1, pmp1, df_m)
!
        do j = jst, jed
          jj = idx_gl_1d_rlm_j(j,1)
          l =  idx_gl_1d_rlm_j(j,2)
          P_pole_rtm(2,j) =    p_m(l)
          dPdt_pole_rtm(2,j) = dp_m(l)
        end do
      end do
!
      end subroutine set_lagender_pole_rlm
!
! -----------------------------------------------------------------------
!
      end module schmidt_poly_on_rtm_grid
