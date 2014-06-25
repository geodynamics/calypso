!>@file   init_sph_trans.f90
!!@brief  module init_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Initialize spherical harmonics transform
!!
!!@verbatim
!!      subroutine copy_sph_trans_nums_from_rtp
!!      subroutine initialize_sph_trans
!!@endverbatim
!
      module init_sph_trans
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: set_mdx_rlm_rtm, set_sin_theta_rtm, set_sin_theta_rtp
      private :: radial_4_sph_trans, cal_legendre_trans_coefs
      private :: set_trans_legendre_rtm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_trans_nums_from_rtp
!
      use m_machine_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
!
      ncomp_sph_trans =  num_tensor_rtp * n_sym_tensor                  &
     &                 + num_vector_rtp * n_vector                      &
     &                 + num_scalar_rtp * n_scalar
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans', ncomp_sph_trans
      end if
!
      end subroutine copy_sph_trans_nums_from_rtp
!
! -----------------------------------------------------------------------
!
      subroutine initialize_sph_trans
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_pole_sph_trans
      use schmidt_poly_on_rtm_grid
      use FFT_selector
      use spherical_SRs_N
      use select_fourier_transform
!
      integer(kind = kint) :: ncomp
      integer(kind = kint) :: Nstacksmp(0:np_smp)
!
!
      call allocate_work_4_sph_trans
      call allocate_wk_nod_data_to_sph
!
      call radial_4_sph_trans
      call set_mdx_rlm_rtm
!
      call s_cal_schmidt_poly_rtm
!
      call set_sin_theta_rtm
      call set_sin_theta_rtp
!
      call allocate_trans_schmidt_rtm
      call set_trans_legendre_rtm
!
!      call allocate_legendre_trans_mat
!      call cal_legendre_trans_coefs
!
!
      ncomp = ncomp_sph_trans*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp_sph_trans*irt_rtp_smp_stack(0:np_smp)
      call sel_fourier_transform_4_sph(ncomp, Nstacksmp)
!
      ncomp = ncomp_sph_trans
      call init_sph_send_recv_N(ncomp, vr_rtp, vr_rtm, sp_rlm, sp_rj)
!
      end subroutine initialize_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_mdx_rlm_rtm
!
      use calypso_mpi
      use m_spheric_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint) :: m, mm, j, mst, med
      integer(kind = kint), allocatable :: mdx_rlm_rtm(:)
      integer(kind = kint), allocatable :: mp_rlm(:), mn_rlm(:)
!
      allocate(mp_rlm( nidx_rlm(2)))
      allocate(mn_rlm( nidx_rlm(2)))
!
      allocate(mdx_rlm_rtm(-l_truncation:l_truncation))
      mdx_rlm_rtm(-l_truncation:l_truncation) = 0
!
      mdx_p_rlm_rtm(1:nidx_rlm(2)) = 0
      mdx_n_rlm_rtm(1:nidx_rlm(2)) = 0
      lstack_rlm(0) = 0
      do m = 1, nidx_rtm(3)
        mm = idx_gl_1d_rtm_m(m,2)
        mdx_rlm_rtm(mm) = m
        lstack_rlm(m) = lstack_rlm(m-1) + (l_truncation - abs(mm) + 1)
      end do
!
      do m = 1, nidx_rtm(3)
        mst = lstack_rlm(m-1)+1
        med = lstack_rlm(m)
        do j = mst, med
          mp_rlm(j) = m
          mn_rlm(j) = nidx_rtm(3) - m + 1
        end do
      end do
!
      do j = 1, nidx_rlm(2)
        m = idx_gl_1d_rlm_j(j,3)
        mdx_p_rlm_rtm(j) = mdx_rlm_rtm( m)
        mdx_n_rlm_rtm(j) = mdx_rlm_rtm(-m)
      end do
! 
!      write(50+my_rank,*) 'j, mdx_p_rlm_rtm(j), mp_rlm(j)'
!      do j = 1, nidx_rlm(2)
!        write(50+my_rank,*) j, mdx_p_rlm_rtm(j), mp_rlm(j)
!      end do
!
!      write(50+my_rank,*) 'j, mdx_n_rlm_rtm(j), mn_rlm(j)'
!      do j = 1, nidx_rlm(2)
!        write(50+my_rank,*) j, mdx_n_rlm_rtm(j), mn_rlm(j)
!      end do
!
!      write(50+my_rank,*) 'm, lstack_rlm(m)'
!      do m = 1, nidx_rtm(3)
!        write(50+my_rank,*) m, lstack_rlm(m)
!      end do
!
      deallocate(mdx_rlm_rtm, mp_rlm, mn_rlm)
!
      end subroutine set_mdx_rlm_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_sin_theta_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtm
!
!
      do l_rtm = 1, nidx_rtm(2)
        asin_theta_1d_rtm(l_rtm) = one / sin(g_colat_rtm(l_rtm))
      end do
!
      end subroutine set_sin_theta_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_sin_theta_rtp
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtp, l_gl
!
!
      do l_rtp = 1, nidx_rtp(2)
        l_gl = idx_gl_1d_rtp_t(l_rtp)
        cos_theta_1d_rtp(l_rtp) = cos(g_colat_rtm(l_gl))
        sin_theta_1d_rtp(l_rtp) = sin(g_colat_rtm(l_gl))
        cot_theta_1d_rtp(l_rtp) = cos_theta_1d_rtp(l_rtp)               &
     &                           / sin_theta_1d_rtp(l_rtp)
      end do
!
      end subroutine set_sin_theta_rtp
!
! -----------------------------------------------------------------------
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
! -----------------------------------------------------------------------
!
      subroutine radial_4_sph_trans
!
      use m_spheric_parameter
!
!
      a_r_1d_rtp_r(1:nidx_rtp(1)) = one/radius_1d_rtp_r(1:nidx_rtp(1))
      a_r_1d_rtm_r(1:nidx_rtm(1)) = one/radius_1d_rtm_r(1:nidx_rtm(1))
      a_r_1d_rlm_r(1:nidx_rlm(1)) = one/radius_1d_rlm_r(1:nidx_rlm(1))
      a_r_1d_rj_r(1:nidx_rj(1)) =   one/radius_1d_rj_r(1:nidx_rj(1))
!
      end subroutine radial_4_sph_trans
!
! -----------------------------------------------------------------------
!
      end module init_sph_trans
