!>@file   const_wz_coriolis_rtp.f90
!!@brief  module const_wz_coriolis_rtp
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief  Evaluate Coriolis term on spherical grid
!!
!!@verbatim
!!      subroutine alloc_sphere_ave_coriolis
!!      subroutine dealloc_sphere_ave_coriolis
!!
!!      subroutine set_colatitude_rtp
!!      subroutine cal_wz_coriolis_pole                                 &
!!     &         (nnod_pole, coef_cor, velo_pole, coriolis_pole)
!!      subroutine cal_wz_coriolis_rtp(nnod, velo_rtp, coriolis_rtp)
!!      subroutine cal_wz_div_coriolis_rtp(nnod, velo_rtp,              &
!!     &          div_coriolis_rtp)
!!      subroutine subtract_sphere_ave_coriolis(nnod, drtp_coriolis)
!!
!!      subroutine ovwrt_rj_coef_prod_vect_smp(coef, i_r)
!!      subroutine clear_rj_degree0_scalar_smp(irj_fld)
!!      subroutine clear_rj_degree0_vector_smp(irj_fld)
!!@endverbatim
!!
!!@n @param irj_fld   Address for spectr data
!
      module const_wz_coriolis_rtp
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_spheric_parameter
!
      implicit none
!
!>      @f$ \theta @f$ on spherical grid
      real(kind = kreal), allocatable :: theta_1d_rtp(:)
!
!>     sphere average of radial coriolis force (local)
      real(kind = kreal), allocatable :: sphere_ave_coriolis_l(:)
!>     sphere average of radial coriolis force
      real(kind = kreal), allocatable :: sphere_ave_coriolis_g(:)
!
      private :: sphere_ave_coriolis_l, sphere_ave_coriolis_g
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sphere_ave_coriolis
!
      integer(kind = kint) :: num
!
!
      num = nidx_rtp(2)
      allocate(theta_1d_rtp(num))
!
      num = nidx_rj(1)
      allocate(sphere_ave_coriolis_l(num))
      allocate(sphere_ave_coriolis_g(num))
!
      theta_1d_rtp = 0.0d0
      sphere_ave_coriolis_l = 0.0d0
      sphere_ave_coriolis_g = 0.0d0
!
      end subroutine alloc_sphere_ave_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sphere_ave_coriolis
!
      deallocate(sphere_ave_coriolis_l, sphere_ave_coriolis_g)
      deallocate(theta_1d_rtp)
!
      end subroutine dealloc_sphere_ave_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_colatitude_rtp
!
      use m_work_4_sph_trans
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint) :: l_rtp, l_rtm
!
!
      call alloc_sphere_ave_coriolis
!
      do l_rtp = 1, nidx_rtp(2)
        l_rtm = idx_gl_1d_rtp_t(l_rtp)
        theta_1d_rtp(l_rtp) = g_colat_rtm(l_rtm)
      end do
!
      end subroutine set_colatitude_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_wz_coriolis_pole                                   &
     &         (nnod_pole, velo_pole, coriolis_pole)
!
      use m_sph_phys_address
      use m_physical_property
!
      integer(kind = kint), intent(in) :: nnod_pole
      real(kind = kreal), intent(in) :: velo_pole(nnod_pole,3)
!
      real(kind = kreal), intent(inout) :: coriolis_pole(nnod_pole,3)
!
      integer(kind = kint) :: inod
      real(kind = kreal), parameter :: omega(3) = (/zero, zero, one/)
!
!
!$omp parallel do private(inod)
      do inod = 1, nnod_pole
            coriolis_pole(inod,1) = - coef_cor                          &
!     &                         * ( omega(2)*velo_pole(inod,3)          &
     &                         * ( -omega(3)*velo_pole(inod,2) )
            coriolis_pole(inod,2) = - coef_cor                          &
     &                         * ( omega(3)*velo_pole(inod,1) )
!     &                           - omega(1)*velo_pole(inod,3) )
            coriolis_pole(inod,3) = zero
!            coriolis_pole(inod,3) = - coef_cor                         &
!     &                         * ( omega(1)*velo_pole(inod,2)          &
!     &                           - omega(2)*velo_pole(inod,1) )
      end do
!$omp end parallel do
!
      end subroutine cal_wz_coriolis_pole
!
! -----------------------------------------------------------------------
!
      subroutine cal_wz_coriolis_rtp(nnod, velo_rtp, coriolis_rtp)
!
      use m_sph_phys_address
      use m_physical_property
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: velo_rtp(nnod,3)
      real(kind = kreal), intent(inout) :: coriolis_rtp(nnod,3)
!
      integer(kind = kint) :: mphi, l_rtp, kr, inod
      real(kind = kreal) :: omega(3)
!
!
!$omp do private(mphi,l_rtp,kr,inod,omega)
      do mphi = 1, nidx_rtp(3)
        do l_rtp = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (l_rtp-1) * nidx_rtp(1)                         &
     &                + (mphi-1)  * nidx_rtp(1)*nidx_rtp(2)
!
            omega(1) =  cos( theta_1d_rtp(l_rtp) )
            omega(2) = -sin( theta_1d_rtp(l_rtp) )
            omega(3) = zero
!
            coriolis_rtp(inod,1) = - coef_cor                           &
     &                         * ( omega(2)*velo_rtp(inod,3)            &
     &                           - omega(3)*velo_rtp(inod,2) )
            coriolis_rtp(inod,2) = - coef_cor                           &
     &                         * ( omega(3)*velo_rtp(inod,1)            &
     &                           - omega(1)*velo_rtp(inod,3) )
            coriolis_rtp(inod,3) = - coef_cor                           &
     &                         * ( omega(1)*velo_rtp(inod,2)            &
     &                           - omega(2)*velo_rtp(inod,1) )
          end do
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_wz_coriolis_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_wz_div_coriolis_rtp(nnod, velo_rtp,                &
     &          div_coriolis_rtp)
!
      use m_physical_property
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: velo_rtp(nnod,3)
      real(kind = kreal), intent(inout) :: div_coriolis_rtp(nnod)
!
      integer(kind = kint) :: mphi, l_rtp, kr, inod
      real(kind = kreal) :: omega(3)
!
!
!$omp do private(mphi,l_rtp,kr,inod,omega)
      do mphi = 1, nidx_rtp(3)
        do l_rtp = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (l_rtp-1) * nidx_rtp(1)                         &
     &                + (mphi-1)  * nidx_rtp(1)*nidx_rtp(2)
!
            omega(1) =  cos( theta_1d_rtp(l_rtp) )
            omega(2) = -sin( theta_1d_rtp(l_rtp) )
            omega(3) = zero
!
            div_coriolis_rtp(inod)                                      &
     &          = coef_cor * ( omega(1)*velo_rtp(inod,1)                &
     &                       - omega(2)*velo_rtp(inod,2) )
          end do
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_wz_div_coriolis_rtp
!
! -----------------------------------------------------------------------
!
      subroutine subtract_sphere_ave_coriolis(nnod, coriolis_rtp)
!
      use calypso_mpi
      use m_sph_phys_address
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: coriolis_rtp(nnod,3)
!
      integer(kind = kint) :: mphi, l_rtp, kr, k_gl, inod
!
!
      if(idx_rj_degree_zero .gt. 0) then
        do kr = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (kr-1)*nidx_rj(2)
          sphere_ave_coriolis_l(kr) = d_rj(inod,ipol%i_coriolis)
        end do
      else
          sphere_ave_coriolis_l(1:nidx_rj(1)) = zero
      end if
      call clear_rj_degree0_scalar_smp(ipol%i_coriolis)
!
      call MPI_Allreduce(sphere_ave_coriolis_l, sphere_ave_coriolis_g,  &
     &    nidx_rj(1), CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
!$omp do private(mphi,l_rtp,kr,k_gl,inod)
      do mphi = 1, nidx_rtp(3)
        do l_rtp = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            k_gl = idx_gl_1d_rtp_r(kr)
            inod = kr + (l_rtp-1) * nidx_rtp(1)                         &
     &                + (mphi-1)  * nidx_rtp(1)*nidx_rtp(2)
!
            coriolis_rtp(inod,1) = coriolis_rtp(inod,1)                 &
     &                             - sphere_ave_coriolis_g(k_gl)
          end do
        end do
      end do
!$omp end do nowait
!
      end subroutine subtract_sphere_ave_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ovwrt_rj_coef_prod_vect_smp(coef, i_r)
!
      use m_spheric_param_smp
      use m_sph_spectr_data
      use overwrite_prod_const_smp
!
      real(kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
!
!
      call ovwrt_coef_prod_vect_smp(np_smp, nnod_rj,                    &
     &   inod_rj_smp_stack, coef, d_rj(1,i_r) )
!
      end subroutine ovwrt_rj_coef_prod_vect_smp
!
!-----------------------------------------------------------------------
!
      subroutine clear_rj_degree0_scalar_smp(irj_fld)
!
      use m_sph_spectr_data
!
      integer (kind = kint), intent(in) :: irj_fld
!
      integer(kind = kint) :: k, inod
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(inod)
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,irj_fld) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_rj_degree0_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine clear_rj_degree0_vector_smp(irj_fld)
!
      use m_sph_spectr_data
!
      integer (kind = kint), intent(in) :: irj_fld
!
      integer(kind = kint) :: k, inod
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(inod)
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,irj_fld  ) = zero
        d_rj(inod,irj_fld+1) = zero
        d_rj(inod,irj_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_rj_degree0_vector_smp
!
! ----------------------------------------------------------------------
!
      end module const_wz_coriolis_rtp
