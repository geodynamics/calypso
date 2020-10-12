!>@file   t_const_wz_coriolis_rtp.f90
!!@brief  module t_const_wz_coriolis_rtp
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief  Evaluate Coriolis term on spherical grid
!!
!!@verbatim
!!      subroutine alloc_sphere_ave_coriolis(sph_rj, ave_cor)
!!      subroutine dealloc_sphere_ave_coriolis(ave_cor)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(spher_average_coriolis), intent(inout) :: ave_cor
!!
!!      subroutine subtract_sphere_ave_coriolis(sph_rtp, sph_rj,        &
!!     &          is_coriolis, ntot_phys_rj, d_rj, coriolis_rtp,        &
!!     &          ave_cor)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(spher_average_coriolis), intent(inout) :: ave_cor
!!
!!      subroutine ovwrt_rj_coef_prod_vect_smp                          &
!!     &         (sph_rj, coef, i_r, n_point, ntot_phys_rj, d_rj)
!!      subroutine clear_rj_degree0_vector_smp                          &
!!     &         (sph_rj, irj_fld, n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!@endverbatim
!!
!!@n @param irj_fld   Address for spectr data
!
      module t_const_wz_coriolis_rtp
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
!
      use t_spheric_rtp_data
      use t_spheric_rj_data
!
      implicit none
!
!>       Structure of sphere average of radial coriolis force
      type spher_average_coriolis
!>       sphere average of radial coriolis force (local)
        real(kind = kreal), allocatable :: save_cor_l(:)
!>       sphere average of radial coriolis force
        real(kind = kreal), allocatable :: save_cor_g(:)
      end type spher_average_coriolis
!
      private :: subtract_sph_ave_coriolis_prt
      private :: subtract_sph_ave_coriolis_rtp
      private :: clear_rj_degree0_scalar_smp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sphere_ave_coriolis(sph_rj, ave_cor)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(spher_average_coriolis), intent(inout) :: ave_cor
!
      integer(kind = kint) :: num
!
!
      num = sph_rj%nidx_rj(1)
      allocate(ave_cor%save_cor_l(num))
      allocate(ave_cor%save_cor_g(num))
!
      ave_cor%save_cor_l = 0.0d0
      ave_cor%save_cor_g = 0.0d0
!
      end subroutine alloc_sphere_ave_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sphere_ave_coriolis(ave_cor)
!
      type(spher_average_coriolis), intent(inout) :: ave_cor
!
      deallocate(ave_cor%save_cor_l, ave_cor%save_cor_g)
!
      end subroutine dealloc_sphere_ave_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine subtract_sphere_ave_coriolis(sph_rtp, sph_rj,          &
     &          is_coriolis, ntot_phys_rj, d_rj, coriolis_rtp,          &
     &          ave_cor)
!
      use calypso_mpi
      use calypso_mpi_real
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: is_coriolis
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout)                                 &
     &           :: d_rj(sph_rj%nnod_rj,ntot_phys_rj)
      real(kind = kreal), intent(inout)                                 &
     &           :: coriolis_rtp(sph_rtp%nnod_rtp,3)
      type(spher_average_coriolis), intent(inout) :: ave_cor
!
      integer(kind = kint) :: mphi, l_rtp, kr, k_gl, inod
      integer(kind = kint_gl) :: num64
!
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        do kr = 1, sph_rj%nidx_rj(1)
          inod = sph_rj%idx_rj_degree_zero + (kr-1)*sph_rj%nidx_rj(2)
          ave_cor%save_cor_l(kr) = d_rj(inod,is_coriolis)
        end do
      else
          ave_cor%save_cor_l(1:sph_rj%nidx_rj(1)) = zero
      end if
      call clear_rj_degree0_scalar_smp                                  &
     &   (sph_rj, is_coriolis, sph_rj%nnod_rj, ntot_phys_rj, d_rj)
!
      num64 = int(sph_rj%nidx_rj(1),KIND(num64))
      call calypso_mpi_allreduce_real                                   &
     &   (ave_cor%save_cor_l, ave_cor%save_cor_g, num64, MPI_SUM)
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call subtract_sph_ave_coriolis_prt                              &
     &         (sph_rtp, coriolis_rtp, ave_cor)
      else
        call subtract_sph_ave_coriolis_rtp                              &
     &         (sph_rtp, coriolis_rtp, ave_cor)
      end if
!
      end subroutine subtract_sphere_ave_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ovwrt_rj_coef_prod_vect_smp                            &
     &         (sph_rj, coef, i_r, n_point, ntot_phys_rj, d_rj)
!
      use overwrite_prod_const_smp
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
!
      integer (kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      call ovwrt_coef_prod_vect_smp(np_smp, n_point,                    &
     &   sph_rj%istack_inod_rj_smp, coef, d_rj(1,i_r) )
!
      end subroutine ovwrt_rj_coef_prod_vect_smp
!
!-----------------------------------------------------------------------
!
      subroutine clear_rj_degree0_scalar_smp                            &
     &         (sph_rj, irj_fld, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer (kind = kint), intent(in) :: irj_fld
      integer (kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(inod)
      do k = 1, sph_rj%nidx_rj(1)
        inod = sph_rj%idx_rj_degree_zero + (k-1) * sph_rj%nidx_rj(2)
        d_rj(inod,irj_fld) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_rj_degree0_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine clear_rj_degree0_vector_smp                            &
     &         (sph_rj, irj_fld, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer (kind = kint), intent(in) :: irj_fld
      integer (kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(inod)
      do k = 1, sph_rj%nidx_rj(1)
        inod = sph_rj%idx_rj_degree_zero + (k-1) * sph_rj%nidx_rj(2)
        d_rj(inod,irj_fld  ) = zero
        d_rj(inod,irj_fld+1) = zero
        d_rj(inod,irj_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_rj_degree0_vector_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine subtract_sph_ave_coriolis_rtp                          &
     &         (sph_rtp, coriolis_rtp, ave_cor)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      real(kind = kreal), intent(inout)                                 &
     &           :: coriolis_rtp(sph_rtp%nnod_rtp,3)
      type(spher_average_coriolis), intent(inout) :: ave_cor
!
      integer(kind = kint) :: mphi, l_rtp, kr, k_gl, inod
!
!
!$omp do private(mphi,l_rtp,kr,k_gl,inod)
      do mphi = 1, sph_rtp%nidx_rtp(3)
        do l_rtp = 1, sph_rtp%nidx_rtp(2)
          do kr = 1, sph_rtp%nidx_rtp(1)
            k_gl = sph_rtp%idx_gl_1d_rtp_r(kr)
            inod = kr + (l_rtp-1) * sph_rtp%istep_rtp(2)                &
     &                + (mphi-1)  * sph_rtp%istep_rtp(3)
!
            coriolis_rtp(inod,1) = coriolis_rtp(inod,1)                 &
     &                             - ave_cor%save_cor_g(k_gl)
          end do
        end do
      end do
!$omp end do nowait
!
      end subroutine subtract_sph_ave_coriolis_rtp
!
! -----------------------------------------------------------------------
!
      subroutine subtract_sph_ave_coriolis_prt                          &
     &         (sph_rtp, coriolis_rtp, ave_cor)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      real(kind = kreal), intent(inout)                                 &
     &           :: coriolis_rtp(sph_rtp%nnod_rtp,3)
      type(spher_average_coriolis), intent(inout) :: ave_cor
!
      integer(kind = kint) :: mphi, l_rtp, kr, k_gl, inod
!
!
!$omp do private(mphi,l_rtp,kr,k_gl,inod)
      do l_rtp = 1, sph_rtp%nidx_rtp(2)
        do kr = 1, sph_rtp%nidx_rtp(1)
          k_gl = sph_rtp%idx_gl_1d_rtp_r(kr)
          do mphi = 1, sph_rtp%nidx_rtp(3)
            inod = mphi + (kr-1) *    sph_rtp%istep_rtp(1)              &
     &                  + (l_rtp-1) * sph_rtp%istep_rtp(2)
!
            coriolis_rtp(inod,1) = coriolis_rtp(inod,1)                 &
     &                             - ave_cor%save_cor_g(k_gl)
          end do
        end do
      end do
!$omp end do nowait
!
      end subroutine subtract_sph_ave_coriolis_prt
!
! -----------------------------------------------------------------------
!
      end module t_const_wz_coriolis_rtp
