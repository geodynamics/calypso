!>@file   cal_buoyancies_sph_MHD.f90
!!@brief  module cal_buoyancies_sph_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine cal_boussinesq_density_sph                           &
!!     &         (ipol, kr_in, kr_out, coef_buo, coef_comp_buo,         &
!!     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
!!        real(kind = kreal) , intent(in) :: coef_buo, coef_comp_buo
!!        real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!!
!!      subroutine rot_self_dbl_buoyancy_sph_MHD(kr_in, kr_out,         &
!!     &          coef_t_buo, is_t, coef_c_buo, is_c, is_buo,           &
!!     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine rot_r_cst_dbl_buoyancy_sph_MHD(kr_in, kr_out,        &
!!     &          coef_t_buo, is_t, coef_c_buo, is_c, is_buo,           &
!!     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind= kint), intent(in) :: is_t, is_c
!!        integer(kind= kint), intent(in) :: is_buo
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
!!        real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!!      subroutine rot_self_buoyancy_sph_MHD(kr_in, kr_out, coef,       &
!!     &          is_fld, is_buo, nidx_rj, radius_1d_rj_r,              &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine rot_r_const_buoyancy_sph_MHD(kr_in, kr_out, coef,    &
!!     &          is_fld, is_buo, nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind= kint), intent(in) :: is_fld, is_buo
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: coef
!!        real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!!      subroutine cal_self_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj,   &
!!     &          coef, is_fld, is_buo, nidx_rj, radius_1d_rj_r,        &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_radial_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj, &
!!     &          coef, is_fld, is_buo, nidx_rj, radius_1d_rj_r,        &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind= kint), intent(in) :: is_fld, is_buo
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: coef
!!        real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),17)
!!        real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!!@endverbatim
!!
      module cal_buoyancies_sph_MHD
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_boussinesq_density_sph                             &
     &         (ipol, kr_in, kr_out, coef_buo, coef_comp_buo,           &
     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!
      use t_phys_address
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal) , intent(in) :: coef_buo, coef_comp_buo
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,ipol%base%i_density)                                  &
     &        = -(d_rj(inod,ipol%base%i_temp)                           &
     &         + coef_comp_buo/coef_buo * d_rj(inod,ipol%base%i_light))
      end do
!$omp end parallel do
!
      end subroutine cal_boussinesq_density_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine rot_self_dbl_buoyancy_sph_MHD(kr_in, kr_out,           &
     &          coef_t_buo, is_t, coef_c_buo, is_c, is_buo,             &
     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, is_c
      integer(kind= kint), intent(in) :: is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,is_buo  ) = 0.0d0
        d_rj(inod,is_buo+1) = 0.0d0
        d_rj(inod,is_buo+2) = (coef_t_buo * d_rj(inod,is_t)             &
     &                       + coef_c_buo * d_rj(inod,is_c)  )          &
     &                      * radius_1d_rj_r(k)
      end do
!$omp end parallel do
!
      end subroutine rot_self_dbl_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine rot_r_cst_dbl_buoyancy_sph_MHD(kr_in, kr_out,          &
     &          coef_t_buo, is_t, coef_c_buo, is_c, is_buo,             &
     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, is_c
      integer(kind= kint), intent(in) :: is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,is_buo  ) = 0.0d0
        d_rj(inod,is_buo+1) = 0.0d0
        d_rj(inod,is_buo+2) = coef_t_buo * d_rj(inod,is_t)              &
     &                       + coef_c_buo * d_rj(inod,is_c)
      end do
!$omp end parallel do
!
      end subroutine rot_r_cst_dbl_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine rot_self_buoyancy_sph_MHD(kr_in, kr_out, coef,         &
     &          is_fld, is_buo, nidx_rj, radius_1d_rj_r,                &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,is_buo  ) = 0.0d0
        d_rj(inod,is_buo+1) = 0.0d0
        d_rj(inod,is_buo+2)                                             &
     &          =  coef * d_rj(inod,is_fld) * radius_1d_rj_r(k)
      end do
!$omp end parallel do
!
      end subroutine rot_self_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine rot_r_const_buoyancy_sph_MHD(kr_in, kr_out, coef,      &
     &          is_fld, is_buo, nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,is_buo  ) = 0.0d0
        d_rj(inod,is_buo+1) = 0.0d0
        d_rj(inod,is_buo+2) = coef * d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine rot_r_const_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_self_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj,     &
     &          coef, is_fld, is_buo, nidx_rj, radius_1d_rj_r,          &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),17)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,is_buo) = coef * g_sph_rj(j,13)                       &
     &                     * d_rj(inod,is_fld) * radius_1d_rj_r(k)**3
        d_rj(inod,is_buo+1) = 0.0d0
        d_rj(inod,is_buo+2) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine cal_self_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_radial_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj,   &
     &          coef, is_fld, is_buo, nidx_rj, radius_1d_rj_r,          &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, is_buo
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),17)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,is_buo) = coef * g_sph_rj(j,13)                       &
     &                     * d_rj(inod,is_fld) * radius_1d_rj_r(k)**2
        d_rj(inod,is_buo+1) = 0.0d0
        d_rj(inod,is_buo+2) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine cal_radial_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_buoyancies_sph_MHD
