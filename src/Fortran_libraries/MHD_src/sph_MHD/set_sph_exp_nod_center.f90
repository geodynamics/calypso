!>@file   set_sph_exp_nod_center.f90
!!@brief  module set_sph_exp_nod_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate field approaching to center
!!
!!@verbatim
!!      subroutine sph_center_fld_and_curl(nri, jmax, a2r_k1, g_sph_rj, &
!!     &          d1nod_mat_fdm_2, d2nod_mat_fdm_2, is_fld, is_rot,     &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_dsdr_sph_center_2(nri, jmax, d1nod_mat_fdm_2,    &
!!     &          is_fld,  n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_center_rot2(nri, jmax, a2r_k1, g_sph_rj, &
!!     &          d1nod_mat_fdm_2, d2nod_mat_fdm_2, is_fld, is_rot,     &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_center_diffuse2 (nri, jmax, a2r_k1,      &
!!     &          g_sph_rj, d2nod_mat_fdm_2, coef_d, is_fld, is_diffuse,&
!!     &          n_point, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param coef_d    Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal magnetic field
!!                    (or velocity) in d_rj
!!@n @param is_rot     Address of poloidal currentdensity
!!                    (or vorticity) in d_rj
!!@n @param is_diffuse Address of poloidal magnetic diffusion
!!                    (or viscousity) in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_sph_exp_nod_center
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
      subroutine sph_center_fld_and_curl(nri, jmax, a2r_k1, g_sph_rj,   &
     &          d1nod_mat_fdm_2, d2nod_mat_fdm_2, is_fld, is_rot,       &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real (kind=kreal), intent(in) :: a2r_k1
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j
        i_p1 = inod + jmax
!
        d1s_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld)          &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld)
        d2s_dr2 =  d2nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld  )        &
     &           + d2nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld+2)        &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_fld+1) = d1s_dr1
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                       * a2r_k1 * d_rj(inod,is_fld) )
      end do
!$omp end parallel do
!
      end subroutine sph_center_fld_and_curl
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_center_2(nri, jmax, d1nod_mat_fdm_2,      &
     &          is_fld,  n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d1s_dr1)
      do inod = 1, jmax
        i_p1 = inod + jmax
        d1s_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld)          &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_fld+1) = d1s_dr1
      end do
!$omp end parallel do
!
      end subroutine cal_dsdr_sph_center_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_rot2(nri, jmax, a2r_k1, g_sph_rj,   &
     &          d1nod_mat_fdm_2, d2nod_mat_fdm_2, is_fld, is_rot,       &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real (kind=kreal), intent(in) :: a2r_k1
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j
        i_p1 = inod + jmax
!
        d2s_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld  )       &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld+2)       &
     &           + d1nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                           * a2r_k1 *d_rj(inod,is_fld) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_diffuse2 (nri, jmax, a2r_k1,        &
     &          g_sph_rj, d2nod_mat_fdm_2, coef_d, is_fld, is_diffuse,  &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real (kind=kreal), intent(in) :: a2r_k1
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d2s_dr2, d2t_dr2
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j
        i_p1 = inod + jmax
!
        d2s_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld  )       &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld  )
        d2t_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld+2)       &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_diffuse  ) = coef_d * (d2s_dr2                     &
     &         - g_sph_rj(j,3) * a2r_k1 * d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) = coef_d * (d2t_dr2                     &
     &         - g_sph_rj(j,3) * a2r_k1 * d_rj(inod,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_nod_center
