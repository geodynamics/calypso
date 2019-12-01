!>@file   set_sph_exp_fix_vector_ICB.f90
!!@brief  module set_sph_exp_fix_vector_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at ICB
!!
!!@verbatim
!!      subroutine cal_sph_nod_icb_rigid_vect                           &
!!     &         (nidx_rj, idx_rj, radius_rj, kr_in, r_ICB,             &
!!     &          Vp_ICB, Vd_ICB, Vt_ICB, is_fld,                       &
!!     &          n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine cal_sph_nod_icb_fixed_rot2(jmax, g_sph_rj,           &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,      &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_icb_fixed_diffuse2(jmax, g_sph_rj,       &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,      &
!!     &          coef_d, is_fld, is_diffuse, n_point, ntot_phys_rj,    &
!!     &          d_rj)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param idx_rj_degree_one(-1:1)    Local address for degree 1
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param Vt_ICB(jmax) Spectr data for toroidal velocity ICB
!!
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!         Matrix to evaluate radial derivative at ICB with fixed field
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_sph_exp_fix_vector_ICB
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
      subroutine cal_sph_nod_icb_rigid_vect                             &
     &         (nidx_rj, idx_rj, radius_rj, kr_in, r_ICB,               &
     &          Vp_ICB, Vd_ICB, Vt_ICB, is_fld,                         &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_rj(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: radius_rj(nidx_rj(1))
      real(kind = kreal), intent(in) :: Vp_ICB(nidx_rj(2))
      real(kind = kreal), intent(in) :: Vd_ICB(nidx_rj(2))
      real(kind = kreal), intent(in) :: Vt_ICB(nidx_rj(2))
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, k, l
!
!
!$omp parallel do private(k,j,l,inod)
      do j = 1, nidx_rj(2)
        l = idx_rj(j,2)
        do k = 1, kr_in
          inod = j + (k-1) * nidx_rj(2)
!
          d_rj(inod,is_fld  ) = Vp_ICB(j)                               &
    &          * (radius_rj(k)*r_ICB(1))**(l+1)
          d_rj(inod,is_fld+1) = Vd_ICB(j) * dble(l+1)                   &
    &          * (radius_rj(k)*r_ICB(1))**(l)
          d_rj(inod,is_fld+2) = Vt_ICB(j)                               &
    &          * (radius_rj(k)*r_ICB(1))**(l+1)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_rigid_vect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_fixed_rot2(jmax, g_sph_rj,             &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,        &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1, i_p2
!
!
!$omp parallel do private(inod,i_p1,i_p2,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_dr_ICB(-1,3) * d_rj(inod,is_fld+1)          &
     &           + fdm2_fix_dr_ICB( 0,3) * d_rj(inod,is_fld  )          &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj(inod,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj(i_p1,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - (d2s_dr2                                &
     &           - g_sph_rj(j,3)*r_ICB(2)*d_rj(inod,is_fld  ))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_fixed_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_fixed_diffuse2(jmax, g_sph_rj,         &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,        &
     &          coef_d, is_fld, is_diffuse, n_point, ntot_phys_rj,      &
     &          d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2,d2t_dr2
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_dr_ICB(-1,3) * d_rj(inod,is_fld+1)          &
     &           + fdm2_fix_dr_ICB( 0,3) * d_rj(inod,is_fld  )          &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj(inod,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj(i_p1,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_diffuse  ) = coef_d * (d2s_dr2                     &
     &              - g_sph_rj(j,3)*r_ICB(2) * d_rj(inod,is_fld  ))
        d_rj(inod,is_diffuse+2) = coef_d * (d2t_dr2                     &
     &              - g_sph_rj(j,3)*r_ICB(2) * d_rj(inod,is_fld+2))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_fixed_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_fix_vector_ICB
