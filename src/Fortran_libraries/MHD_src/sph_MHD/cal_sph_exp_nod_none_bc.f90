!>@file   cal_sph_exp_nod_none_bc.f90
!!@brief  module cal_sph_exp_nod_none_bc
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate derivatives with no boundary conditions
!!
!!@verbatim
!!      subroutine cal_sph_nod_nobc_in_rot2(coef_fdm_fix_in_2, kr_in,   &
!!     &          is_fld, is_rot)
!!      subroutine cal_sph_nod_nobc_in_div2(coef_fdm_fix_in_2, kr_in,   &
!!     &          is_fld, is_rot)
!!      subroutine cal_sph_nod_nobc_in_diffuse2(coef_fdm_fix_in_2,      &
!!     &          kr_in,  is_fld, is_diffuse)
!!
!!      subroutine cal_sph_nod_nobc_out_rot2(coef_fdm_fix_out_2, kr_out,&
!!     &          is_fld, is_rot)
!!      subroutine cal_sph_nod_nobc_out_div2(coef_fdm_fix_out_2, kr_out,&
!!     &          is_fld, is_rot)
!!      subroutine cal_sph_nod_nobc_out_diffuse2(coef_fdm_fix_out_2,    &
!!     &          kr_out, is_fld, is_diffuse)
!!
!!      subroutine delete_bc_rj_vector(kr_bc, is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param coef_fdm_fix_in_2(0:2,3)
!!            Finite difference matrix for ICB with no boundary condition
!!@n @param coef_fdm_fix_out_2(0:2,3)
!!            Finite difference matrix for CMB with no boundary condition
!!
!!@n @param kr_in   radial ID for inner boundary
!!@n @param kr_out  radial ID for outer boundary
!!@n @param kr_bc   radial ID to be deleted
!!
!!@n @param is_fld       Field address of input field
!!@n @param is_rot       Field address for curl of field
!!@n @param is_diffuse   Field address for diffusion of field
!
      module cal_sph_exp_nod_none_bc
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_rot2(coef_fdm_fix_in_2, kr_in,     &
     &          is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_in_2( 0,3) * d_rj(inod,is_fld  )        &
     &           + coef_fdm_fix_in_2( 1,3) * d_rj(i_p1,is_fld  )        &
     &           + coef_fdm_fix_in_2( 2,3) * d_rj(i_p2,is_fld  )
        d1t_dr1 =  coef_fdm_fix_in_2( 0,2) * d_rj(inod,is_fld+2)        &
     &           + coef_fdm_fix_in_2( 1,2) * d_rj(i_p1,is_fld+2)        &
     &           + coef_fdm_fix_in_2( 2,2) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2                               &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(inod,is_fld  ))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_div2(coef_fdm_fix_in_2, kr_in,     &
     &          is_fld, is_div)
!
      integer(kind = kint), intent(in) :: is_fld, is_div
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d1s_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
!
        d1s_dr1 =  coef_fdm_fix_in_2( 0,2) * d_rj(inod,is_fld  )        &
     &           + coef_fdm_fix_in_2( 1,2) * d_rj(i_p1,is_fld  )        &
     &           + coef_fdm_fix_in_2( 2,2) * d_rj(i_p2,is_fld  )
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                     * max(g_sph_rj(j,3),half)*ar_1d_rj(kr_in,2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_div2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_diffuse2(coef_fdm_fix_in_2,        &
     &          kr_in,  is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2,d2t_dr2
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_in_2( 0,3) * d_rj(inod,is_fld  )        &
     &           + coef_fdm_fix_in_2( 1,3) * d_rj(i_p1,is_fld  )        &
     &           + coef_fdm_fix_in_2( 2,3) * d_rj(i_p2,is_fld  )
        d2t_dr2 =  coef_fdm_fix_in_2( 0,3) * d_rj(inod,is_fld+2)        &
     &           + coef_fdm_fix_in_2( 1,3) * d_rj(i_p1,is_fld+2)        &
     &           + coef_fdm_fix_in_2( 2,3) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =   d2s_dr2                             &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(inod,is_fld  )
        d_rj(inod,is_diffuse+2) =   d2t_dr2                             &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(inod,is_fld+2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_rot2(coef_fdm_fix_out_2, kr_out,  &
     &          is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (kr_out-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_out_2(2,3) * d_rj(i_n2,is_fld  )        &
     &           + coef_fdm_fix_out_2(1,3) * d_rj(i_n1,is_fld  )        &
     &           + coef_fdm_fix_out_2(0,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_out_2(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2                               &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_out,2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_div2(coef_fdm_fix_out_2, kr_out,  &
     &          is_fld, is_div)
!
      integer(kind = kint), intent(in) :: is_fld, is_div
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d1s_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (kr_out-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d1s_dr1 =  coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_out_2(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                  * max(g_sph_rj(j,3),half) * ar_1d_rj(kr_out,2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_div2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_diffuse2(coef_fdm_fix_out_2,      &
     &          kr_out, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j + (kr_out-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_out_2(2,3) * d_rj(i_n2,is_fld  )        &
     &           + coef_fdm_fix_out_2(1,3) * d_rj(i_n1,is_fld  )        &
     &           + coef_fdm_fix_out_2(0,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  coef_fdm_fix_out_2(2,3) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_out_2(1,3) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_out_2(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  d2s_dr2                              &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_out,2)*d_rj(inod,is_fld  )
        d_rj(inod,is_diffuse+2) =  d2t_dr2                              &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_out,2)*d_rj(inod,is_fld+2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine delete_bc_rj_vector(kr_bc, is_fld)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: kr_bc
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, nidx_rj(2)
        inod = j + (kr_bc-1) * nidx_rj(2)
!
        d_rj(inod,is_fld  ) = 0.0d0
        d_rj(inod,is_fld+1) = 0.0d0
        d_rj(inod,is_fld+2) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine delete_bc_rj_vector
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_nod_none_bc
