!>@file   cal_sph_exp_nod_none_bc.f90
!!@brief  module cal_sph_exp_nod_none_bc
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate derivatives with no boundary conditions
!!
!!@verbatim
!!      subroutine cal_sph_nod_nobc_in_grad2(jmax, kr_in, r_ICB,        &
!!     &          fdm2_fix_fld_ICB, is_fld, is_grad)
!!      subroutine cal_sph_nod_nobc_in_rot2(jmax, kr_in, r_ICB,         &
!!     &          fdm2_fix_fld_ICB, is_fld, is_rot)
!!      subroutine cal_sph_nod_nobc_in_div2(jmax, kr_in, r_ICB,         &
!!     &          fdm2_fix_fld_ICB, is_fld, is_div)
!!      subroutine cal_sph_nod_nobc_in_diffuse2(jmax, kr_in, r_ICB,     &
!!     &          fdm2_fix_fld_ICB, is_fld, is_diffuse)
!!
!!      subroutine cal_sph_nod_nobc_out_grad2(jmax, kr_out, r_CMB,      &
!!     &          fdm2_fix_fld_CMB, is_fld, is_grad)
!!      subroutine cal_sph_nod_nobc_out_rot2(jmax, kr_out, r_CMB,       &
!!     &          fdm2_fix_fld_CMB, is_fld, is_rot)
!!      subroutine cal_sph_nod_nobc_out_div2(jmax, kr_out, r_CMB,       &
!!     &          fdm2_fix_fld_CMB, is_fld, is_div)
!!      subroutine cal_sph_nod_nobc_out_diffuse2(jmax, kr_out, r_CMB,   &
!!     &          fdm2_fix_fld_CMB, is_fld, is_diffuse)
!!
!!      subroutine delete_bc_rj_vector(jmax, kr_bc, is_fld)
!!@endverbatim
!!
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!            Finite difference matrix for ICB with no boundary condition
!!@n @param fdm2_fix_fld_CMB(0:2,3)
!!            Finite difference matrix for CMB with no boundary condition
!!
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param kr_bc   radial ID to be deleted
!!
!!@n @param is_fld       Field address of input field
!!@n @param is_grad      Field address for gradient of field
!!@n @param is_div       Field address for divergence of field
!!@n @param is_rot       Field address for curl of field
!!@n @param is_diffuse   Field address for diffusion of field
!
      module cal_sph_exp_nod_none_bc
!
      use m_precision
!
      use m_constants
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
      subroutine cal_sph_nod_nobc_in_grad2(jmax, kr_in, r_ICB,          &
     &          fdm2_fix_fld_ICB, is_fld, is_grad)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_grad
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj(inod,is_fld)           &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj(i_p1,is_fld)           &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj(i_p2,is_fld)
!
        d_rj(inod,is_grad  ) = d1s_dr1 * g_sph_rj(j,13) * r_ICB(0)**2
        d_rj(inod,is_grad+1) = d_rj(inod,is_fld)
        d_rj(inod,is_grad+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_grad2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_rot2(jmax, kr_in, r_ICB,           &
     &          fdm2_fix_fld_ICB, is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj(inod,is_fld  )         &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj(i_p1,is_fld  )         &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj(i_p2,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj(inod,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj(i_p1,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2                               &
     &               - g_sph_rj(j,3)*r_ICB(2)*d_rj(inod,is_fld  ))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_div2(jmax, kr_in, r_ICB,           &
     &          fdm2_fix_fld_ICB, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj(inod,is_fld  )         &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj(i_p1,is_fld  )         &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj(i_p2,is_fld  )
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                     * max(g_sph_rj(j,3),half)*r_ICB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_div2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_diffuse2(jmax, kr_in, r_ICB,       &
     &          fdm2_fix_fld_ICB, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
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
        d2s_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj(inod,is_fld  )         &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj(i_p1,is_fld  )         &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj(i_p2,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj(inod,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj(i_p1,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =   d2s_dr2                             &
     &               - g_sph_rj(j,3)*r_ICB(2)*d_rj(inod,is_fld  )
        d_rj(inod,is_diffuse+2) =   d2t_dr2                             &
     &               - g_sph_rj(j,3)*r_ICB(2)*d_rj(inod,is_fld+2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_grad2(jmax, kr_out, r_CMB,        &
     &          fdm2_fix_fld_CMB, is_fld, is_grad)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_grad
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1s_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld)            &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld)            &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld)
!
        d_rj(inod,is_grad  ) = d1s_dr1 * g_sph_rj(j,13) * r_CMB(0)**2
        d_rj(inod,is_grad+1) = d_rj(inod,is_fld  )
        d_rj(inod,is_grad+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_grad2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_rot2(jmax, kr_out, r_CMB,         &
     &          fdm2_fix_fld_CMB, is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_fld_CMB(2,3) * d_rj(i_n2,is_fld  )          &
     &           + fdm2_fix_fld_CMB(1,3) * d_rj(i_n1,is_fld  )          &
     &           + fdm2_fix_fld_CMB(0,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2                               &
     &               - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_div2(jmax, kr_out, r_CMB,         &
     &          fdm2_fix_fld_CMB, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1s_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld  )          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld  )          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld  )
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                  * max(g_sph_rj(j,3),half) * r_CMB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_div2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_out_diffuse2(jmax, kr_out, r_CMB,     &
     &          fdm2_fix_fld_CMB, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_fld_CMB(2,3) * d_rj(i_n2,is_fld  )          &
     &           + fdm2_fix_fld_CMB(1,3) * d_rj(i_n1,is_fld  )          &
     &           + fdm2_fix_fld_CMB(0,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_CMB(2,3) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,3) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  d2s_dr2                              &
     &               - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld  )
        d_rj(inod,is_diffuse+2) =  d2t_dr2                              &
     &               - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld+2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_out_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine delete_bc_rj_vector(jmax, kr_bc, is_fld)
!
      integer(kind = kint), intent(in) :: jmax, kr_bc
      integer(kind = kint), intent(in) :: is_fld
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_bc-1) * jmax
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
