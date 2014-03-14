!>@file   cal_sph_exp_rotation.f90
!!@brief  module cal_sph_exp_rotation
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate derivatives explicitly
!!
!!@verbatim
!!      subroutine cal_sph_diff_pol_and_rot2(kr_in, kr_out,             &
!!     &          is_fld, is_rot)
!!        input:  d_rj(:,is_fld),   d_rj(:,is_fld+2)
!!        output: d_rj(:,is_fld+1), d_rj(:,is_rot:is_rot+2)
!!
!!      subroutine cal_sph_diff_poloidal2(kr_in, kr_out, is_fld)
!!        input:  d_rj(:,is_fld)
!!        output: d_rj(:,is_fld+1)
!!
!!      subroutine cal_sph_nod_vect_rot2(kr_in, kr_out, is_fld, is_rot)
!!        input:  d_rj(:,is_fld),   d_rj(:,is_fld+2)
!!        output: d_rj(:,is_rot:is_rot+2)
!!
!!      subroutine cal_sph_nod_vect_w_div_rot2(kr_in, kr_out,           &
!!     &          is_fld, is_rot)
!!      subroutine cal_sph_nod_vect_div2(kr_in, kr_out, is_fld, is_div)
!!
!!      subroutine cal_sph_nod_diffuse_by_rot2(kr_in, kr_out, coef_d,   &
!!     &          is_fld, is_rot)
!!@endverbatim
!!
!!@n @param kr_in    Radial ID for inner boundary
!!@n @param kr_out   Radial ID for outer boundary
!!@n @param coef_d   Coefficient for diffusion term
!!
!!@n @param is_fld       Field address of input field
!!@n @param is_rot       Field address of curl of field
!!@n @param is_div       Field address of divergence of field
!!@n @param is_diffuse   Field address for diffusion of field
!
      module cal_sph_exp_rotation
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
      use m_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_diff_pol_and_rot2(kr_in, kr_out,               &
     &          is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d1s_dr1,d2s_dr2,d1t_dr1)
!cdir nodep
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod-j) / nidx_rj(2)
!
        d1s_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
        d2s_dr2 =  d2nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_fld+1) =   d1s_dr1
        d_rj(inod,is_rot  ) =   d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =   d1t_dr1
        d_rj(inod,is_rot+2) = - d2s_dr2                                 &
     &             + g_sph_rj(j,3)*ar_1d_rj(k,2)*d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_diff_pol_and_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_diff_poloidal2(kr_in, kr_out, is_fld)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d1s_dr1)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod-j) / nidx_rj(2)
!
        d1s_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
!
        d_rj(inod,is_fld+1) = d1s_dr1
      end do
!$omp end parallel do
!
      end subroutine cal_sph_diff_poloidal2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_rot2(kr_in, kr_out, is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d2s_dr2,d1t_dr1)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d2s_dr2 =  d2nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_rot  ) =   d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =   d1t_dr1
        d_rj(inod,is_rot+2) = - d2s_dr2                                 &
     &             + g_sph_rj(j,3)*ar_1d_rj(k,2)*d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_w_div_rot2(kr_in, kr_out,             &
     &          is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d1d_dr1, d1t_dr1
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d1d_dr1,d1t_dr1)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d1d_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld+1)          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld+1)          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld+1)
        d1t_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_rot  ) =   d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =   d1t_dr1
        d_rj(inod,is_rot+2) = - d1d_dr1                                 &
     &             + g_sph_rj(j,3)*ar_1d_rj(k,2)*d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_w_div_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_div2(kr_in, kr_out, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_div
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d1s_dr1)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d1s_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                     * max(g_sph_rj(j,3),half) * ar_1d_rj(k,2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_div2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_diffuse_by_rot2(kr_in, kr_out, coef_d,     &
     &          is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d2s_dr2,d1t_dr1)
!cdir nodep
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d2s_dr2 =  d2nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld+2)          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_rot  ) =  -coef_d * d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  -coef_d * d1t_dr1
        d_rj(inod,is_rot+2) =   coef_d * ( d2s_dr2                      &
     &                        - g_sph_rj(j,3)*ar_1d_rj(k,2)             &
     &                         *d_rj(inod,is_fld) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_diffuse_by_rot2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_rotation
