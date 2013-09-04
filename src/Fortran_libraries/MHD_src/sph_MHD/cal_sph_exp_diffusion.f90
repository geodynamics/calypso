!>@file   cal_sph_exp_diffusion.f90
!!@brief  module cal_sph_exp_diffusion
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate diffusion term
!!
!!@verbatim
!!      subroutine cal_sph_nod_scalar_diffuse2(kr_in, kr_out, coef_d,   &
!!     &          is_fld, is_diffuse)
!!      subroutine cal_sph_nod_vect_diffuse2(kr_in, kr_out, coef_d,     &
!!     &          is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param kr_in    radial ID for inner boundary
!!@n @param kr_out   radial ID for outer boundary
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param is_fld     Input field address for d_rj
!!@n @param is_diffuse Diffusion term address for d_rj
!
      module cal_sph_exp_diffusion
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
      subroutine cal_sph_nod_scalar_diffuse2(kr_in, kr_out, coef_d,     &
     &          is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal) :: d1s_dr1
      real(kind = kreal) :: d2s_dr2
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist  = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d2s_dr2,d1s_dr1)
!cdir nodep
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d1s_dr1 =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
        d2s_dr2 =  d2nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld  )          &
     &           + d2nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld  )
!
        d_rj(inod,is_diffuse  )                                         &
     &          = coef_d * (d2s_dr2 + two*ar_1d_rj(k,1) * d1s_dr1       &
     &           - g_sph_rj(j,3)*ar_1d_rj(k,2)*d_rj(inod,is_fld) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_scalar_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_diffuse2(kr_in, kr_out, coef_d,       &
     &          is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal) :: d2s_dr2, d2t_dr2
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist  = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d2s_dr2,d2t_dr2)
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
        d2t_dr2 =  d2nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld+2)          &
     &           + d2nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld+2)          &
     &           + d2nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &           - g_sph_rj(j,3)*ar_1d_rj(k,2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &           - g_sph_rj(j,3)*ar_1d_rj(k,2)*d_rj(inod,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_diffuse2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_diffusion
