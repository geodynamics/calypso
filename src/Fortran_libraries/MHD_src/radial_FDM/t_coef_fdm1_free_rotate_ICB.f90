!>@file   t_coef_fdm1_free_rotate_ICB.f90
!!@brief  module t_coef_fdm1_free_rotate_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate 1st order FDM
!!      at ICB with fixed field boundary
!!
!!@verbatim
!!      subroutine check_fdm1_ICB_fixed_field(id_file, fdm1_fix_fld_ICB)
!!        integer(kind = kint), intent(in) :: id_file
!!        real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!!
!!      subroutine cal_fdm1_coef_fix_fld_ICB(r_from_ICB,                &
!!     &          fdm1_fix_fld_ICB)
!!        real(kind = kreal), intent(in) :: r_from_ICB(0:1)
!!        real(kind = kreal), intent(inout) :: fdm1_fix_fld_ICB(0:1,2)
!!
!!      subroutine set_rotate_icb_vt_sph_mat(dt, idx_rj_l0, kr_in,      &
!!     &          nri, jmax, ar_1d_rj, fdm1_fix_fld_ICB, coef_imp,      &
!!     &          coef_d, vt_evo_mat)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
!!        real(kind = kreal), intent(in) :: coef_imp, coef_d
!!        real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
!!        real(kind = kreal), intent(in) :: dt
!!        real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!!        real(kind = kreal), intent(inout) :: vt_evo_mat(3,nri,jmax)
!!      subroutine cal_icore_viscous_drag_l1(idx_rj_l0, kr_in,          &
!!     &          fdm1_fix_fld_ICB, coef_d, it_velo, it_viscous,        &
!!     &          nri, jmax, ar_1d_rj, n_point, ntot_phys_rj, d_rj)
!!        integer(kind = kint), intent(in) :: n_point, nri, jmax
!!        real(kind = kreal), intent(in) :: coef_d
!!        integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
!!        integer(kind = kint), intent(in) :: it_velo, it_viscous
!!        integer(kind = kint), intent(in) :: ntot_phys_rj
!!        real(kind= kreal), intent(in) :: ar_1d_rj(nri,3)
!!        real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!!        real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!!
!!
!!   Matrix for derivatives with fixed field
!!    at inner boundary of the shell
!!      dfdr =      fdm1_fix_fld_ICB( 0,2) * d_rj(ICB  )
!!                + fdm1_fix_fld_ICB( 1,2) * d_rj(ICB+1)
!!@endverbatim
!!
      module t_coef_fdm1_free_rotate_ICB
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
      subroutine check_fdm1_ICB_fixed_field(id_file, fdm1_fix_fld_ICB)
!
      integer(kind = kint), intent(in) :: id_file
      real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!
      write(id_file,*) ' fdm1_fix_fld_ICB'
      write(id_file,*) ' mat_fdm21,  mat_fdm22'
      write(id_file,'(1p9E25.15e3)') fdm1_fix_fld_ICB(0:1,2)
!
      end subroutine check_fdm1_ICB_fixed_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm1_coef_fix_fld_ICB(r_from_ICB,                  &
     &          fdm1_fix_fld_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:1)
      real(kind = kreal), intent(inout) :: fdm1_fix_fld_ICB(0:1,2)
!
      real(kind = kreal) :: dr_p1
!
!
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
!
      fdm1_fix_fld_ICB(0,1) =  one
      fdm1_fix_fld_ICB(1,1) =  zero
      fdm1_fix_fld_ICB(0,2) = -one / dr_p1
      fdm1_fix_fld_ICB(1,2) =  one / dr_p1
!
      end subroutine cal_fdm1_coef_fix_fld_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rotate_icb_vt_sph_mat(dt, idx_rj_l0, kr_in,        &
     &          nri, jmax, ar_1d_rj, fdm1_fix_fld_ICB, coef_imp,        &
     &          coef_d, vt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
      real(kind = kreal), intent(in) :: coef_imp, coef_d
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!
      real(kind = kreal), intent(inout) :: vt_evo_mat(3,nri,jmax)
!
!
      if(idx_rj_l0 .le. 0) return
!
!       vt_evo_mat(3,kr_in-1,idx_rj_l0) = zero
        vt_evo_mat(2,kr_in,  idx_rj_l0)                                 &
     &     = one - coef_imp*dt*coef_d * five                            &
     &      * (fdm1_fix_fld_ICB(0,2) - two*ar_1d_rj(kr_in,1))           &
     &      * ar_1d_rj(kr_in,1)
        vt_evo_mat(1,kr_in+1,idx_rj_l0)                                 &
     &     = - coef_imp*dt*coef_d * five * ar_1d_rj(kr_in,1)            &
     &        * fdm1_fix_fld_ICB(1,2)
!
      end subroutine set_rotate_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine cal_icore_viscous_drag_l1(idx_rj_l0, kr_in,            &
     &          fdm1_fix_fld_ICB, coef_d, it_velo, it_viscous,          &
     &          nri, jmax, ar_1d_rj, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      real(kind = kreal), intent(in) :: coef_d
      integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
      integer(kind = kint), intent(in) :: it_velo, it_viscous
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real(kind= kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) ::  i10c_ri, i10c_r1
      real(kind = kreal) :: mat_1, mat_0
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_ri = idx_rj_l0 + (kr_in-1)*jmax
      i10c_r1 = idx_rj_l0 +  kr_in * jmax
!
      mat_0 = fdm1_fix_fld_ICB(0,2) - two*ar_1d_rj(kr_in,1)
      mat_1 = fdm1_fix_fld_ICB(1,2)
!
      d_rj(i10c_ri,it_viscous)                                          &
     &                   =  five  * coef_d * ar_1d_rj(kr_in,1)          &
     &                          * (mat_0 * d_rj(i10c_ri,it_velo)        &
     &                           + mat_1 * d_rj(i10c_r1,it_velo))
!
      end subroutine cal_icore_viscous_drag_l1
!
! ----------------------------------------------------------------------
!
      end module t_coef_fdm1_free_rotate_ICB
