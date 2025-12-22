!> @file  select_diff_adv_source.f90
!!      module select_diff_adv_source
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution explicitly
!!
!!@verbatim
!!      subroutine sel_scalar_diff_adv_src_adams(ist, ied, inod_center, &
!!     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          ipol_pre, dt, coef_exp, coef_src, rj_fld)
!!      subroutine sel_scalar_diff_adv_src_euler(ist, ied, inod_center, &
!!     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          dt, coef_exp, coef_src, rj_fld)
!!      subroutine sel_exp_static_src_euler(ist, ied, inod_center,      &
!!     &          ipol_source, ipol_scalar, coef_src, rj_fld)
!!      subroutine sel_ini_adams_scalar_w_src(ist, ied, inod_center,    &
!!     &          ipol_advect, ipol_source, ipol_pre, coef_src, rj_fld)
!!        integer(kind = kint), intent(in) :: ist, ied, inod_center
!!        integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
!!        integer(kind = kint), intent(in) :: ipol_source
!!        integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
!!        real(kind = kreal), intent(in) :: coef_exp, coef_src
!!        real(kind = kreal), intent(in) :: dt
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param ipol_diffuse  address for diffusion term
!!@param ipol_advect   address for advection term
!!@param ipol_source   address for source term
!!@param ipol_scalar   address for scalar field to update
!!@param ipol_pre      address for storeing previous evolution
!!@param coef_exp      coeefient for expilict evolution for diffusion
!!@param coef_src      coefficient for source term
!
      module select_diff_adv_source
!
      use m_precision
      use m_constants
!
      use t_phys_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_scalar_diff_adv_src_adams(ist, ied, inod_center,   &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, dt, coef_exp, coef_src, rj_fld)
!
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_source .eq. izero) then
        call scalar_diff_advect_adams(ist, ied, ipol_diffuse,           &
     &      ipol_advect, ipol_scalar, ipol_pre, dt, coef_exp,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call scalar_diff_adv_src_adams                                  &
     &     (ist, ied, inod_center, ipol_diffuse, ipol_advect,           &
     &      ipol_source, ipol_scalar, ipol_pre, dt, coef_exp, coef_src, &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_scalar_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_scalar_diff_adv_src_euler(ist, ied, inod_center,   &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          dt, coef_exp, coef_src, rj_fld)
!
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_source .eq. izero) then
        call scalar_diff_advect_euler(ist, ied,                         &
     &      ipol_diffuse, ipol_advect, ipol_scalar, dt, coef_exp,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call scalar_diff_adv_src_euler(ist, ied, inod_center,           &
     &      ipol_diffuse, ipol_advect, ipol_source,                     &
     &      ipol_scalar, dt, coef_exp, coef_src,                        &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_scalar_diff_adv_src_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_exp_static_src_euler(ist, ied, inod_center,        &
     &          ipol_source, ipol_scalar, coef_src, rj_fld)
!
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_src
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_source .eq. izero) then
        call stable_scalar_diffusion                                    &
     &     (ist, ied, inod_center, ipol_scalar,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call stable_scalar_diff_src(ist, ied, inod_center,              &
     &      ipol_source, ipol_scalar, coef_src,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_exp_static_src_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_scalar_w_src(ist, ied, inod_center,      &
     &          ipol_advect, ipol_source, ipol_pre, coef_src, rj_fld)
!
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_advect, ipol_source
      integer(kind = kint), intent(in) :: ipol_pre
      real(kind = kreal), intent(in) :: coef_src
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_source .eq. izero) then
        call set_ini_adams_scalar(ist, ied, ipol_advect, ipol_pre,      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call set_ini_adams_scalar_w_src(ist, ied, inod_center,          &
     &      ipol_advect, ipol_source, ipol_pre, coef_src,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ini_adams_scalar_w_src
!
! ----------------------------------------------------------------------
!
      end module select_diff_adv_source
