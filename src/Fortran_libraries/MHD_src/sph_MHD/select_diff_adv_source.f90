!> @file  select_diff_adv_source.f90
!!      module select_diff_adv_source
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution explicitly
!!
!!@verbatim
!!      subroutine sel_scalar_diff_adv_src_adams(kr_st, kr_ed,          &
!!     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          ipol_pre, dt, coef_exp, coef_src, sph_rj, rj_fld)
!!      subroutine sel_scalar_diff_adv_src_euler(kr_st, kr_ed,          &
!!     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
!!      subroutine sel_ini_adams_scalar_w_src                           &
!!     &         (kr_st, kr_ed, ipol_advect, ipol_source, ipol_pre,     &
!!     &          coef_src, sph_rj, rj_fld)
!!
!!      subroutine sel_ctr_scl_diff_adv_src_adams                       &
!!     &         (ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          ipol_pre, dt, coef_exp, coef_src, sph_rj, rj_fld)
!!      subroutine sel_ctr_scl_diff_adv_src_euler                       &
!!     &         (ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param kr_st         Radial address for inner boundary
!!@param kr_ed         Radial address for outer boundary
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
      use t_spheric_rj_data
      use t_phys_data
!
      use cal_diff_adv_src_explicit
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_scalar_diff_adv_src_adams(kr_st, kr_ed,            &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, dt, coef_exp, coef_src, sph_rj, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_st-1) * sph_rj%nidx_rj(2) + 1
      ied = kr_ed * sph_rj%nidx_rj(2)
!
      if(ipol_source .eq. izero) then
        call scalar_diff_advect_adams(ist, ied, ipol_diffuse,           &
     &      ipol_advect, ipol_scalar, ipol_pre, dt, coef_exp,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call scalar_diff_adv_src_adams                                  &
     &     (ist, ied, ipol_diffuse, ipol_advect, ipol_source,           &
     &      ipol_scalar, ipol_pre, dt, coef_exp, coef_src,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_scalar_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_scalar_diff_adv_src_euler(kr_st, kr_ed,            &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_adv, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_st-1) * sph_rj%nidx_rj(2) + 1
      ied = kr_ed * sph_rj%nidx_rj(2)
!
      if(coef_adv .eq. zero) then
        write(*,*) 'coef_adv', coef_adv
        if(ipol_source .eq. izero) then
          call stable_scalar_diffusion(ist, ied, ipol_scalar,           &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          call stable_scalar_diff_src                                   &
     &       (ist, ied, ipol_source, ipol_scalar, coef_src,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else
        if(ipol_source .eq. izero) then
          call scalar_diff_advect_euler(ist, ied,                       &
     &        ipol_diffuse, ipol_advect, ipol_scalar, dt, coef_exp,     &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          call scalar_diff_adv_src_euler(ist, ied,                      &
     &        ipol_diffuse, ipol_advect, ipol_source,                   &
     &        ipol_scalar, dt, coef_exp, coef_src,                      &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      end if
!
      end subroutine sel_scalar_diff_adv_src_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_scalar_w_src                             &
     &         (kr_st, kr_ed, ipol_advect, ipol_source, ipol_pre,       &
     &          coef_src, sph_rj, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_advect, ipol_source
      integer(kind = kint), intent(in) :: ipol_pre
      real(kind = kreal), intent(in) :: coef_src
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_st-1) * sph_rj%nidx_rj(2) + 1
      ied = kr_ed * sph_rj%nidx_rj(2)
!
      if(ipol_source .eq. izero) then
        call set_ini_adams_scalar(ist, ied, ipol_advect, ipol_pre,      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call set_ini_adams_scalar_w_src                                 &
     &     (ist, ied, ipol_advect, ipol_source, ipol_pre,               &
     &      coef_src, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call center_ini_adams_scalar_w_src(sph_rj%inod_rj_center,       &
     &      ipol_advect, ipol_source, ipol_pre,                         &
     &      coef_src, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ini_adams_scalar_w_src
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_ctr_scl_diff_adv_src_adams                         &
     &         (ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, dt, coef_exp, coef_src, sph_rj, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_source .eq. izero) return
      call center_scl_diff_adv_src_adams(sph_rj%inod_rj_center,         &
     &    ipol_diffuse, ipol_advect, ipol_source,                       &
     &    ipol_scalar, ipol_pre, dt, coef_exp, coef_src,                &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine sel_ctr_scl_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_ctr_scl_diff_adv_src_euler                         &
     &         (ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_adv, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(coef_adv .eq. zero) then
        write(*,*) 'coef_adv', coef_adv
        if(ipol_source .eq. izero) then
          rj_fld%d_fld(sph_rj%inod_rj_center,ipol_scalar) = zero
        else
          rj_fld%d_fld(sph_rj%inod_rj_center,ipol_scalar)               &
     &     = coef_src * rj_fld%d_fld(sph_rj%inod_rj_center,ipol_source)
        end if
!
      else if(ipol_source .gt. izero) then
        call center_scl_diff_adv_src_euler(sph_rj%inod_rj_center,       &
     &      ipol_diffuse, ipol_advect, ipol_source,                     &
     &      ipol_scalar, dt, coef_exp, coef_src,                        &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ctr_scl_diff_adv_src_euler
!
! ----------------------------------------------------------------------
!
      end module select_diff_adv_source
