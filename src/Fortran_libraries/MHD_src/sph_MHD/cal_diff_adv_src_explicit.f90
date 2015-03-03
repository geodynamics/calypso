!> @file  cal_diff_adv_src_explicit.f90
!!      module cal_diff_adv_src_explicit
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution explicitly
!!
!!@verbatim
!!      subroutine sel_scalar_diff_adv_src_adams(kr_st, kr_ed,          &
!!     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          ipol_pre, coef_exp, coef_src)
!!      subroutine sel_scalar_diff_adv_src_euler(kr_st, kr_ed,          &
!!     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,  &
!!     &          coef_exp, coef_adv, coef_src)
!!
!!      subroutine sel_ini_adams_scalar_w_src(kr_st, kr_ed,             &
!!     &          ipol_advect, ipol_source, ipol_pre, coef_src)
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
      module cal_diff_adv_src_explicit
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_t_int_parameter
!
      implicit  none
!
      private :: scalar_diff_adv_src_adams, scalar_diff_adv_src_euler
      private :: set_ini_adams_scalar_w_src
      private :: scalar_stable_diff_src, scalar_stable_diffusion
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_scalar_diff_adv_src_adams(kr_st, kr_ed,            &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, coef_exp, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
!
!
      if(ipol_source .eq. izero) then
        call scalar_diff_advect_adams(kr_st, kr_ed,                     &
     &          ipol_diffuse, ipol_advect, ipol_scalar, ipol_pre,       &
     &          coef_exp)
      else
        call scalar_diff_adv_src_adams(kr_st, kr_ed,                    &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, coef_exp, coef_src)
      end if
!
      end subroutine sel_scalar_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_scalar_diff_adv_src_euler(kr_st, kr_ed,            &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          coef_exp, coef_adv, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_adv, coef_src
!
!
      if(coef_adv .eq. zero) then
        if(ipol_source .eq. izero) then
          call scalar_stable_diffusion(kr_st, kr_ed, ipol_scalar)
        else
          call scalar_stable_diff_src(kr_st, kr_ed,                     &
     &        ipol_source, ipol_scalar, coef_src)
        end if
      else if(ipol_source .eq. izero) then
        call scalar_diff_advect_euler(kr_st, kr_ed,                     &
     &          ipol_diffuse, ipol_advect, ipol_scalar, coef_exp)
      else
        call scalar_diff_adv_src_euler(kr_st, kr_ed,                    &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          coef_exp, coef_src)
      end if
!
      end subroutine sel_scalar_diff_adv_src_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_scalar_w_src(kr_st, kr_ed,               &
     &          ipol_advect, ipol_source, ipol_pre, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_advect, ipol_source
      integer(kind = kint), intent(in) :: ipol_pre
      real(kind = kreal), intent(in) :: coef_src
!
!
      if(ipol_source .eq. izero) then
        call set_ini_adams_scalar(kr_st, kr_ed, ipol_advect, ipol_pre)
      else
        call set_ini_adams_scalar_w_src(kr_st, kr_ed,                   &
     &          ipol_advect, ipol_source, ipol_pre, coef_src)
      end if
!
      end subroutine sel_ini_adams_scalar_w_src
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_diff_advect_adams(kr_st, kr_ed,                 &
     &          ipol_diffuse, ipol_advect, ipol_scalar, ipol_pre,       &
     &          coef_exp)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_scalar) = d_rj(inod,ipol_scalar)                 &
     &          + dt * (coef_exp * d_rj(inod,ipol_diffuse)              &
     &              - adam_0 * d_rj(inod,ipol_advect)                   &
     &              + adam_1 * d_rj(inod,ipol_pre) )
!
         d_rj(inod,ipol_pre) = - d_rj(inod,ipol_advect)
       end do
!$omp end do
!
      end subroutine scalar_diff_advect_adams
!
! ----------------------------------------------------------------------
!
      subroutine scalar_diff_advect_euler(kr_st, kr_ed,                 &
     &          ipol_diffuse, ipol_advect, ipol_scalar, coef_exp)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_scalar) = d_rj(inod,ipol_scalar)                 &
     &         + dt * (coef_exp*d_rj(inod,ipol_diffuse)                 &
     &             - d_rj(inod,ipol_advect) )
       end do
!$omp end do
!
      end subroutine scalar_diff_advect_euler
!
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_scalar(kr_st, kr_ed,                     &
     &          ipol_advect, ipol_pre)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_advect, ipol_pre
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
         d_rj(inod,ipol_pre) =  -d_rj(inod,ipol_advect)
      end do
!$omp end do
!
      end subroutine set_ini_adams_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_diff_adv_src_adams(kr_st, kr_ed,                &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          ipol_pre, coef_exp, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_scalar) = d_rj(inod,ipol_scalar)                 &
     &          + dt * (coef_exp * d_rj(inod,ipol_diffuse)              &
     &              + adam_0 * ( -d_rj(inod,ipol_advect)                &
     &                 + coef_src * d_rj(inod,ipol_source) )            &
     &              + adam_1 * d_rj(inod,ipol_pre) )
!
         d_rj(inod,ipol_pre) = - d_rj(inod,ipol_advect)                 &
     &               + coef_src * d_rj(inod,ipol_source)
       end do
!$omp end do
!
      if(inod_rj_center .eq. 0) return
        inod = inod_rj_center
        d_rj(inod,ipol_scalar) = d_rj(inod,ipol_scalar)                 &
     &          + dt * (coef_exp * d_rj(inod,ipol_diffuse)              &
     &              + adam_0 * ( -d_rj(inod,ipol_advect)                &
     &                 + coef_src * d_rj(inod,ipol_source) )            &
     &              + adam_1 * d_rj(inod,ipol_pre) )
!
         d_rj(inod,ipol_pre) = - d_rj(inod,ipol_advect)                 &
     &               + coef_src * d_rj(inod,ipol_source)
!
      end subroutine scalar_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine scalar_diff_adv_src_euler(kr_st, kr_ed,                &
     &          ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,    &
     &          coef_exp, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_src
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_scalar) = d_rj(inod,ipol_scalar)                 &
     &         + dt * (coef_exp*d_rj(inod,ipol_diffuse)                 &
     &             - d_rj(inod,ipol_advect)                             &
     &              + coef_src * d_rj(inod,ipol_source) )
       end do
!$omp end do
!
      if(inod_rj_center .eq. 0) return
        inod = inod_rj_center
        d_rj(inod,ipol_scalar) = d_rj(inod,ipol_scalar)                 &
     &         + dt * (coef_exp*d_rj(inod,ipol_diffuse)                 &
     &             - d_rj(inod,ipol_advect)                             &
     &              + coef_src * d_rj(inod,ipol_source) )
!
      end subroutine scalar_diff_adv_src_euler
!
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_scalar_w_src(kr_st, kr_ed,               &
     &          ipol_advect, ipol_source, ipol_pre, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_advect, ipol_source
      integer(kind = kint), intent(in) :: ipol_pre
      real(kind = kreal), intent(in) :: coef_src
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
         d_rj(inod,ipol_pre) =  -d_rj(inod,ipol_advect)                 &
     &                        + coef_src * d_rj(inod,ipol_source)
      end do
!$omp end do
!
      if(inod_rj_center .eq. 0) return
      d_rj(inod_rj_center,ipol_pre) = -d_rj(inod_rj_center,ipol_advect) &
     &                    + coef_src * d_rj(inod_rj_center,ipol_source)
!
      end subroutine set_ini_adams_scalar_w_src
!
! ----------------------------------------------------------------------
!
      subroutine scalar_stable_diff_src(kr_st, kr_ed,                   &
     &          ipol_source, ipol_scalar, coef_src)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_source, ipol_scalar
      real(kind = kreal), intent(in) :: coef_src
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_scalar) = coef_src * d_rj(inod,ipol_source)
      end do
!$omp end do
!
      if(inod_rj_center .eq. 0) return
      d_rj(inod_rj_center,ipol_scalar)                                  &
     &     = coef_src * d_rj(inod_rj_center,ipol_source)
!
      end subroutine scalar_stable_diff_src
!
! ----------------------------------------------------------------------
!
      subroutine scalar_stable_diffusion(kr_st, kr_ed, ipol_scalar)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_scalar
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_st-1)*nidx_rj(2) + 1
      ied = kr_ed * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_scalar) = zero
      end do
!$omp end do
!
      end subroutine scalar_stable_diffusion
!
! ----------------------------------------------------------------------
!
      end module cal_diff_adv_src_explicit
