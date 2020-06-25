!>@file   poynting_flux_smp.f90
!!@brief  module poynting_flux_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Evaluate poynting flux in physical space
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_electric_field_smp                               &
!!     &         (nnod, coef_d, current, uxb, e_field)
!!      subroutine cal_poynting_flux_smp                                &
!!     &         (nnod, coef_d, current, uxb, b_field, poynting)
!!      subroutine cal_rtp_magnetic_streach                             &
!!     &         (nnod, nri, nth, a_r_1d_rtp_r, cot_theta_1d_rtp,       &
!!     &          b_field, u_field, grad_ux, grad_uy, grad_uz,          &
!!     &          magne_streach)
!!      subroutine cal_xyz_magnetic_streach(nnod, b_field,              &
!!     &          grad_ux, grad_uy, grad_uz, magne_streach)
!!
!!      subroutine cal_rtp_magnetic_streach_tmp                         &
!!     &         (nnod, nri, nth, a_r_1d_rtp_r, cot_theta_1d_rtp,       &
!!     &          b_field, u_field, grad_ux, grad_uy, grad_uz,          &
!!     &          magne_streach)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param coef_d        Coefficient for magnetic diffusion
!!
!!@n @param  b_field(nnod,3)     Magnetic field @f$ B_{i} @f$
!!@n @param  current(nnod,3)     Current density
!!                                 @f$e_{ijk) \partial_{j} B_{k})@f$
!!@n @param  uxb(nnod,3)         Induction @f$(e_{ijk) u{j} B_{k})@f$
!!@n @param  e_field(nnod,3)     Electric field
!!              @f$ E_{i} = \sigma^{-1} J_{i} - (e_{ijk) u{j} B_{k})@f$
!!@n @param  poynting(nnod,3)    Poynting flux @f$(e_{ijk) E{j} B_{k})@f$
!
      module poynting_flux_smp
!
      use m_precision
      use m_constants
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_electric_field_smp                                 &
     &         (nnod, coef_d, current, uxb, e_field)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef_d
      real (kind=kreal), intent(in) :: current(nnod,3), uxb(nnod,3)
      real (kind=kreal), intent(inout) :: e_field(nnod,3)
!
      integer(kind = kint) :: inod
!
!$omp do private(inod)
      do inod = 1, nnod
        e_field(inod,1) = coef_d * current(inod,1) - uxb(inod,1)
        e_field(inod,2) = coef_d * current(inod,2) - uxb(inod,2)
        e_field(inod,3) = coef_d * current(inod,3) - uxb(inod,3)
      end do
!$omp end do nowait
!
      end subroutine cal_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_poynting_flux_smp                                  &
     &         (nnod, coef_d, current, uxb, b_field, poynting)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef_d
      real (kind=kreal), intent(in) :: current(nnod,3), uxb(nnod,3)
      real (kind=kreal), intent(in) :: b_field(nnod,3)
      real (kind=kreal), intent(inout) :: poynting(nnod,3)
!
      integer(kind = kint) :: inod
      real (kind=kreal) :: e_fld(3)
!
!$omp do private(inod,e_fld)
      do inod = 1, nnod
        e_fld(1) = coef_d * current(inod,1) - uxb(inod,1)
        e_fld(2) = coef_d * current(inod,2) - uxb(inod,2)
        e_fld(3) = coef_d * current(inod,3) - uxb(inod,3)
!
        poynting(inod,1)                                                &
     &         = e_fld(2)*b_field(inod,3) - e_fld(3)*b_field(inod,2)
        poynting(inod,2)                                                &
     &         = e_fld(3)*b_field(inod,1) - e_fld(1)*b_field(inod,3)
        poynting(inod,3)                                                &
     &         = e_fld(1)*b_field(inod,2) - e_fld(2)*b_field(inod,1)
      end do
!$omp end do nowait
!
      end subroutine cal_poynting_flux_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_magnetic_streach                               &
     &         (nnod, nri, nth, a_r_1d_rtp_r, cot_theta_1d_rtp,         &
     &          b_field, u_field, grad_ux, grad_uy, grad_uz,            &
     &          magne_streach)
!
      integer (kind=kint), intent(in) :: nnod, nri, nth
      real (kind=kreal), intent(in) :: a_r_1d_rtp_r(nri)
      real (kind=kreal), intent(in) :: cot_theta_1d_rtp(nth)
      real (kind=kreal), intent(in) :: grad_ux(nnod,3), grad_uy(nnod,3)
      real (kind=kreal), intent(in) :: grad_uz(nnod,3)
      real (kind=kreal), intent(in) :: u_field(nnod,3), b_field(nnod,3)
      real (kind=kreal), intent(inout) :: magne_streach(nnod,3)
!
      integer(kind = kint) :: inod, lt, kr, lnod
!
!$omp do private(inod,kr,lnod,lt)
      do inod = 1, nnod
        kr =   ione + mod( (inod-ione),nri)
        lnod = ione + (inod - kr) / nri
        lt =   ione + mod( (lnod-ione),nth)
!
        magne_streach(inod,1) =  grad_ux(inod,1)*b_field(inod,1)        &
     &                         + grad_ux(inod,2)*b_field(inod,2)        &
     &                         + grad_ux(inod,3)*b_field(inod,3)        &
     &                         - (b_field(inod,2)*u_field(inod,2)       &
     &                          + b_field(inod,3)*u_field(inod,3))      &
     &                         * a_r_1d_rtp_r(kr)
        magne_streach(inod,2) =  grad_uy(inod,1)*b_field(inod,1)        &
     &                         + grad_uy(inod,2)*b_field(inod,2)        &
     &                         + grad_uy(inod,3)*b_field(inod,3)        &
     &                          - (b_field(inod,3)*u_field(inod,3)      &
     &                          * cot_theta_1d_rtp(lt)                  &
     &                          - b_field(inod,2)*u_field(inod,1))      &
     &                         * a_r_1d_rtp_r(kr)
        magne_streach(inod,3) =  grad_uz(inod,1)*b_field(inod,1)        &
     &                         + grad_uz(inod,2)*b_field(inod,2)        &
     &                         + grad_uz(inod,3)*b_field(inod,3)        &
     &                         + (b_field(inod,3)*u_field(inod,1)       &
     &                          + b_field(inod,3)*u_field(inod,2)       &
     &                          * cot_theta_1d_rtp(lt) )                &
     &                         * a_r_1d_rtp_r(kr)
      end do
!$omp end do nowait
!
      end subroutine cal_rtp_magnetic_streach
!
! -----------------------------------------------------------------------
!
      subroutine cal_xyz_magnetic_streach(nnod, b_field,                &
     &          grad_ux, grad_uy, grad_uz, magne_streach)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: grad_ux(nnod,3), grad_uy(nnod,3)
      real (kind=kreal), intent(in) :: grad_uz(nnod,3)
      real (kind=kreal), intent(in) :: b_field(nnod,3)
!
      real (kind=kreal), intent(inout) :: magne_streach(nnod,3)
!
      integer(kind = kint) :: inod
!
!$omp do private(inod)
      do inod = 1, nnod
        magne_streach(inod,1) =  grad_ux(inod,1)*b_field(inod,1)        &
     &                         + grad_ux(inod,2)*b_field(inod,2)        &
     &                         + grad_ux(inod,3)*b_field(inod,3)
        magne_streach(inod,2) =  grad_uy(inod,1)*b_field(inod,1)        &
     &                         + grad_uy(inod,2)*b_field(inod,2)        &
     &                         + grad_uy(inod,3)*b_field(inod,3)
        magne_streach(inod,3) =  grad_uz(inod,1)*b_field(inod,1)        &
     &                         + grad_uz(inod,2)*b_field(inod,2)        &
     &                         + grad_uz(inod,3)*b_field(inod,3)
      end do
!$omp end do nowait
!
      end subroutine cal_xyz_magnetic_streach
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_magnetic_streach_tmp                           &
     &         (nnod, nri, nth, a_r_1d_rtp_r, cot_theta_1d_rtp,         &
     &          b_field, u_field, grad_ux, grad_uy, grad_uz,            &
     &          magne_streach)
!
      integer (kind=kint), intent(in) :: nnod, nri, nth
      real (kind=kreal), intent(in) :: a_r_1d_rtp_r(nri)
      real (kind=kreal), intent(in) :: cot_theta_1d_rtp(nth)
      real (kind=kreal), intent(in) :: grad_ux(nnod,3), grad_uy(nnod,3)
      real (kind=kreal), intent(in) :: grad_uz(nnod,3)
      real (kind=kreal), intent(in) :: u_field(nnod,3), b_field(nnod,3)
      real (kind=kreal), intent(inout) :: magne_streach(nnod,3)
!
      integer(kind = kint) :: inod, lt, kr, lnod
!
!$omp do private(inod,kr,lnod,lt)
      do inod = 1, nnod
        kr =   ione + mod( (inod-ione),nri)
        lnod = ione + (inod - kr) / nri
        lt =   ione + mod( (lnod-ione),nth)
!
!        magne_streach(inod,1) =  grad_ux(inod,1)*b_field(inod,1)       &
!     &                         + grad_ux(inod,2)*b_field(inod,2)       &
!     &                         + grad_ux(inod,3)*b_field(inod,3)       &
!     &                         - (b_field(inod,2)*u_field(inod,2)      &
!     &                          + b_field(inod,3)*u_field(inod,3))     &
!     &                         * a_r_1d_rtp_r(kr)
!        magne_streach(inod,2) =  grad_uy(inod,1)*b_field(inod,1)       &
!     &                         + grad_uy(inod,2)*b_field(inod,2)       &
!     &                         + grad_uy(inod,3)*b_field(inod,3)       &
!     &                          - (b_field(inod,3)*u_field(inod,3)     &
!     &                          * cot_theta_1d_rtp(lt)                 &
!     &                          - b_field(inod,2)*u_field(inod,1))     &
!     &                         * a_r_1d_rtp_r(kr)
!        magne_streach(inod,3) =  grad_uz(inod,1)*b_field(inod,1)       &
!     &                         + grad_uz(inod,2)*b_field(inod,2)       &
!     &                         + grad_uz(inod,3)*b_field(inod,3)       &
!     &                         + (b_field(inod,3)*u_field(inod,1)      &
!     &                          + b_field(inod,3)*u_field(inod,2)      &
!     &                          * cot_theta_1d_rtp(lt) )               &
!     &                         * a_r_1d_rtp_r(kr)
!
        magne_streach(inod,1) = zero
        magne_streach(inod,2) = zero
        magne_streach(inod,3) =  grad_uz(inod,1)*b_field(inod,1)       &
     &                         + grad_uz(inod,2)*b_field(inod,2)
!         magne_streach(inod,3) = grad_uz(inod,3)*b_field(inod,3)
      end do
!$omp end do nowait
!
      end subroutine cal_rtp_magnetic_streach_tmp
!
! -----------------------------------------------------------------------
!
      end module poynting_flux_smp
