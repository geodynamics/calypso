!> @file  cal_sph_buoyancy_flux.f90
!!      module cal_sph_buoyancy_flux
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine sph_self_buoyancy_flux_rin(nnod_rtp, nidx_rtp,       &
!!     &          radius, coef, scalar, vr, prod)
!!      subroutine sph_self_buoyancy_flux_pin(nnod_rtp, nidx_rtp,       &
!!     &          radius, coef, scalar, vr, prod)
!!        integer(kind = kint), intent(in) :: nnod_rtp
!!        integer(kind = kint), intent(in) :: nidx_rtp(3)
!!        real(kind=kreal), intent(in) :: coef
!!        real(kind=kreal), intent(in) :: scalar(nnod_rtp), vr(nnod_rtp)
!!        real(kind=kreal), intent(in) :: radius(nidx_rtp(1))
!!        real(kind=kreal), intent(inout) :: prod(nnod_rtp)
!!
!!      subroutine pole_sph_self_buoyancy_flux(nnod_pole, nidx_rtp_r,   &
!!     &          radius, coef, t_pole, v_pole, d_pole)
!!      subroutine pole_sph_const_buoyancy_flux(nnod_pole, nidx_rtp_r,  &
!!     &          coef, t_pole, v_pole, d_pole)
!!        integer(kind = kint), intent(in) :: nnod_pole
!!        integer(kind = kint), intent(in) :: nidx_rtp_r
!!        real(kind=kreal), intent(in) :: radius(nidx_rtp_r)
!!        real(kind = kreal), intent(in) :: coef
!!        real(kind = kreal), intent(in) :: t_pole(nnod_pole)
!!        real(kind = kreal), intent(in) :: v_pole(nnod_pole,3)
!!        real(kind = kreal), intent(inout) :: d_pole(nnod_pole)
!!@endverbatim
!
      module cal_sph_buoyancy_flux
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_self_buoyancy_flux_rin(nnod_rtp, nidx_rtp,         &
     &          radius, coef, scalar, vr, prod)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: scalar(nnod_rtp), vr(nnod_rtp)
      real(kind=kreal), intent(in) :: radius(nidx_rtp(1))
!
      real(kind=kreal), intent(inout) :: prod(nnod_rtp)
!
      integer (kind=kint) :: inod, k, ml
!
!
!$omp parallel do private(ml,k,inod)
      do ml = 1, nidx_rtp(2)*nidx_rtp(3)
        do k = 1, nidx_rtp(1)
          inod = k + (ml-1) * nidx_rtp(1)
          prod(inod) =  coef*scalar(inod)*vr(inod)*radius(k)
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_self_buoyancy_flux_rin
!
!-----------------------------------------------------------------------
!
      subroutine sph_self_buoyancy_flux_pin(nnod_rtp, nidx_rtp,         &
     &          radius, coef, scalar, vr, prod)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: scalar(nnod_rtp), vr(nnod_rtp)
      real(kind=kreal), intent(in) :: radius(nidx_rtp(1))
!
      real(kind=kreal), intent(inout) :: prod(nnod_rtp)
!
      integer (kind=kint) :: inod, k, l, m
!
!
!$omp parallel private(l,k)
      do l = 1, nidx_rtp(2)
        do k = 1, nidx_rtp(1)
!$omp do private(m,inod)
          do m = 1, nidx_rtp(3)
            inod = m + (k-1) * nidx_rtp(3)                              &
     &               + (l-1) * nidx_rtp(3) * nidx_rtp(1)
            prod(inod) =  coef*scalar(inod)*vr(inod)*radius(k)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sph_self_buoyancy_flux_pin
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pole_sph_self_buoyancy_flux(nnod_pole, nidx_rtp_r,     &
     &          radius, coef, t_pole, v_pole, d_pole)
!
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: nidx_rtp_r
      real(kind=kreal), intent(in) :: radius(nidx_rtp_r)
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: t_pole(nnod_pole)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,3)
!
      real(kind = kreal), intent(inout) :: d_pole(nnod_pole)
!
      integer(kind = kint) :: inod, kr
!
!
!  field for north pole (kr) and south pole (inod)
!$omp parallel do private(kr,inod)
      do kr = 1, nidx_rtp_r
        inod = kr + nidx_rtp_r
        d_pole(kr) =    coef*t_pole(kr) * v_pole(kr,3) * radius(kr)
        d_pole(inod) = -coef*t_pole(inod)*v_pole(inod,3)*radius(kr)
      end do
!$omp end parallel do
!
      d_pole(2*nidx_rtp_r+1) = zero
!
      end subroutine pole_sph_self_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      subroutine pole_sph_const_buoyancy_flux(nnod_pole, nidx_rtp_r,    &
     &          coef, t_pole, v_pole, d_pole)
!
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: nidx_rtp_r
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: t_pole(nnod_pole)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,3)
!
      real(kind = kreal), intent(inout) :: d_pole(nnod_pole)
!
      integer(kind = kint) :: inod, kr
!
!
!  field for north pole (kr) and south pole (inod)
!$omp parallel do private(kr,inod)
      do kr = 1, nidx_rtp_r
        inod = kr + nidx_rtp_r
        d_pole(kr)                                                      &
     &       =  coef*t_pole(kr)*v_pole(kr,3)
        d_pole(inod)                                                    &
     &       = -coef*t_pole(inod)*v_pole(inod,3)
      end do
!$omp end parallel do
!
      d_pole(2*nidx_rtp_r+1) = zero
!
      end subroutine pole_sph_const_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      end module cal_sph_buoyancy_flux
