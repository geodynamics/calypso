!> @file  pole_poynting_flux_smp.f90
!!      module pole_poynting_flux_smp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Evaluate products at poles
!!
!!@verbatim
!!      subroutine cal_pole_electric_field_smp(numnod, internal_node,   &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef_d,                     &
!!     &          current, uxb, e_field)
!!      subroutine cal_pole_poynting_flux_smp(numnod, internal_node,    &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef_d,                     &
!!     &          current, uxb, b_field, poynting)
!!@endverbatim
!!
!!@n @param numnod               number of nodes
!!@n @param internal_node        number of internal nodes
!!@n @param xx                   position
!!@n @param nnod_rtp             number of grid points
!!                               for @f$ f(r,\theta,\phi) @f$
!!@n @param nidx_rtp_r           number of radial points 
!!                               for @f$ f(r,\theta,\phi) @f$
!!@n @param coef_d        Coefficient for magnetic diffusion
!!
!!@n @param  b_field(numnod,3)  Magnetic field @f$ B_{i} @f$
!!@n @param  current(numnod,3)  Current density
!!                                 @f$e_{ijk) \partial_{j} B_{k})@f$
!!@n @param  uxb(numnod,3)      Induction @f$(e_{ijk) u{j} B_{k})@f$
!!@n @param  e_field(numnod,3)  Electric field
!!              @f$ E_{i} = \sigma^{-1} J_{i} - (e_{ijk) u{j} B_{k})@f$
!!@n @param  poynting(numnod,3) Poynting flux @f$(e_{ijk) E{j} B_{k})@f$
!
      module pole_poynting_flux_smp
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_pole_electric_field_smp(numnod, internal_node,     &
     &          xx, nnod_rtp, nidx_rtp_r, coef_d,                       &
     &          current, uxb, e_field)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: current(numnod,3)
      real(kind = kreal), intent(in) :: uxb(numnod,3)
      real(kind = kreal), intent(inout) :: e_field(numnod,3)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          e_field(inod,1) = coef_d * current(inod,1) - uxb(inod,1)
          e_field(inod,2) = coef_d * current(inod,2) - uxb(inod,2)
          e_field(inod,3) = coef_d * current(inod,3) - uxb(inod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          e_field(inod,1) = coef_d * current(inod,1) - uxb(inod,1)
          e_field(inod,2) = coef_d * current(inod,2) - uxb(inod,2)
          e_field(inod,3) = coef_d * current(inod,3) - uxb(inod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      e_field(inod,1) = coef_d * current(inod,1) - uxb(inod,1)
      e_field(inod,2) = coef_d * current(inod,2) - uxb(inod,2)
      e_field(inod,3) = coef_d * current(inod,3) - uxb(inod,3)
!
      end subroutine cal_pole_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pole_poynting_flux_smp(numnod, internal_node,      &
     &          xx, nnod_rtp, nidx_rtp_r, coef_d,                       &
     &          current, uxb, b_field, poynting)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: current(numnod,3)
      real(kind = kreal), intent(in) :: uxb(numnod,3)
      real(kind = kreal), intent(in) :: b_field(numnod,3)
      real(kind = kreal), intent(inout) :: poynting(numnod,3)
!
      integer(kind = kint) :: inod, kr
      real (kind=kreal) :: e_fld(3)
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          e_fld(1) = coef_d * current(inod,1) - uxb(inod,1)
          e_fld(2) = coef_d * current(inod,2) - uxb(inod,2)
          e_fld(3) = coef_d * current(inod,3) - uxb(inod,3)
!
          poynting(inod,1)                                              &
     &         = e_fld(2)*b_field(inod,3) - e_fld(3)*b_field(inod,2)
          poynting(inod,2)                                              &
     &         = e_fld(3)*b_field(inod,1) - e_fld(1)*b_field(inod,3)
          poynting(inod,3)                                              &
     &         = e_fld(1)*b_field(inod,2) - e_fld(2)*b_field(inod,1)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          e_fld(1) = coef_d * current(inod,1) - uxb(inod,1)
          e_fld(2) = coef_d * current(inod,2) - uxb(inod,2)
          e_fld(3) = coef_d * current(inod,3) - uxb(inod,3)
!
          poynting(inod,1)                                              &
     &         = e_fld(2)*b_field(inod,3) - e_fld(3)*b_field(inod,2)
          poynting(inod,2)                                              &
     &         = e_fld(3)*b_field(inod,1) - e_fld(1)*b_field(inod,3)
          poynting(inod,3)                                              &
     &         = e_fld(1)*b_field(inod,2) - e_fld(2)*b_field(inod,1)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      e_fld(1) = coef_d * current(inod,1) - uxb(inod,1)
      e_fld(2) = coef_d * current(inod,2) - uxb(inod,2)
      e_fld(3) = coef_d * current(inod,3) - uxb(inod,3)
!
      poynting(inod,1)                                                  &
     &         = e_fld(2)*b_field(inod,3) - e_fld(3)*b_field(inod,2)
      poynting(inod,2)                                                  &
     &         = e_fld(3)*b_field(inod,1) - e_fld(1)*b_field(inod,3)
      poynting(inod,3)                                                  &
     &         = e_fld(1)*b_field(inod,2) - e_fld(2)*b_field(inod,1)
!
      end subroutine cal_pole_poynting_flux_smp
!
! -----------------------------------------------------------------------
!
      end module pole_poynting_flux_smp
