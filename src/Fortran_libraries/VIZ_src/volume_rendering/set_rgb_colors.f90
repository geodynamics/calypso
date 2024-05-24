!>@file   set_rgb_colors.F90
!!@brief  module set_rgb_colors
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2024
!
!>@brief  Normalization for color mapping
!!
!!@verbatim
!!      subroutine normalize_by_linear(dat_min, dat_max, value,         &
!!     &          colordat)
!!        real(kind = kreal), intent(in) :: value
!!        real(kind = kreal), intent(in) :: dat_min, dat_max
!!        real(kind = kreal), intent(out) :: colordat
!!      subroutine normalize_by_linear_segment(num_point, datamap_param,&
!!     &          value, colordat)
!!        real(kind = kreal), intent(in) :: value
!!        integer(kind = kint), intent(in) :: num_point
!!        real(kind = kreal), intent(in) :: datamap_param(2,num_point)
!!        real(kind = kreal), intent(out) :: colordat
!!
!!      subroutine restore_linear_normalize(value_rgb,                  &
!!     &          mincolor, maxcolor, value)
!!      subroutine restore_segment_normalize(value_rgb,                 &
!!     &          mincolor, maxcolor, num_point, datamap_param, value)
!!        real(kind = kreal), intent(in) :: value_rgb
!!        real(kind = kreal), intent(in) :: mincolor, maxcolor
!!        integer(kind = kint), intent(in) :: num_point
!!        real(kind = kreal), intent(in) :: datamap_param(2,num_point)
!!        real(kind = kreal), intent(out) :: value
!!@endverbatim
      module set_rgb_colors
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind = kreal), parameter :: EPSILON = 1.0d-11
      private :: EPSILON
!
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine normalize_by_linear(dat_min, dat_max, value,           &
     &          colordat)
!
      real(kind = kreal), intent(in) :: value
      real(kind = kreal), intent(in) :: dat_min, dat_max
!
      real(kind = kreal), intent(out) :: colordat
!
!
      if( abs(dat_max-dat_min) .gt. EPSILON) then
        if(value .lt. dat_min) then
          colordat = zero
        else if(value .gt. dat_max) then
          colordat = one
        else
          colordat = (value-dat_min) / (dat_max-dat_min)
        end if
      end if
!
      end subroutine normalize_by_linear
!
! ----------------------------------------------------------------------
!
      subroutine normalize_by_linear_segment(num_point, datamap_param,  &
     &          value, colordat)
!
      real(kind = kreal), intent(in) :: value
      integer(kind = kint), intent(in) :: num_point
      real(kind = kreal), intent(in) :: datamap_param(2,num_point)
!
      real(kind = kreal), intent(out) :: colordat
!
      integer(kind = kint) :: i
!
!
      if(value .lt. datamap_param(1,1)) then
        colordat = zero
      else if(value .gt. datamap_param(1,num_point)) then
        colordat = one
      else
        do i = 1, num_point-1
          if(value.ge.datamap_param(1,i)                                &
     &     .and. value.le.datamap_param(1,i+1)) then
            colordat =  datamap_param(2,i)                              &
     &                + (datamap_param(2,i+1)-datamap_param(2,i))       &
     &                 * (value - datamap_param(1,i))                   &
     &                / (datamap_param(1,i+1)-datamap_param(1,i))
            exit
          end if
        end do
      end if
!
      end subroutine normalize_by_linear_segment
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine restore_linear_normalize(value_rgb,                    &
     &          mincolor, maxcolor, value)
!
      real(kind = kreal), intent(in) :: value_rgb
      real(kind = kreal), intent(in) :: mincolor, maxcolor
!
      real(kind = kreal), intent(out) :: value
!
!
      if( abs(maxcolor-mincolor) .gt. EPSILON) then
         value = value_rgb * (maxcolor - mincolor) + mincolor
      end if
!
      end subroutine restore_linear_normalize
!
! ----------------------------------------------------------------------
!
      subroutine restore_segment_normalize(value_rgb,                   &
     &          mincolor, maxcolor, num_point, datamap_param, value)
!
      real(kind = kreal), intent(in) :: value_rgb
      real(kind = kreal), intent(in) :: mincolor, maxcolor
      integer(kind = kint), intent(in) :: num_point
      real(kind = kreal), intent(in) :: datamap_param(2,num_point)
!
      real(kind = kreal), intent(out) :: value
!
      integer(kind = kint) :: i
!
!
      if(value_rgb .lt. datamap_param(2,1)) then
         value = mincolor
      else if(value_rgb .gt. datamap_param(2,num_point)) then
         value = maxcolor
      else
        do i = 1, num_point
          if(value .ge. datamap_param(2,i)                              &
     &         .and. value .le. datamap_param(2,i+1)) then
            value = datamap_param(1,i)                                  &
     &             + (datamap_param(1,i+1) - datamap_param(1,i))        &
     &              * (value_rgb - datamap_param(2,i))                  &
     &              / (datamap_param(2,i+1) - datamap_param(2,i))
          end if
        end do
      end if
!
      end subroutine restore_segment_normalize
!
! ----------------------------------------------------------------------
!
      end module set_rgb_colors
