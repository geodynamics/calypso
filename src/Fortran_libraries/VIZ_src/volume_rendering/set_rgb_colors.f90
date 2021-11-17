!
!      module set_rgb_colors
!
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
!      subroutine normalize_by_linear(dat_min, dat_max, value,          &
!     &          colordat)
!      subroutine normalize_by_linear_segment(num_point, datamap_param, &
!     &          value, colordat)
!
!      subroutine restore_linear_normalize(value_rgb,                   &
!     &          mincolor, maxcolor, value)
!      subroutine restore_segment_normalize(value_rgb,                  &
!     &          mincolor, maxcolor, num_point, datamap_param, value)
!
!      subroutine color_rainbow(rnorm, r, g, b)
!      subroutine color_redblue(rnorm, r, g, b)
!      subroutine color_grayscale(rnorm, r, g, b)
!      subroutine color_sym_grayscale(rnorm, r, g, b)
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
! ----------------------------------------------------------------------
!
      subroutine color_rainbow(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: purple = zero
      real(kind = kreal), parameter :: blue =   0.1e0
      real(kind = kreal), parameter :: ocean =  0.325e0
      real(kind = kreal), parameter :: green =  0.55e0
      real(kind = kreal), parameter :: yellow = 0.775e0
      real(kind = kreal), parameter :: red =    one
      real(kind = kreal), parameter :: forty =  four*ten
!
!
      if (rnorm .lt. purple ) then
        r = half
        g = zero
        b = one
      else if (rnorm .ge. purple .and. rnorm.lt.blue) then
        r = half - five*rnorm
        g = zero
        b = one
      else if (rnorm .ge. blue .and. rnorm.lt.ocean) then
        r = zero
        g = forty*(rnorm-blue) / dnine
        b = one
      else if (rnorm .ge. ocean .and. rnorm.lt.green) then
        r = zero
        g = one
        b = one - forty*(rnorm-ocean) / dnine
      else if (rnorm .ge. green .and. rnorm.lt.yellow) then
        r = forty*(rnorm-green) / dnine
        g = one
        b = zero
      else if (rnorm .ge. yellow .and. rnorm.lt. red) then
        r = one
        g = one - forty*(rnorm-yellow) / dnine
        b = zero
      else if (rnorm .ge. red ) then
        r = one
        g = zero
        b = zero
      end if
!
      end subroutine color_rainbow
!
! ----------------------------------------------------------------------
!
      subroutine color_redblue(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: abyss = zero
      real(kind = kreal), parameter :: blue =  0.2d0
      real(kind = kreal), parameter :: white = half
      real(kind = kreal), parameter :: red =   0.8d0
      real(kind = kreal), parameter :: blood =  one
!
!
      if (rnorm .lt. abyss ) then
        r = zero
        g = 0.2d0
        b = 0.5d0
      else if (rnorm .ge. abyss .and. rnorm.lt.blue) then
        r = zero
        g = blue - rnorm
        b = 0.5d0 + 2.5 * rnorm
      else if (rnorm .ge. blue .and. rnorm.lt.white) then
        r = (rnorm - blue) * 2.5
        g = (rnorm - blue) * 2.5
        b = one
      else if (rnorm .ge. white .and. rnorm.lt.red) then
        r = one
        g = (red - rnorm) * 2.5
        b = (red - rnorm) * 2.5
      else if (rnorm .ge. red .and. rnorm.lt. blood) then
        r = one - (rnorm - red) * 2.5
        g = zero
        b = zero
      else if (rnorm .ge. blood) then
        r = half
        g = zero
        b = zero
      end if
!
      end subroutine color_redblue
!
! ----------------------------------------------------------------------
!
      subroutine color_grayscale(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: black = zero
      real(kind = kreal), parameter :: white = one
!
!
      if (rnorm .lt. zero ) then
        r = zero
        g = zero
        b = zero
      else if (rnorm .ge. zero .and. rnorm.lt.white) then
        r = 0.85d0*rnorm
        g = 0.85d0*rnorm
        b = 0.85d0*rnorm
      else if (rnorm .ge. white ) then
        r = 0.85d0
        g = 0.85d0
        b = 0.85d0
      end if
!
      end subroutine color_grayscale
!
! ----------------------------------------------------------------------
!
      subroutine color_sym_grayscale(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: black = zero
      real(kind = kreal), parameter :: white = one
      real(kind = kreal), parameter :: half = one / two
!
!
      if (rnorm .lt. zero ) then
        r = zero
        g = zero
        b = zero
      else if (rnorm .ge. zero .and. rnorm.lt.half) then
        r = 0.85d0*two*rnorm
        g = 0.85d0*two*rnorm
        b = 0.85d0*two*rnorm
      else if (rnorm .ge. half .and. rnorm.lt.white) then
        r = 0.85d0*two*(one - rnorm)
        g = 0.85d0*two*(one - rnorm)
        b = 0.85d0*two*(one - rnorm)
      else if (rnorm .ge. white ) then
        r = zero
        g = zero
        b = zero
      end if
!
      end subroutine color_sym_grayscale
!
! ----------------------------------------------------------------------
!
      end module set_rgb_colors
