!>@file  set_color_4_pvr.f90
!!       module set_color_4_pvr
!!
!!@author H. Matsui
!!@date   Programmed in July. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine normalize_by_color(value, id_colormap_style,        &
!!     &          num_point, datamap_param, colordat)
!!      subroutine restore_from_normalize(value_rgb, id_colormap_style,&
!!     &          mincolor, maxcolor, num_point, datamap_param, value)
!!
!!      subroutine normvalue_to_rgb(id_color_system, colordat, color)
!!
!!      subroutine value_to_rgb(id_colormap_style, id_color_system,    &
!!     &          num_interval_map, interval_point, value, color)
!!@endverbatim
!
      module set_color_4_pvr
!
      use m_precision
!
      use set_rgb_colors
!
      implicit  none
!
      character(len = kchara), parameter :: hd_rainbow =   'rainbow'
      character(len = kchara), parameter :: hd_grayscale = 'grayscale'
      character(len = kchara), parameter                                &
     &                        :: hd_radblue =  'blue_to_red'
      character(len = kchara), parameter                                &
     &                        :: hd_sym_gray = 'symmetric_grayscale'
      integer(kind = kint), parameter :: iflag_redblue =    3
      integer(kind = kint), parameter :: iflag_rainbow =    1
      integer(kind = kint), parameter :: iflag_grayscale =  2
      integer(kind = kint), parameter :: iflag_sym_gray =   4
!
      character(len = kchara), parameter :: hd_minmax =    'minmax'
      character(len = kchara), parameter :: hd_linear =    'linear'
      character(len = kchara), parameter :: hd_nonlinear = 'nonlinear'
      character(len = kchara), parameter                                &
     &                        :: hd_colorlist = 'colormap_list'
      integer(kind = kint), parameter :: iflag_automatic =  1
      integer(kind = kint), parameter :: iflag_minmax =     2
      integer(kind = kint), parameter :: iflag_colorlist =  3
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine normalize_by_color(value, id_colormap_style,           &
     &          num_point, datamap_param, colordat)
!
      integer(kind = kint), intent(in) :: id_colormap_style
      real(kind = kreal), intent(in) :: value
      integer(kind = kint), intent(in) :: num_point
      real(kind = kreal), intent(in) :: datamap_param(2,num_point)
!
      real(kind = kreal), intent(out) :: colordat
!
!
      if(id_colormap_style .eq. iflag_colorlist) then
        call normalize_by_linear_segment(num_point, datamap_param,      &
     &      value, colordat)
      else
        call normalize_by_linear(datamap_param(1,1),                    &
     &      datamap_param(1,2), value, colordat)
      end if
!
      end subroutine normalize_by_color
!
! ----------------------------------------------------------------------
!
      subroutine restore_from_normalize(value_rgb, id_colormap_style,   &
     &          mincolor, maxcolor, num_point, datamap_param, value)
!
      integer(kind = kint), intent(in) :: id_colormap_style
      real(kind = kreal), intent(in) :: value_rgb
      real(kind = kreal), intent(in) :: mincolor, maxcolor
      integer(kind = kint), intent(in) :: num_point
      real(kind = kreal), intent(in) :: datamap_param(2,num_point)
!
      real(kind = kreal), intent(out) :: value
!
!
      if(id_colormap_style .eq. iflag_colorlist) then
        call restore_segment_normalize(value_rgb,                       &
     &          mincolor, maxcolor, num_point, datamap_param, value)
      else
        call restore_linear_normalize(value_rgb,  mincolor, maxcolor,   &
     &       value)
      end if
!
      end subroutine restore_from_normalize
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine normvalue_to_rgb(id_color_system, colordat, color)
!
      use set_rgb_colors
!
      integer(kind = kint), intent(in) :: id_color_system
      real(kind = kreal), intent(in) :: colordat
!
      real(kind = kreal), intent(out) :: color(3)
!
!
      if(id_color_system .eq. iflag_redblue) then
        call color_redblue(colordat, color(1), color(2), color(3))
      else if(id_color_system .eq. iflag_sym_gray) then
        call color_sym_grayscale                                        &
     &     (colordat, color(1), color(2), color(3))
      else if(id_color_system .eq. iflag_grayscale) then
        call color_grayscale(colordat, color(1), color(2), color(3))
      else
        call color_rainbow(colordat, color(1), color(2), color(3))
      end if
!
      end subroutine normvalue_to_rgb
!
! ----------------------------------------------------------------------
!
      subroutine value_to_rgb(id_colormap_style, id_color_system,       &
     &          num_interval_map, interval_point, value, color)
!
      use set_rgb_colors
!
      integer(kind = kint), intent(in) :: id_colormap_style
      integer(kind = kint), intent(in) :: id_color_system
      integer(kind = kint), intent(in) :: num_interval_map
      real(kind = kreal), intent(in)                                    &
     &                   :: interval_point(2,num_interval_map)
      real(kind = kreal), intent(in) :: value
!
      real(kind = kreal), intent(inout) :: color(3)
!
      real(kind = kreal) ::  colordat
!
!
      call normalize_by_color(value, id_colormap_style,                 &
     &    num_interval_map, interval_point, colordat)
      call normvalue_to_rgb(id_color_system, colordat, color)
!
      end subroutine value_to_rgb
!
! ----------------------------------------------------------------------
!
      end module set_color_4_pvr
