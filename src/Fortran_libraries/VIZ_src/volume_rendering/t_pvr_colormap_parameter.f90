!>@file  t_pvr_colormap_parameter.f90
!!       module t_pvr_colormap_parameter
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine alloc_pvr_color_parameteres(color)
!!      subroutine alloc_pvr_opacity_list(color)
!!      subroutine alloc_light_posi_in_view(color)
!!      subroutine dealloc_pvr_color_parameteres(color)
!!        type(pvr_colormap_parameter), intent(inout) :: color
!!@endverbatim
!
      module t_pvr_colormap_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>  Structure for PVR colormap parameters
      type pvr_colormap_parameter
!>    Colormap IDs
!!@n        pvr_colormap(:) =        id_pvr_color(1)
!!@n        pvr_data_mapping(:) =    id_pvr_color(2)
!!@n        opacity_style(:) =       find_dis_minmax(3)
        integer(kind = kint) :: id_pvr_color(3) = (/0,0,0/)
!
!>    Number of data points to define color
        integer(kind = kint) :: num_pvr_datamap_pnt = 0
!>    Data and corresponding color value
!!@n        Field data:  pvr_datamap_param(1,:)
!!@n        Color data:  pvr_datamap_param(2,:)
        real(kind = kreal), allocatable :: pvr_datamap_param(:,:)
!
!>    Number of data points to define color
        integer(kind = kint) :: num_opacity_pnt = 0
!>     Maximum opacity for colorbar
        real(kind = kreal) :: pvr_max_opacity = zero
!>     Opacity data table
!!@n        pvr_opacity_dat_low(:) =  pvr_opacity_param(1,:)
!!@n        pvr_opacity_dat_high(:) = pvr_opacity_param(2,:)
!!@n        pvr_opacity_opacity(:) =  pvr_opacity_param(3,:)
!!@n        ambient_opacity:  pvr_opacity_param(3,(num_opacity_pnt(:)+1))
        real(kind = kreal), allocatable :: pvr_opacity_param(:,:)
!
!>    Defined flag for lights
        integer(kind = kint) :: iflag_pvr_lights = 0
!>        Number of lights
        integer(kind = kint) :: num_pvr_lights = 0
!!@n        ambient_coef(:) =  pvr_lighting_real(1,:)
!!@n        diffuse_coef(:) =  pvr_lighting_real(2,:)
!!@n        specular_coef(:) = pvr_lighting_real(3,:)
        real(kind = kreal) :: pvr_lighting_real(3) = (/zero,zero,zero/)
!>    Position of lights
        real(kind = kreal), allocatable :: xyz_pvr_lights(:,:)
!
!>        Background color
        real(kind = kreal) :: bg_rgba_real(4) = (/0.0,0.0,0.0,0.0/)
      end type pvr_colormap_parameter
!
!>  Structure for PVR colorbar parameters
      type pvr_colorbar_parameter
!>    Draw flag for color bar
        logical :: flag_pvr_colorbar =  .FALSE.
!>    Bottom colorbar flag
        logical :: flag_pvr_cbar_bottom =  .FALSE.
!>    Draw flag for color bar numbers
        integer(kind = kint) :: iflag_pvr_cbar_nums = 0
!>    Draw flag for zero line in color bar
        integer(kind = kint) :: iflag_pvr_zero_mark = 0
!>    Flag of colorbar with opacity
        integer(kind = kint) :: iflag_opacity = 1
!>    Scaling for number font
        integer(kind = kint) :: iscale_font = 1
!>    Thicknsess of colorbar
        integer(kind = kint) :: ntick_pvr_colorbar =  3
!>    Range of colorbar
        real(kind = kreal) :: cbar_range(2) = (/zero,one/)
!
!>    Draw flag for axis label
        logical :: flag_pvr_axis = .FALSE.
!>    Draw flag for time label
        logical :: flag_draw_time = .FALSE.
!
!>    Draw flag for map grid
        logical :: flag_draw_mapgrid = .FALSE.
      end type pvr_colorbar_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_color_parameteres(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%pvr_datamap_param(2,color%num_pvr_datamap_pnt) )
      if(color%num_pvr_datamap_pnt .gt. 0) then
        color%pvr_datamap_param = 0.0d0
      end if
!
      end subroutine alloc_pvr_color_parameteres
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_opacity_list(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%pvr_opacity_param(3,color%num_opacity_pnt+1) )
      if(color%num_opacity_pnt .gt. 0) then
        color%pvr_opacity_param = 0.0d0
      end if
!
      end subroutine alloc_pvr_opacity_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_light_posi_in_view(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%xyz_pvr_lights(3,color%num_pvr_lights) )
      if (color%num_pvr_lights .le. 0) return
      color%xyz_pvr_lights =    0.0d0
!
      end subroutine alloc_light_posi_in_view
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_color_parameteres(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      deallocate(color%pvr_datamap_param)
      deallocate(color%pvr_opacity_param)
      deallocate(color%xyz_pvr_lights)
!
      end subroutine dealloc_pvr_color_parameteres
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_colormap_parameter
