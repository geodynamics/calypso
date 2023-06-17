!>@file   t_map_projection.f90
!!@brief  module t_map_projection
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine alloc_map_rendering_module(map)
!!      subroutine alloc_map_rendering_module(map)
!!        type(map_rendering_module), intent(inout) :: map
!!@endverbatim
      module t_map_projection
!
      use calypso_mpi
      use m_precision
!
      use t_cross_section
      use t_psf_results
      use t_control_data_maps
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_pvr_image_array
      use t_map_rendering_data
!
      implicit  none
!
      type map_rendering_module
!>        Number of sections
        integer(kind = kint) :: num_map = 0
!
!>        Structure of case table for isosurface
        type(psf_cases) :: psf_case_tbls
!
!>        Structure for table for sections
        type(sectioning_list), allocatable :: map_list(:)
!>        Structure for table for sections
        type(grp_section_list), allocatable :: map_grp_list(:)
!
!>        Structure for search table for sections
        type(psf_search_lists), allocatable :: psf_search(:)
!
!>        Structure of sectioning module parameter
        type(psf_parameters), allocatable :: map_param(:)
!>        Structure of cross sectioning parameter
        type(section_define), allocatable  :: map_def(:)
!>        Structure of projection parameter
        type(pvr_view_parameter), allocatable:: view_param(:)
!>        Structure of color map parameter
        type(pvr_colormap_parameter), allocatable :: color_param(:)
!>        Structure of color bar parameter
        type(pvr_colorbar_parameter), allocatable :: cbar_param(:)
!
!>        Structure for psf patch data on local domain
        type(psf_local_data), allocatable :: map_mesh(:)
!
!>        Structure of color bar parameter
        type(psf_results), allocatable :: map_psf_dat(:)
!>        Structure of color bar parameter
        type(map_rendering_data), allocatable :: map_data(:)
!>        Structure of color bar parameter
        type(pvr_image_type), allocatable :: map_rgb(:)
      end type map_rendering_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_map_rendering_module(map)
!
      use m_field_file_format
!
      type(map_rendering_module), intent(inout) :: map
      integer(kind = kint) :: i_psf
!
!
      allocate(map%map_mesh(map%num_map))
      allocate(map%map_list(map%num_map))
      allocate(map%map_grp_list(map%num_map))
      allocate(map%psf_search(map%num_map))
      allocate(map%map_param(map%num_map))
      allocate(map%map_def(map%num_map))
!
      allocate(map%view_param(map%num_map))
      allocate(map%color_param(map%num_map))
      allocate(map%cbar_param(map%num_map))
      allocate(map%map_data(map%num_map))
      allocate(map%map_rgb(map%num_map))
      allocate(map%map_psf_dat(map%num_map))
!
      do i_psf = 1, map%num_map
        call alloc_coefficients_4_psf(map%map_def(i_psf))
      end do
!
      end subroutine alloc_map_rendering_module
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_map_rendering_module(map)
!
      use set_map_control
      use set_psf_control
      use set_fields_for_psf
      use find_node_and_patch_psf
!
      type(map_rendering_module), intent(inout) :: map
      integer(kind = kint) :: i_psf
!
!
      if(map%num_map .le. 0) return
!
      do i_psf = 1, map%num_map
        call dealloc_node_param_smp(map%map_mesh(i_psf)%node)
        call dealloc_ele_param_smp(map%map_mesh(i_psf)%patch)
!
        call dealloc_inod_grp_psf(map%map_grp_list(i_psf))
        call dealloc_coefficients_4_psf(map%map_def(i_psf))
        call dealloc_pvr_image_array(map%map_rgb(i_psf))
      end do
!
      call dealloc_psf_node_and_patch                                   &
    &    (map%num_map, map%map_list, map%map_mesh)
      call dealloc_psf_field_name(map%num_map, map%map_mesh)
      call dealloc_psf_field_data(map%num_map, map%map_mesh)
      call dealloc_psf_case_table(map%psf_case_tbls)
!
      deallocate(map%map_mesh, map%map_list, map%map_grp_list)
      deallocate(map%psf_search)
      deallocate(map%map_param)
      deallocate(map%map_rgb, map%map_data)
!
      end subroutine dealloc_map_rendering_module
!
!  ---------------------------------------------------------------------
!
      end module t_map_projection
