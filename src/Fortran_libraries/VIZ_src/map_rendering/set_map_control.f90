!>@file   set_map_control.f90
!!@brief  module set_map_control
!!
!!@author H. Matsui
!!@date Programmed in May., 2006
!!@n    Modified in  June, 1015
!
!>@brief Structure for parallel sectioned data
!!
!!@verbatim
!!      subroutine s_set_map_control(num_map, group, nod_fld, map_ctls, &
!!     &          psf_param, psf_def, psf_mesh, view_param, color_param,&
!!     &          cbar_param, map_data, map_rgb)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(psf_parameters), intent(inout) :: psf_param(num_map)
!!        type(section_define), intent(inout) :: psf_def(num_map)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_map)
!!        type(pvr_view_parameter), intent(inout) :: view_param(num_map)
!!        type(pvr_colormap_parameter), intent(inout)                   &
!!     &                             :: color_param(num_map)
!!        type(pvr_colorbar_parameter), intent(inout)                   &
!!     &                             :: cbar_param(num_map)
!!        type(map_rendering_data), intent(inout) :: map_data(num_map)
!!        type(pvr_image_type), intent(inout) :: map_rgb(num_map)
!!@endverbatim
!
      module set_map_control
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_control_data_maps
      use t_control_data_4_map
      use t_control_params_4_psf
      use t_psf_patch_data
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_map_rendering_data
      use t_pvr_image_array
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: default_map_prefix = 'map'
!
      private :: count_control_4_map, set_control_4_map
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_map_control(num_map, group, nod_fld, map_ctls,   &
     &          psf_param, psf_def, psf_mesh, view_param, color_param,  &
     &          cbar_param, map_data, map_rgb)
!
      use calypso_mpi
      use t_read_control_elements
      use t_psf_patch_data
!
      use set_field_comp_for_viz
      use mpi_abort_by_missing_zlib
!
      use set_psf_control
!
      integer(kind= kint), intent(in) :: num_map
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(psf_parameters), intent(inout) :: psf_param(num_map)
      type(section_define), intent(inout) :: psf_def(num_map)
      type(psf_local_data), intent(inout) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(inout) :: view_param(num_map)
      type(pvr_colormap_parameter), intent(inout)                       &
     &                             :: color_param(num_map)
      type(pvr_colorbar_parameter), intent(inout)                       &
     &                             :: cbar_param(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: map_rgb(num_map)
!
      integer(kind = kint) :: i, ierr
!
!
      do i = 1, num_map
        call count_control_4_map(my_rank, map_ctls%map_ctl_struct(i),   &
     &      group%ele_grp, psf_param(i), map_rgb(i), ierr)
!
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
        call mpi_abort_by_no_zlib_in_fld(map_rgb(i)%pvr_prefix,         &
     &                                   map_rgb(i)%id_pvr_file_type)
      end do
!
      do i = 1, num_map
        psf_mesh(i)%field%num_phys = 2
        call alloc_phys_name(psf_mesh(i)%field)
        call set_control_4_map                                          &
     &     (map_ctls%map_ctl_struct(i), group%ele_grp, group%surf_grp,  &
     &      nod_fld%num_phys, nod_fld%phys_name, psf_mesh(i)%field,     &
     &      psf_param(i), psf_def(i), view_param(i), color_param(i),    &
     &      cbar_param(i), map_data(i), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
!
        call dealloc_cont_dat_4_map(map_ctls%map_ctl_struct(i))
!
        call count_total_comps_4_viz(psf_mesh(i)%field)
      end do
!
      call dealloc_map_ctl_stract(map_ctls)
!
      end subroutine s_set_map_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_control_4_map(id_rank, map_c, ele_grp,           &
     &                               psf_param, map_rgb, ierr)
!
      use m_error_IDs
      use m_file_format_switch
      use set_area_4_viz
      use set_sections_file_ctl
      use set_field_comp_for_viz
      use set_sections_file_ctl
      use delete_data_files
!
      type(group_data), intent(in) :: ele_grp
!
      integer, intent(in) :: id_rank
!
      type(map_ctl), intent(in) :: map_c
      type(psf_parameters), intent(inout) :: psf_param
      type(pvr_image_type), intent(inout) :: map_rgb
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      call set_image_file_control(map_c%map_image_fmt_ctl,              &
     &                            map_rgb%id_pvr_file_type)
!
      if(map_c%map_image_prefix_ctl%iflag .le. 0) then
        map_rgb%pvr_prefix = default_map_prefix
      else
        map_rgb%pvr_prefix = map_c%map_image_prefix_ctl%charavalue
      end if
!
      if(check_file_writable(id_rank, map_rgb%pvr_prefix)               &
     &                                             .eqv. .FALSE.) then
        ierr = ierr_VIZ
        return
      end if
!
      call count_control_4_psf_define                                   &
     &   (map_c%map_define_ctl%psf_def_c, ele_grp, psf_param, ierr)
!
      end subroutine count_control_4_map
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_map(map_c, ele_grp, sf_grp,              &
     &          num_nod_phys, phys_nod_name, psf_fld, psf_param,        &
     &          psf_def, view_param, color_param, cbar_param,           &
     &          map_data, ierr)
!
      use calypso_mpi
      use m_error_IDs
      use skip_comment_f
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
      use set_control_pvr_color
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(map_ctl), intent(in) :: map_c
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      type(section_define), intent(inout) :: psf_def
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_colormap_parameter), intent(inout) :: color_param
      type(pvr_colorbar_parameter), intent(inout) :: cbar_param
      type(map_rendering_data), intent(inout) :: map_data
      integer(kind = kint), intent(inout) :: ierr
!
!
      call alloc_area_group_psf(psf_param)
      call set_control_psf_define(map_c%map_define_ctl%psf_def_c,       &
     &    ele_grp, sf_grp, psf_param, psf_def, ierr)
!
      if(ierr .gt. 0) call calypso_MPI_abort(ierr_VIZ,                  &
     &                                      'Check surface parameter')
!
      call alloc_output_comps_psf(itwo, psf_param)
      map_data%fill_flag = .FALSE.
      if((map_c%map_field_ctl%iflag*map_c%map_comp_ctl%iflag)           &
     &                                                   .gt. 0) then
        map_data%fill_flag = .TRUE.
        call set_one_component_4_viz(num_nod_phys, phys_nod_name,       &
     &     map_c%map_field_ctl%charavalue,                              &
     &     map_c%map_comp_ctl%charavalue,                               &
     &     psf_param%id_output(1), psf_param%icomp_output(1),           &
     &     psf_fld%num_component(1), psf_param%ncomp_org(1),            &
     &     psf_fld%phys_name(1))
        if(psf_fld%num_component(1) .gt. 1)                             &
     &     call calypso_MPI_abort(ierr_VIZ, 'set scalar for rendering')
      end if
!
      if((map_c%isoline_field_ctl%iflag*map_c%isoline_comp_ctl%iflag)   &
     &                                                     .gt. 0) then
        call set_one_component_4_viz(num_nod_phys, phys_nod_name,       &
     &     map_c%isoline_field_ctl%charavalue,                          &
     &     map_c%isoline_comp_ctl%charavalue,                           &
     &     psf_param%id_output(2), psf_param%icomp_output(2),           &
     &     psf_fld%num_component(2), psf_param%ncomp_org(2),            &
     &     psf_fld%phys_name(2))
        if(psf_fld%num_component(2) .gt. 1)                             &
     &     call calypso_MPI_abort(ierr_VIZ, 'set scalar for isolines')
      end if
!
      if(psf_param%id_output(1) .le. 0                                  &
     &          .and. psf_param%id_output(2) .le. 0) then
        call calypso_MPI_abort(ierr_VIZ,                                &
     &      'set either field for rendering or isolines')
      else if(psf_param%id_output(1) .gt. 0                             &
     &          .and. psf_param%id_output(2) .le. 0) then
        psf_param%id_output(2) =     psf_param%id_output(1)
        psf_param%icomp_output(2) =  psf_param%icomp_output(1)
        psf_param%ncomp_org(2) =     psf_param%ncomp_org(1)
        psf_fld%num_component(2) =   psf_fld%num_component(1)
        psf_fld%phys_name(2) =       psf_fld%phys_name(1)
      else if(psf_param%id_output(2) .gt. 0                             &
     &          .and. psf_param%id_output(1) .le. 0) then
        psf_param%id_output(1) =     psf_param%id_output(2)
        psf_param%icomp_output(1) =  psf_param%icomp_output(2)
        psf_param%ncomp_org(1) =     psf_param%ncomp_org(2)
        psf_fld%num_component(1) =   psf_fld%num_component(2)
        psf_fld%phys_name(1) =       psf_fld%phys_name(2)
      end if
!
      call copy_pvr_image_size(map_c%mat%pixel, view_param)
      call copy_pvr_perspective_matrix(map_c%mat%proj, view_param)
!
      call set_control_pvr_colormap(map_c%cmap_cbar_c%color,            &
     &                              color_param)
      call set_control_pvr_colorbar(map_c%cmap_cbar_c%cbar_ctl,         &
     &                              cbar_param)
      cbar_param%iflag_opacity = 0
!
      call set_ctl_map_rendering_param                                  &
     &   (map_c%mat%projection_type_ctl, map_c%mat%proj,                &
     &    map_c%map_define_ctl, map_data)
!
      end subroutine set_control_4_map
!
!  ---------------------------------------------------------------------
!
      subroutine set_image_file_control(file_fmt_ctl, id_pvr_file_type)
!
      use skip_comment_f
      use t_control_array_character
      use output_image_sel_4_png
!
      type(read_character_item), intent(in) :: file_fmt_ctl
      integer(kind = kint), intent(inout) :: id_pvr_file_type
!
      character(len = kchara) :: tmpchara
!
!
      tmpchara = file_fmt_ctl%charavalue
      if(cmp_no_case(tmpchara, hd_PNG)) then
        id_pvr_file_type = iflag_PNG
      else if(cmp_no_case(tmpchara, hd_BMP)) then
        id_pvr_file_type = iflag_BMP
      else
        id_pvr_file_type = iflag_BMP
      end if
!
      end subroutine set_image_file_control
!
!  ---------------------------------------------------------------------
!
      end module set_map_control
