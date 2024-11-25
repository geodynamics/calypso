!>@file   set_control_each_fline.f90
!!@brief  module set_control_each_fline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Set seed points from tracers
!!
!!@verbatim
!!      subroutine count_control_4_fline(fln, ele_grp, sf_grp, fln_prm)
!!      subroutine set_control_4_fline(fln, ele_grp, nod_fld, fln_prm)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fline_ctl), intent(inout) :: fln
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine set_fline_ctl_4_tracer_seed(num_tracer, tracer_prm,  &
!!     &                                       fln, fln_prm)
!!        integer(kind = kint), intent(in) :: num_tracer
!!        type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
!!        type(fline_ctl), intent(in) :: fln
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!@endverbatim
!
      module set_control_each_fline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use t_control_params_4_fline
      use t_ctl_data_field_line
      use t_geometry_data
      use t_group_data
!
      use set_area_4_viz
      use set_field_comp_for_viz
      use set_fields_for_fieldline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_fline(fln, ele_grp, sf_grp,            &
     &                                 fln_prm, ierr)
!
      use m_field_file_format
      use m_control_fline_flags
!
      use t_source_of_filed_line
      use set_control_platform_data
      use set_isosurface_file_ctl
      use set_area_4_viz
      use skip_comment_f
      use delete_data_files
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!
      type(fline_ctl), intent(in) :: fln
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: character_256
!
!
      ierr = 0
      if(fln%fline_file_head_ctl%iflag .gt. 0) then
        fln_prm%fline_file_IO%file_prefix                               &
     &         =  fln%fline_file_head_ctl%charavalue
      else
        fln_prm%fline_file_IO%file_prefix =  'field_line'
      end if
!
      call calypso_mpi_barrier
      if(check_file_writable(my_rank,                                   &
     &                       fln_prm%fline_file_IO%file_prefix)         &
     &                                             .eqv. .FALSE.) then
        ierr = ierr_VIZ
        return
      end if
!
      if(fln%fline_output_type_ctl%iflag .eq. 0) then
        fln_prm%fline_file_IO%iflag_format = iflag_sgl_vtk
      else
        fln_prm%fline_file_IO%iflag_format                              &
     &      = sel_iso_file_format(fln%fline_output_type_ctl%charavalue)
      end if
!
      call set_ctl_parallel_file_w_def(default_tracer_prefix,           &
     &    fln%fline_rst_prefix_ctl, fln%fline_rst_format_ctl,           &
     &    fln_prm%fline_rst_IO)
!
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    fln_prm%nele_grp_area_fline)
!
      if(fln_prm%nele_grp_area_fline .eq. 0)                            &
     &  call calypso_MPI_abort(ierr_mesh, 'set correct element group')
!
!
      fln_prm%flag_use_broadcast = .TRUE.
      character_256 = fln%fline_comm_mode_ctl%charavalue
      if     (cmp_no_case(character_256, cflag_send_recv)) then
        fln_prm%flag_use_broadcast = .FALSE.
      end if
!
      character_256 = fln%starting_type_ctl%charavalue
      if     (cmp_no_case(character_256, cflag_surface_group)) then
        fln_prm%id_fline_seed_type =  iflag_surface_group
      else if(cmp_no_case(character_256, cflag_surface_list)) then 
        fln_prm%id_fline_seed_type =  iflag_surface_list
      else if(cmp_no_case(character_256, cflag_position_list)) then 
        fln_prm%id_fline_seed_type =  iflag_position_list
      else if(cmp_no_case(character_256, cflag_spray_in_domain)) then 
        fln_prm%id_fline_seed_type =  iflag_spray_in_domain
      else if(cmp_no_case(character_256, cflag_seed_from_tracer)) then 
        fln_prm%id_fline_seed_type =  iflag_tracer_seeds
      end if
!
!
      character_256 = fln%line_direction_ctl%charavalue
      if     (cmp_no_case(character_256, cflag_forward_trace)) then
        fln_prm%id_fline_direction =  iflag_forward_trace
      else if(cmp_no_case(character_256, cflag_backward_trace)) then 
        fln_prm%id_fline_direction =  iflag_backward_trace
      else if(cmp_no_case(character_256, cflag_both_trace)) then 
        fln_prm%id_fline_direction =  iflag_both_trace
      end if
!
!
      if     (fln_prm%id_fline_seed_type .eq. iflag_surface_group       &
     &   .or. fln_prm%id_fline_seed_type .eq. iflag_spray_in_domain)    &
     &                                                            then
        fln_prm%id_seed_distribution =  iflag_random_by_amp
        character_256 = fln%selection_type_ctl%charavalue
        if     (cmp_no_case(character_256, cflag_random_by_amp)) then
          fln_prm%id_seed_distribution =  iflag_random_by_amp
        else if(cmp_no_case(character_256, cflag_random_by_area)) then
          fln_prm%id_seed_distribution =  iflag_random_by_area
        else if(cmp_no_case(character_256, cflag_no_random)) then 
          fln_prm%id_seed_distribution =  iflag_no_random
        end if
      end if
!
      fln_prm%max_line_stepping = -1
      if(fln%max_line_stepping_ctl%iflag .gt. 0) then
          fln_prm%max_line_stepping                                     &
     &           = fln%max_line_stepping_ctl%intvalue
      end if
!
      fln_prm%max_trace_length = -1.0
      if(fln%max_trace_length_ctl%iflag .gt. 0) then
          fln_prm%max_trace_length                                      &
     &           = fln%max_trace_length_ctl%realvalue
      end if
!
      if(    fln_prm%id_fline_seed_type .eq. iflag_surface_group        &
     &  .or. fln_prm%id_fline_seed_type .eq. iflag_spray_in_domain)     &
     &      then
        if(fln%num_fieldline_ctl%iflag .gt. 0) then
          fln_prm%num_each_field_line = fln%num_fieldline_ctl%intvalue
        else
          fln_prm%num_each_field_line = 8
        end if
!
        if(fln%seed_surf_grp_ctl%iflag .gt. 0) then
          fln_prm%igrp_start_fline_surf_grp                             &
     &      = set_surf_grp_id_4_viz(sf_grp%num_grp, sf_grp%grp_name,    &
     &                              fln%seed_surf_grp_ctl%charavalue)
        end if
!
        if(fln%seed_ele_grp_ctl%iflag .gt. 0) then
          fln_prm%igrp_start_fline_ele_grp                              &
     &      = set_surf_grp_id_4_viz(ele_grp%num_grp, ele_grp%grp_name,  &
     &                              fln%seed_ele_grp_ctl%charavalue)
        end if
      end if
!
      end subroutine count_control_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_fline(fln, ele_grp, nod_fld, fln_prm)
!
      use t_source_of_filed_line
      use set_components_flags
      use set_area_4_viz
      use coordinate_converter
!
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fline_ctl), intent(inout) :: fln
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: ncomp(1), ncomp_org(1)
      integer(kind = kint) :: ifield_tmp(1), icomp_tmp(1)
      character(len=kchara) :: tmpfield(1)
      character(len=kchara) :: tmpcomp(1)
      character(len=kchara) :: tmpchara(1)
!
!
      tmpfield(1) = fln%fline_field_ctl%charavalue
      tmpcomp(1) =  'vector'
      call set_components_4_viz                                         &
     &   (nod_fld%num_phys, nod_fld%phys_name, ione, tmpfield, tmpcomp, &
     &    ione, ifield_tmp, icomp_tmp, ncomp, ncomp_org, tmpchara)
!
      if(icomp_tmp(1) .ne. icomp_VECTOR) then
        call calypso_MPI_abort(ierr_fld,                                &
     &      'Choose vector field for field line')
      end if
      fln_prm%iphys_4_fline                                             &
     &       = nod_fld%istack_component(ifield_tmp(1)-1) + 1
!
!
      tmpfield(1) = fln%seed_ref_field_ctl%charavalue
      tmpcomp(1) =  fln%seed_ref_comp_ctl%charavalue
      call set_components_4_viz                                         &
     &   (nod_fld%num_phys, nod_fld%phys_name, ione, tmpfield, tmpcomp, &
     &    ione, ifield_tmp, icomp_tmp, ncomp, ncomp_org, tmpchara)
      fln_prm%ifield_4_density = ifield_tmp(1)
      fln_prm%icomp_4_density =  icomp_tmp(1)
!
      call set_ctl_params_viz_fields(fln%fline_field_output_ctl,        &
     &                               nod_fld, fln_prm%fline_fields)
!
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline)
!
      end subroutine set_control_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_ctl_4_tracer_seed(num_tracer, tracer_prm,    &
     &                                       fln, fln_prm)
!
      use m_control_fline_flags
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_tracer
      type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
      type(fline_ctl), intent(in) :: fln
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: i
      character(len=kchara) :: tracer_prefix, character_256
!
!
      fln_prm%id_tracer_for_seed = 0
      if(fln%seed_file_prefix_ctl%iflag .gt. 0) then
        tracer_prefix = fln%seed_file_prefix_ctl%charavalue
        do i = 1, num_tracer
          character_256 = tracer_prm(i)%fline_file_IO%file_prefix
          if(cmp_no_case(tracer_prefix, character_256)) then
            fln_prm%id_tracer_for_seed = i
            exit
          end if
        end do
      end if
!
      if(fln_prm%id_fline_seed_type .eq. iflag_tracer_seeds             &
     &                .and. fln_prm%id_tracer_for_seed .le. 0) then
        call calypso_MPI_abort(ierr_mesh,                               &
     &     'set correct tracer file prefix for seeds')
      end if
!
      end subroutine set_fline_ctl_4_tracer_seed
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_fline
