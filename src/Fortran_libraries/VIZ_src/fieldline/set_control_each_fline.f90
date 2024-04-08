!set_control_each_fline.f90
!      module set_control_each_fline
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine count_control_4_fline                                &
!!     &         (fln, ele, ele_grp, sf_grp, fln_prm, fln_src)
!!      subroutine set_control_4_fline                                  &
!!     &         (fln, ele, ele_grp, sf_grp, nod_fld, fln_prm, fln_src)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fline_ctl), intent(inout) :: fln
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
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
      subroutine count_control_4_fline                                  &
     &         (fln, ele, ele_grp, sf_grp, fln_prm, fln_src)
!
      use m_field_file_format
      use m_control_fline_flags
!
      use t_source_of_filed_line
      use set_area_4_viz
      use skip_comment_f
      use delete_data_files
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!
      type(fline_ctl), intent(in) :: fln
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      character(len=kchara) :: character_256
!
!
      if(fln%fline_file_head_ctl%iflag .gt. 0) then
        fln_prm%fline_prefix =  fln%fline_file_head_ctl%charavalue
      else
        fln_prm%fline_prefix =  'field_line'
      end if
!
      call calypso_mpi_barrier
      if(check_file_writable(my_rank, fln_prm%fline_prefix)             &
     &                                             .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_VIZ,                                &
     &                         'Check Directory for Fieldline output')
      end if
!
      character_256 = fln%fline_output_type_ctl%charavalue
      if     (cmp_no_case(character_256, 'ucd')) then
        fln_prm%iformat_file_file = iflag_ucd
      else if(cmp_no_case(character_256, 'vtk')) then
        fln_prm%iformat_file_file = iflag_vtk
      else
        fln_prm%iformat_file_file = iflag_vtk
      end if
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    fln_prm%nele_grp_area_fline)
!
      if(fln_prm%nele_grp_area_fline .eq. 0)                            &
     &  call calypso_MPI_abort(ierr_mesh, 'set correct element group')
!
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
      if     (fln_prm%id_fline_seed_type .eq. iflag_surface_group) then
        fln_prm%id_seed_distribution =  iflag_random_by_amp
        character_256 = fln%selection_type_ctl%charavalue
        if     (cmp_no_case(character_256, cflag_random_by_amp)) then
          fln_prm%id_seed_distribution =  iflag_random_by_amp
        else if(cmp_no_case(character_256, cflag_random_by_area)) then
          fln_prm%id_seed_distribution =  iflag_random_by_area
        else if(cmp_no_case(character_256, cflag_no_random)) then 
          fln_prm%id_seed_distribution =  iflag_no_random
        end if
      else if(fln_prm%id_fline_seed_type                                &
     &                          .eq. iflag_spray_in_domain) then
        fln_prm%id_seed_distribution =  iflag_no_random
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
        fln_prm%max_line_stepping = 1000
        if(fln%max_line_stepping_ctl%iflag .gt. 0) then
          fln_prm%max_line_stepping                                     &
     &           = fln%max_line_stepping_ctl%intvalue
        end if
!
        if(fln%start_surf_grp_ctl%iflag .gt. 0) then
          call set_surf_grp_id_4_viz(sf_grp%num_grp, sf_grp%grp_name,   &
     &        fln%start_surf_grp_ctl%charavalue,                        &
     &        fln_prm%igrp_start_fline_surf_grp)
        end if
!
        fln_src%nele_start_grp = count_nsurf_for_starting(ele, sf_grp,  &
     &                          fln_prm%igrp_start_fline_surf_grp)
!
      else if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        if(fln%seed_surface_ctl%num .gt. 0) then
          fln_prm%num_each_field_line = fln%seed_surface_ctl%num
        end if
      else if(fln_prm%id_fline_seed_type .eq. iflag_position_list) then
        if(fln%seed_point_ctl%num .gt. 0) then
          fln_prm%num_each_field_line = fln%seed_point_ctl%num
        end if
      end if
!
      end subroutine count_control_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_fline                                    &
     &         (fln, ele, ele_grp, sf_grp, nod_fld, fln_prm, fln_src)
!
      use t_source_of_filed_line
      use set_components_flags
      use set_area_4_viz
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fline_ctl), intent(inout) :: fln
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: i, ncomp(1), ncomp_org(1)
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
      fln_prm%ifield_4_fline = ifield_tmp(1)
      fln_prm%icomp_4_fline =  icomp_tmp(1)
!
      if(fln_prm%icomp_4_fline .ne. icomp_VECTOR) then
        call calypso_MPI_abort(ierr_fld,                                &
     &      'Choose vector field for field line')
      end if
!
      tmpfield(1) = fln%fline_color_field_ctl%charavalue
      tmpcomp(1) = fln%fline_color_comp_ctl%charavalue
      call set_components_4_viz                                         &
     &   (nod_fld%num_phys, nod_fld%phys_name,                          &
     &    ione, tmpfield, tmpcomp, ione, ifield_tmp, icomp_tmp,         &
     &    ncomp, ncomp_org, tmpchara)
      fln_prm%ifield_linecolor =   ifield_tmp(1)
      fln_prm%icomp_linecolor =    icomp_tmp(1)
      fln_prm%name_color_output =  tmpchara(1)
!
      if(ncomp(1) .ne. ione) then
        call calypso_MPI_abort(ierr_fld,'field color should be scalar')
      end if
!
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline)
!
!
      if(fln_prm%id_fline_seed_type .eq. iflag_surface_group) then
        call set_isurf_for_starting                                     &
     &     (ele, sf_grp, fln_prm%igrp_start_fline_surf_grp, fln_src)
      else if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        do i = 1, fln_prm%num_each_field_line
          fln_prm%id_gl_surf_start_fline(1,i)                           &
     &          = fln%seed_surface_ctl%int1(i)
          fln_prm%id_gl_surf_start_fline(2,i)                           &
     &          = fln%seed_surface_ctl%int2(i)
        end do
      else if(fln_prm%id_fline_seed_type .eq. iflag_position_list) then
        do i = 1, fln_prm%num_each_field_line
          fln_prm%xx_surf_start_fline(1,i) = fln%seed_point_ctl%vec1(i)
          fln_prm%xx_surf_start_fline(2,i) = fln%seed_point_ctl%vec2(i)
          fln_prm%xx_surf_start_fline(3,i) = fln%seed_point_ctl%vec3(i)
        end do
      end if
!
      end subroutine set_control_4_fline
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_fline
