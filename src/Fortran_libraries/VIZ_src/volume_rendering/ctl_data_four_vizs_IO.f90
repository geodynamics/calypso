!>@file   ctl_data_four_vizs_IO.f90
!!@brief  module ctl_data_four_vizs_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine s_read_viz4_controls(id_control, viz_ctls, c_buf)
!!       integer(kind = kint), intent(in) :: id_control
!!       character(len=kchara), intent(in) :: hd_block
!!       type(vis4_controls), intent(inout) :: viz_ctls
!!       type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_viz4_controls                                  &
!!     &         (id_control, hd_block, viz_ctls, level)
!!       integer(kind = kint), intent(in) :: id_control
!!       character(len=kchara), intent(in) :: hd_block
!!       type(vis4_controls), intent(in) :: viz_ctls
!!       integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    array  cross_section_ctl
!!      ....
!!    end array cross_section_ctl
!!
!!    array  isosurface_ctl
!!      ....
!!    end array isosurface_ctl
!!
!!    array  map_rendering_ctl
!!      ....
!!    end array map_rendering_ctl
!!
!!    array  volume_rendering
!!      ....
!!    end array volume_rendering
!!
!!    array  fieldline
!!      ....
!!    end array fieldline
!!  end  visual_control
!
!!    delta_t_sectioning_ctl       1.0e-3
!!    i_step_sectioning_ctl        400
!!    delta_t_isosurface_ctl       1.0e-3
!!    i_step_isosurface_ctl        400
!!    delta_t_map_projection_ctl   1.0e-3
!!    i_step_map_projection_ctl    400
!!    delta_t_pvr_ctl              1.0e-2
!!    i_step_pvr_ctl               400
!!    delta_t_fline_ctl            1.0e-1
!!    i_step_fline_ctl             400
!!    delta_t_field_ctl            1.0e-3
!!    i_step_field_ctl             800
!!    output_field_file_fmt_ctl   'VTK'
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module ctl_data_four_vizs_IO
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_viz4
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_data_pvrs
      use t_control_data_flines
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!     Top level
      character(len=kchara), parameter, private                         &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_map_rendering = 'map_rendering_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_fline_ctl =  'fieldline'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_section =        'i_step_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_isosurf =        'i_step_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_map_projection = 'i_step_map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_pvr =            'i_step_pvr_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_lic =            'i_step_LIC_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_fline =          'i_step_fline_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_ucd =       'i_step_field_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_section =        'delta_t_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_isosurf =        'delta_t_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_map_projection = 'delta_t_map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_pvr =           'delta_t_pvr_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_lic =           'delta_t_LIC_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_fline =         'delta_t_fline_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_ucd =       'delta_t_field_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_output_fld_file_fmt = 'output_field_file_fmt_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_viz4_controls                                   &
     &         (id_control, hd_block, viz_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use ctl_file_map_renderings_IO
      use ctl_file_fieldlines_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(vis4_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(viz_ctls%i_viz_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_files_4_psf_ctl(id_control, hd_section_ctl,           &
     &      viz_ctls%psf_ctls, c_buf)
        call read_files_4_iso_ctl(id_control, hd_isosurf_ctl,           &
     &      viz_ctls%iso_ctls, c_buf)
        call read_files_4_map_ctl(id_control, hd_map_rendering,         &
     &                            viz_ctls%map_ctls, c_buf)
!
        call read_files_4_pvr_ctl(id_control, hd_pvr_ctl,               &
     &      viz_ctls%pvr_ctls, c_buf)
!
        call read_files_4_fline_ctl(id_control, hd_fline_ctl,           &
     &      viz_ctls%fline_ctls, c_buf)
!
!
        call read_integer_ctl_type(c_buf, hd_i_step_section,            &
     &      viz_ctls%i_step_psf_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_isosurf,            &
     &      viz_ctls%i_step_iso_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_map_projection,     &
     &                             viz_ctls%i_step_map_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_pvr,                &
     &      viz_ctls%i_step_pvr_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_fline,              &
     &      viz_ctls%i_step_fline_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_ucd,                &
     &      viz_ctls%i_step_ucd_v_ctl)
!
        call read_real_ctl_type(c_buf, hd_delta_t_section,              &
     &      viz_ctls%delta_t_psf_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_isosurf,              &
     &      viz_ctls%delta_t_iso_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_map_projection,       &
     &                          viz_ctls%delta_t_map_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_pvr,                  &
     &      viz_ctls%delta_t_pvr_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_fline,                &
     &      viz_ctls%delta_t_fline_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_ucd,                  &
     &      viz_ctls%delta_t_ucd_v_ctl)
!
        call read_chara_ctl_type(c_buf, hd_output_fld_file_fmt,         &
     &      viz_ctls%output_field_file_fmt_ctl)
      end do
      viz_ctls%i_viz_control = 1
!
      end subroutine s_read_viz4_controls
!
!  ---------------------------------------------------------------------
!
      subroutine write_viz4_controls                                    &
     &         (id_control, hd_block, viz_ctls, level)
!
      use t_read_control_elements
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use ctl_file_map_renderings_IO
      use ctl_file_fieldlines_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
      type(vis4_controls), intent(in) :: viz_ctls
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(viz_ctls%i_viz_control .le. 0) return
!
      maxlen = len_trim(hd_delta_t_section)
      maxlen = max(maxlen, len_trim(hd_i_step_section))
      maxlen = max(maxlen, len_trim(hd_delta_t_isosurf))
      maxlen = max(maxlen, len_trim(hd_i_step_isosurf))
      maxlen = max(maxlen, len_trim(hd_delta_t_map_projection))
      maxlen = max(maxlen, len_trim(hd_i_step_map_projection))
      maxlen = max(maxlen, len_trim(hd_delta_t_pvr))
      maxlen = max(maxlen, len_trim(hd_i_step_pvr))
      maxlen = max(maxlen, len_trim(hd_delta_t_fline))
      maxlen = max(maxlen, len_trim(hd_i_step_fline))
      maxlen = max(maxlen, len_trim(hd_delta_t_ucd))
      maxlen = max(maxlen, len_trim(hd_i_step_ucd))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_section, viz_ctls%delta_t_psf_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_section, viz_ctls%i_step_psf_v_ctl)
      call write_files_4_psf_ctl(id_control, hd_section_ctl,            &
     &                             viz_ctls%psf_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_isosurf, viz_ctls%delta_t_iso_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_isosurf, viz_ctls%i_step_iso_v_ctl)
      call write_files_4_iso_ctl(id_control, hd_isosurf_ctl,            &
     &                           viz_ctls%iso_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_map_projection, viz_ctls%delta_t_map_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_map_projection, viz_ctls%i_step_map_v_ctl)
      call write_files_4_map_ctl(id_control, hd_map_rendering,          &
     &                           viz_ctls%map_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_pvr, viz_ctls%delta_t_pvr_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_pvr, viz_ctls%i_step_pvr_v_ctl)
      call write_files_4_pvr_ctl(id_control, hd_pvr_ctl,                &
     &                           viz_ctls%pvr_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_fline, viz_ctls%delta_t_fline_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_fline, viz_ctls%i_step_fline_v_ctl)
      call write_files_4_fline_ctl(id_control, hd_fline_ctl,            &
     &                             viz_ctls%fline_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_ucd, viz_ctls%delta_t_ucd_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_ucd, viz_ctls%i_step_ucd_v_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_output_fld_file_fmt, viz_ctls%output_field_file_fmt_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_viz4_controls
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_four_vizs_IO
