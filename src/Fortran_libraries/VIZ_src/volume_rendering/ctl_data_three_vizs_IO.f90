!>@file   ctl_data_three_vizs_IO.f90
!!@brief  module ctl_data_three_vizs_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine init_viz3_ctl_label(hd_block, viz3_ctls)
!!      subroutine s_read_viz3_controls(id_control, viz3_ctls, c_buf)
!!       integer(kind = kint), intent(in) :: id_control
!!       character(len=kchara), intent(in) :: hd_block
!!       type(vis3_controls), intent(inout) :: viz3_ctls
!!       type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_viz3_controls                                  &
!!     &         (id_control, hd_block, viz3_ctls, level)
!!       integer(kind = kint), intent(in) :: id_control
!!       character(len=kchara), intent(in) :: hd_block
!!       type(vis3_controls), intent(in) :: viz3_ctls
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
!!  end  visual_control
!!
!!    delta_t_sectioning_ctl   1.0e-3
!!    i_step_sectioning_ctl    400
!!    delta_t_isosurface_ctl   1.0e-3
!!    i_step_isosurface_ctl    400
!!    delta_t_map_projection_ctl   1.0e-3
!!    i_step_map_projection_ctl    400
!!    delta_t_pvr_ctl          1.0e-2
!!    i_step_pvr_ctl           400
!!    delta_t_field_ctl        1.0e-3
!!    i_step_field_ctl         800
!!    output_field_file_fmt_ctl   'VTK'
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module ctl_data_three_vizs_IO
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_viz3
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
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_section =        'i_step_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_isosurf =        'i_step_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_map_projection = 'i_step_map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_pvr =            'i_step_pvr_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_ucd =       'i_step_field_ctl'
!
!
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_section =        'delta_t_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_isosurf =        'delta_t_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_map_projection = 'delta_t_map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_pvr =            'delta_t_pvr_ctl'
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
      subroutine s_read_viz3_controls                                   &
     &         (id_control, hd_block, viz3_ctls, c_buf)
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
      type(vis3_controls), intent(inout) :: viz3_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(viz3_ctls%i_viz_control .gt. 0) return
      call init_psf_ctls_labels(hd_section_ctl, viz3_ctls%psf_ctls)
      call init_iso_ctls_labels(hd_isosurf_ctl, viz3_ctls%iso_ctls)
      call init_map_ctls_labels(hd_map_rendering, viz3_ctls%map_ctls)
      call init_pvr_ctls_labels(hd_pvr_ctl, viz3_ctls%pvr_ctls)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_files_4_psf_ctl(id_control, hd_section_ctl,           &
     &                            viz3_ctls%psf_ctls, c_buf)
        call read_files_4_iso_ctl(id_control, hd_isosurf_ctl,           &
     &                            viz3_ctls%iso_ctls, c_buf)
        call read_files_4_map_ctl(id_control, hd_map_rendering,         &
     &                            viz3_ctls%map_ctls, c_buf)
!
        call read_files_4_pvr_ctl(id_control, hd_pvr_ctl,               &
     &                            viz3_ctls%pvr_ctls, c_buf)
!
        call read_integer_ctl_type(c_buf, hd_i_step_section,            &
     &                             viz3_ctls%i_step_psf_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_isosurf,            &
     &                             viz3_ctls%i_step_iso_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_map_projection,     &
     &                             viz3_ctls%i_step_map_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_pvr,                &
     &                             viz3_ctls%i_step_pvr_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_ucd,                &
     &                             viz3_ctls%i_step_ucd_v_ctl)
!
        call read_real_ctl_type(c_buf, hd_delta_t_section,              &
     &                          viz3_ctls%delta_t_psf_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_isosurf,              &
     &                          viz3_ctls%delta_t_iso_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_map_projection,       &
     &                          viz3_ctls%delta_t_map_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_pvr,                  &
     &                          viz3_ctls%delta_t_pvr_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_ucd,                  &
     &                          viz3_ctls%delta_t_ucd_v_ctl)
!
        call read_chara_ctl_type(c_buf, hd_output_fld_file_fmt,         &
     &      viz3_ctls%output_field_file_fmt_ctl)
      end do
      viz3_ctls%i_viz_control = 1
!
      end subroutine s_read_viz3_controls
!
!  ---------------------------------------------------------------------
!
      subroutine write_viz3_controls                                    &
     &         (id_control, hd_block, viz3_ctls, level)
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
      type(vis3_controls), intent(in) :: viz3_ctls
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(viz3_ctls%i_viz_control .le. 0) return
!
      maxlen = len_trim(hd_delta_t_section)
      maxlen = max(maxlen, len_trim(hd_i_step_section))
      maxlen = max(maxlen, len_trim(hd_delta_t_isosurf))
      maxlen = max(maxlen, len_trim(hd_i_step_isosurf))
      maxlen = max(maxlen, len_trim(hd_delta_t_map_projection))
      maxlen = max(maxlen, len_trim(hd_i_step_map_projection))
      maxlen = max(maxlen, len_trim(hd_delta_t_pvr))
      maxlen = max(maxlen, len_trim(hd_i_step_pvr))
      maxlen = max(maxlen, len_trim(hd_delta_t_ucd))
      maxlen = max(maxlen, len_trim(hd_i_step_ucd))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz3_ctls%delta_t_psf_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz3_ctls%i_step_psf_v_ctl)
      call write_files_4_psf_ctl(id_control, hd_section_ctl,            &
     &                             viz3_ctls%psf_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz3_ctls%delta_t_iso_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz3_ctls%i_step_iso_v_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz3_ctls%delta_t_map_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz3_ctls%i_step_map_v_ctl)
      call write_files_4_map_ctl(id_control, hd_map_rendering,          &
     &                           viz3_ctls%map_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz3_ctls%delta_t_pvr_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz3_ctls%i_step_pvr_v_ctl)
      call write_files_4_pvr_ctl(id_control, hd_pvr_ctl,                &
     &                           viz3_ctls%pvr_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz3_ctls%delta_t_ucd_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz3_ctls%i_step_ucd_v_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    viz3_ctls%output_field_file_fmt_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_viz3_controls
!
!  ---------------------------------------------------------------------
!
      subroutine init_viz3_ctl_label(hd_block, viz3_ctls)
!
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use ctl_file_map_renderings_IO
      use ctl_file_fieldlines_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(vis3_controls), intent(inout) :: viz3_ctls
!
!
      viz3_ctls%block_name = hd_block
      call init_psf_ctls_labels(hd_section_ctl, viz3_ctls%psf_ctls)
      call init_iso_ctls_labels(hd_isosurf_ctl, viz3_ctls%iso_ctls)
      call init_map_ctls_labels(hd_map_rendering, viz3_ctls%map_ctls)
      call init_pvr_ctls_labels(hd_pvr_ctl, viz3_ctls%pvr_ctls)
!
        call init_int_ctl_item_label(hd_i_step_section,                 &
     &                             viz3_ctls%i_step_psf_v_ctl)
        call init_int_ctl_item_label(hd_i_step_isosurf,                 &
     &                             viz3_ctls%i_step_iso_v_ctl)
        call init_int_ctl_item_label(hd_i_step_map_projection,          &
     &                             viz3_ctls%i_step_map_v_ctl)
        call init_int_ctl_item_label(hd_i_step_pvr,                     &
     &                             viz3_ctls%i_step_pvr_v_ctl)
        call init_int_ctl_item_label(hd_i_step_ucd,                     &
     &                             viz3_ctls%i_step_ucd_v_ctl)
!
        call init_real_ctl_item_label(hd_delta_t_section,               &
     &                          viz3_ctls%delta_t_psf_v_ctl)
        call init_real_ctl_item_label(hd_delta_t_isosurf,               &
     &                          viz3_ctls%delta_t_iso_v_ctl)
        call init_real_ctl_item_label(hd_delta_t_map_projection,        &
     &                          viz3_ctls%delta_t_map_v_ctl)
        call init_real_ctl_item_label(hd_delta_t_pvr,                   &
     &                          viz3_ctls%delta_t_pvr_v_ctl)
        call init_real_ctl_item_label(hd_delta_t_ucd,                   &
     &                          viz3_ctls%delta_t_ucd_v_ctl)
!
        call init_chara_ctl_item_label(hd_output_fld_file_fmt,          &
     &      viz3_ctls%output_field_file_fmt_ctl)
!
      end subroutine init_viz3_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_three_vizs_IO
