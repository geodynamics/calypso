!>@file   control_data_surfacing_IO.f90
!!@brief  module control_data_surfacing_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine s_read_surfacing_controls                            &
!!     &         (id_control, hd_block, surfacing_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control 
!!        character(len=kchara), intent(in) :: hd_block
!!        type(surfacing_controls), intent(inout) :: surfacing_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_surfacing_controls                             &
!!     &         (id_control, hd_block, surfacing_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control 
!!        character(len=kchara), intent(in) :: hd_block
!!        type(surfacing_controls), intent(in) :: surfacing_ctls
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_surfacings()
!!      subroutine set_label_surfacings(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    delta_t_sectioning_ctl   1.0e-3
!!    i_step_sectioning_ctl    400
!!    array  cross_section_ctl  1
!!      ....
!!    end array cross_section_ctl
!!
!!    delta_t_isosurface_ctl   1.0e-3
!!    i_step_isosurface_ctl    400
!!    array  isosurface_ctl  1
!!      ....
!!    end array isosurface_ctl
!!
!!    delta_t_field_ctl        1.0e-3
!!    i_step_field_ctl         800
!!    output_field_file_fmt_ctl   'VTK'
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module control_data_surfacing_IO
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_data_surfacings
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!     Top level
      character(len=kchara), parameter, private                         &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_section =   'i_step_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_isosurf =   'i_step_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_ucd =       'i_step_field_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_section =   'delta_t_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_isosurf =   'delta_t_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_ucd =       'delta_t_field_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_output_fld_file_fmt = 'output_field_file_fmt_ctl'
!
      integer(kind = kint), parameter, private                          &
     &                      :: n_label_surfacings = 9
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_surfacing_controls                              &
     &         (id_control, hd_block, surfacing_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(surfacing_ctls%i_surfacing_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_files_4_psf_ctl(id_control, hd_section_ctl,           &
     &      surfacing_ctls%psf_s_ctls, c_buf)
        call read_files_4_iso_ctl(id_control, hd_isosurf_ctl,           &
     &      surfacing_ctls%iso_s_ctls, c_buf)
!
        call read_real_ctl_type(c_buf, hd_delta_t_section,              &
     &      surfacing_ctls%delta_t_psf_s_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_isosurf,              &
     &      surfacing_ctls%delta_t_iso_s_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_ucd,                  &
     &      surfacing_ctls%delta_t_ucd_s_ctl)
!
        call read_integer_ctl_type(c_buf, hd_i_step_section,            &
     &      surfacing_ctls%i_step_psf_s_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_isosurf,            &
     &      surfacing_ctls%i_step_iso_s_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_ucd,                &
     &      surfacing_ctls%i_step_ucd_s_ctl)
!
        call read_chara_ctl_type(c_buf, hd_output_fld_file_fmt,         &
     &      surfacing_ctls%output_ucd_fmt_s_ctl)
      end do
      surfacing_ctls%i_surfacing_control = 1
!
      end subroutine s_read_surfacing_controls
!
!   --------------------------------------------------------------------
!
      subroutine write_surfacing_controls                               &
     &         (id_control, hd_block, surfacing_ctls, level)
!
      use t_read_control_elements
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
      type(surfacing_controls), intent(in) :: surfacing_ctls
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(surfacing_ctls%i_surfacing_control .le. 0) return
!
      maxlen = len_trim(hd_delta_t_section)
      maxlen = max(maxlen, len_trim(hd_i_step_section))
      maxlen = max(maxlen, len_trim(hd_delta_t_isosurf))
      maxlen = max(maxlen, len_trim(hd_i_step_isosurf))
      maxlen = max(maxlen, len_trim(hd_delta_t_ucd))
      maxlen = max(maxlen, len_trim(hd_i_step_ucd))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_section, surfacing_ctls%delta_t_psf_s_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_section, surfacing_ctls%i_step_psf_s_ctl)
      call write_files_4_psf_ctl(id_control,                            &
     &    hd_section_ctl, surfacing_ctls%psf_s_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_isosurf, surfacing_ctls%delta_t_iso_s_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_isosurf, surfacing_ctls%i_step_iso_s_ctl)
      call write_files_4_iso_ctl(id_control,                            &
     &    hd_isosurf_ctl, surfacing_ctls%iso_s_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_t_ucd, surfacing_ctls%delta_t_ucd_s_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_i_step_ucd, surfacing_ctls%i_step_ucd_s_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_output_fld_file_fmt, surfacing_ctls%output_ucd_fmt_s_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_surfacing_controls
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_surfacings()
      num_label_surfacings = n_label_surfacings
      return
      end function num_label_surfacings
!
! ----------------------------------------------------------------------
!
      subroutine set_label_surfacings(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_surfacings)
!
!
      call set_control_labels(hd_i_step_section,  names( 1))
      call set_control_labels(hd_delta_t_section, names( 2))
      call set_control_labels(hd_section_ctl,     names( 3))
!
      call set_control_labels(hd_i_step_isosurf,  names( 4))
      call set_control_labels(hd_delta_t_isosurf, names( 5))
      call set_control_labels(hd_isosurf_ctl,     names( 6))
!
      call set_control_labels(hd_i_step_ucd,          names( 7))
      call set_control_labels(hd_delta_t_ucd,         names( 8))
      call set_control_labels(hd_output_fld_file_fmt, names( 9))
!
      end subroutine set_label_surfacings
!
!  ---------------------------------------------------------------------
!
      end module control_data_surfacing_IO
