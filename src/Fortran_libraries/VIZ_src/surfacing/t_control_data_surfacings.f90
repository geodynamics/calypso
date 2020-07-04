!>@file   t_control_data_surfacings.f90
!!@brief  module t_control_data_surfacings
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine read_surfacing_controls                              &
!!     &         (id_control, hd_block, surfacing_ctls, c_buf)
!!      subroutine bcast_surfacing_controls(surfacing_ctls)
!!      subroutine dealloc_surfacing_controls(surfacing_ctls)
!!       type(surfacing_controls), intent(inout) :: surfacing_ctls
!!       type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine section_step_ctls_to_time_ctl(surfacing_ctls, tctl)
!!        type(surfacing_controls), intent(in) :: surfacing_ctls
!!        type(time_data_control), intent(inout) :: tctl
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
      module t_control_data_surfacings
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!>        Structures of surfacing controls
      type surfacing_controls
!>        Structures of setioning controls
        type(section_controls) :: psf_s_ctls
!>        Structures of isosurface controls
        type(isosurf_controls) :: iso_s_ctls
!
!>   time interval for sectioning
        type(read_real_item) :: delta_t_psf_s_ctl
!>   time interval for isosurface
        type(read_real_item) :: delta_t_iso_s_ctl
!>   time interval for field data output
        type(read_real_item) :: delta_t_ucd_s_ctl
!
!>   Increment for sectioning
        type(read_integer_item) :: i_step_psf_s_ctl
!>   Increment for isosurface
        type(read_integer_item) :: i_step_iso_s_ctl
!>   Increment for field data output
        type(read_integer_item) :: i_step_ucd_s_ctl
!
!>   Putput field file format
        type(read_character_item) :: output_ucd_fmt_s_ctl
!
        integer (kind=kint) :: i_surfacing_control = 0
      end type surfacing_controls
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
      subroutine read_surfacing_controls                                &
     &         (id_control, hd_block, surfacing_ctls, c_buf)
!
      use t_read_control_elements
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
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_array_flag(c_buf, hd_section_ctl)) then
          call read_files_4_psf_ctl(id_control, hd_section_ctl,         &
     &        surfacing_ctls%psf_s_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_isosurf_ctl)) then
          call read_files_4_iso_ctl(id_control, hd_isosurf_ctl,         &
     &        surfacing_ctls%iso_s_ctls, c_buf)
        end if
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
      end subroutine read_surfacing_controls
!
!   --------------------------------------------------------------------
!
      subroutine bcast_surfacing_controls(surfacing_ctls)
!
      use bcast_control_arrays
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
!
!
      call bcast_files_4_psf_ctl(surfacing_ctls%psf_s_ctls)
      call bcast_files_4_iso_ctl(surfacing_ctls%iso_s_ctls)
!
      call bcast_ctl_type_r1(surfacing_ctls%delta_t_psf_s_ctl)
      call bcast_ctl_type_r1(surfacing_ctls%delta_t_iso_s_ctl)
      call bcast_ctl_type_r1(surfacing_ctls%delta_t_ucd_s_ctl)
!
      call bcast_ctl_type_i1(surfacing_ctls%i_step_psf_s_ctl)
      call bcast_ctl_type_i1(surfacing_ctls%i_step_iso_s_ctl)
      call bcast_ctl_type_i1(surfacing_ctls%i_step_ucd_s_ctl)
!
      call bcast_ctl_type_c1(surfacing_ctls%output_ucd_fmt_s_ctl)
!
      call MPI_BCAST(surfacing_ctls%i_surfacing_control, 1,             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_surfacing_controls
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_surfacing_controls(surfacing_ctls)
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
!
!
      call dealloc_psf_ctl_stract(surfacing_ctls%psf_s_ctls)
      call dealloc_iso_ctl_stract(surfacing_ctls%iso_s_ctls)
!
      surfacing_ctls%delta_t_psf_s_ctl%iflag =   0
      surfacing_ctls%delta_t_iso_s_ctl%iflag =   0
      surfacing_ctls%delta_t_ucd_s_ctl%iflag =   0
!
      surfacing_ctls%i_step_psf_s_ctl%iflag =   0
      surfacing_ctls%i_step_iso_s_ctl%iflag =   0
      surfacing_ctls%i_step_ucd_s_ctl%iflag =   0
!
      surfacing_ctls%output_ucd_fmt_s_ctl%iflag = 0
!
      surfacing_ctls%i_surfacing_control = 0
!
      end subroutine dealloc_surfacing_controls
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine section_step_ctls_to_time_ctl(surfacing_ctls, tctl)
!
      use t_ctl_data_4_time_steps
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      type(surfacing_controls), intent(in) :: surfacing_ctls
      type(time_data_control), intent(inout) :: tctl
!
!
      if(surfacing_ctls%i_step_psf_s_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (surfacing_ctls%i_step_psf_s_ctl, tctl%i_step_psf_ctl)
      end if
      if(surfacing_ctls%i_step_iso_s_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (surfacing_ctls%i_step_iso_s_ctl, tctl%i_step_iso_ctl)
      end if
      if(surfacing_ctls%i_step_ucd_s_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (surfacing_ctls%i_step_ucd_s_ctl, tctl%i_step_ucd_ctl)
      end if
!
      if(surfacing_ctls%delta_t_psf_s_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (surfacing_ctls%delta_t_psf_s_ctl, tctl%delta_t_psf_ctl)
      end if
      if(surfacing_ctls%delta_t_iso_s_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (surfacing_ctls%delta_t_iso_s_ctl, tctl%delta_t_iso_ctl)
      end if
      if(surfacing_ctls%delta_t_ucd_s_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (surfacing_ctls%delta_t_ucd_s_ctl, tctl%delta_t_field_ctl)
      end if
!
      end subroutine section_step_ctls_to_time_ctl
!
!  ---------------------------------------------------------------------
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
      end module t_control_data_surfacings
