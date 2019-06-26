!>@file   read_sections_control_data.f90
!!@brief  module read_sections_control_data
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief read control data for cross sections and isosurfaces
!!
!!@verbatim
!!      subroutine s_read_sections_control_data                         &
!!     &         (id_control, psf_ctls, iso_ctls, c_buf)
!!        type(section_controls), intent(inout) :: psf_ctls
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module read_sections_control_data
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_sections
      use t_control_data_isosurfaces
!
      implicit  none
!
!
!   entry label
!
      character(len=kchara), parameter :: hd_viz_ctl = 'visual_control'
      integer (kind=kint) :: i_viz_ctl = 0
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
!
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
!
      private :: hd_section_ctl, hd_psf_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
      private :: hd_viz_ctl, i_viz_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_sections_control_data                           &
     &         (id_control, psf_ctls, iso_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(section_controls), intent(inout) :: psf_ctls
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_viz_ctl) .eqv. .FALSE.) return
!
      if(i_viz_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_viz_ctl)) exit
!
        if(check_array_flag(c_buf, hd_psf_ctl)) then
          call read_files_4_psf_ctl                                     &
     &       (id_control, hd_psf_ctl, psf_ctls, c_buf)
        end if
        if(check_array_flag(c_buf, hd_section_ctl)) then
          call read_files_4_psf_ctl                                     &
     &     (id_control, hd_section_ctl, psf_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_iso_ctl)) then
          call read_files_4_iso_ctl                                     &
     &       (id_control, hd_iso_ctl, iso_ctls, c_buf)
        end if
        if(check_array_flag(c_buf, hd_isosurf_ctl)) then
          call read_files_4_iso_ctl                                     &
     &       (id_control, hd_isosurf_ctl, iso_ctls, c_buf)
        end if
      end do
      i_viz_ctl = 1
!
      end subroutine s_read_sections_control_data
!
!   --------------------------------------------------------------------
!
      end module read_sections_control_data

