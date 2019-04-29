!>@file   t_control_data_zm_vizs.f90
!!@brief  module t_control_data_zm_vizs
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2017
!
!> @brief Control data structure for zonal mean visualization controls
!!
!!@verbatim
!!      subroutine read_zonal_mean_control(zm_ctls)
!!      subroutine bcast_zonal_mean_control(viz_ctls)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin zonal_mean_control
!!    file  zonal_mean_section_ctl
!!    begin  zonal_RMS_section_ctl
!!      ....
!!    end zonal_RMS_section_ctl
!!  end zonal_mean_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_zm_vizs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_sections
!
      implicit  none
!
!>      Structures of zonal mean controls
      type sph_zonal_means_controls
!>        Structures of zonal mean sectioning controls
        type(section_controls) :: zm_psf_ctls
!>        Structures of zonal RMS sectioning controls
        type(section_controls) :: zRMS_psf_ctls
      end type sph_zonal_means_controls
!
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
      integer (kind=kint) :: i_viz_ctl = 0
!
!     lavel for volume rendering
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_zm_section = 'zonal_mean_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_zRMS_section = 'zonal_RMS_section_ctl'
!
      private :: hd_zm_section, hd_zRMS_section
      private :: hd_zm_viz_ctl, i_viz_ctl
      private :: read_single_section_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_zonal_mean_control(zm_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(sph_zonal_means_controls), intent(inout) :: zm_ctls
!
!
      if(right_begin_flag(hd_zm_viz_ctl) .eq. 0) return
      if (i_viz_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_viz_ctl = find_control_end_flag(hd_zm_viz_ctl)
        if(i_viz_ctl .eq. 1) exit
!
        call read_single_section_ctl                                &
     &     (hd_zm_section, zm_ctls%zm_psf_ctls)
        call read_single_section_ctl                                &
     &     (hd_zRMS_section, zm_ctls%zRMS_psf_ctls)
      end do
!
      end subroutine read_zonal_mean_control
!
!   --------------------------------------------------------------------
!
      subroutine bcast_zonal_mean_control(zm_ctls)
!
      type(sph_zonal_means_controls), intent(inout) :: zm_ctls
!
!
      call bcast_files_4_psf_ctl(zm_ctls%zm_psf_ctls)
      call bcast_files_4_psf_ctl(zm_ctls%zRMS_psf_ctls)
!
      end subroutine bcast_zonal_mean_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_single_section_ctl(hd_section, psf_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      character(len = kchara), intent(in) :: hd_section
      type(section_controls), intent(inout) :: psf_ctls
!
      integer(kind=kint) :: i_psf_ctl = 0
!
!
      if(psf_ctls%num_psf_ctl .gt. 0) return
!      call check_read_control_header
!      call check_read_control_buffer
!
      if(right_file_flag(hd_section) .gt. 0) then
        psf_ctls%num_psf_ctl = 1
        call alloc_psf_ctl_stract(psf_ctls)
        call read_file_names_from_ctl_line                            &
     &     (psf_ctls%num_psf_ctl, i_psf_ctl, psf_ctls%fname_psf_ctl)
      else if(right_begin_flag(hd_section) .gt. 0) then
        i_psf_ctl = i_psf_ctl + 1
        psf_ctls%num_psf_ctl = 1
        call alloc_psf_ctl_stract(psf_ctls)
        psf_ctls%fname_psf_ctl(i_psf_ctl) = 'NO_FILE'
        call read_psf_control_data                                    &
     &     (hd_section, psf_ctls%psf_ctl_struct(i_psf_ctl))
      end if
!
      end subroutine read_single_section_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_zm_vizs
