!>@file   t_control_data_zm_vizs.f90
!!@brief  module t_control_data_zm_vizs
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2017
!
!> @brief Control data structure for zonal mean visualization controls
!!
!!@verbatim
!!      subroutine read_zonal_mean_control(id_control, zm_ctls, c_buf)
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
!
        integer (kind=kint) :: i_viz_ctl = 0
      end type sph_zonal_means_controls
!
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
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
      private :: hd_zm_viz_ctl
      private :: read_single_section_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_zonal_mean_control(id_control, zm_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(sph_zonal_means_controls), intent(inout) :: zm_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_zm_viz_ctl) .eqv. .FALSE.) return
      if(zm_ctls%i_viz_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_zm_viz_ctl)) exit
!
        call read_single_section_ctl(id_control, hd_zm_section,         &
     &      zm_ctls%zm_psf_ctls, c_buf)
        call read_single_section_ctl(id_control, hd_zRMS_section,       &
     &      zm_ctls%zRMS_psf_ctls, c_buf)
      end do
      zm_ctls%i_viz_ctl = 1
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
      subroutine read_single_section_ctl                                &
     &          (id_control, hd_section, psf_ctls, c_buf)
!
      use t_read_control_elements
      use t_control_data_sections
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_section
      type(section_controls), intent(inout) :: psf_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(psf_ctls%num_psf_ctl .gt. 0) return
!
      if(check_file_flag(c_buf, hd_section)) then
        psf_ctls%num_psf_ctl = 1
        call alloc_psf_ctl_stract(psf_ctls)
        psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl)                    &
     &                                  = third_word(c_buf)
!
        write(*,'(3a)', ADVANCE='NO') 'Read file for ',                 &
     &                               trim(hd_section), '... '
        call read_control_4_psf_file(id_control+2,                      &
     &      psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl),               &
     &      psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl))
      else if(check_begin_flag(c_buf, hd_section)) then
        psf_ctls%num_psf_ctl = 1
        call alloc_psf_ctl_stract(psf_ctls)
        psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl) = 'NO_FILE'
!
        write(*,*) 'Control for', trim(hd_section), ' is included'
        call read_psf_control_data(id_control, hd_section,              &
     &      psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl), c_buf)
      end if
!
      end subroutine read_single_section_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_zm_vizs
