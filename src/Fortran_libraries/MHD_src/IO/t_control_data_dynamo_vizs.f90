!>@file   t_control_data_dynamo_vizs.f90
!!@brief  module t_control_data_dynamo_vizs
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2017
!
!> @brief Control data structure for zonal mean visualization controls
!!
!!@verbatim
!!      subroutine read_dynamo_viz_control                              &
!!     &         (id_control, hd_block, zm_ctls, c_buf)
!!      subroutine bcast_dynamo_viz_control(viz_ctls)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin dynamo_vizs_control
!!    file  zonal_mean_section_ctl
!!    begin  zonal_RMS_section_ctl
!!      ....
!!    end zonal_RMS_section_ctl
!!
!!    begin crustal_filtering_ctl
!!      truncation_degree_ctl        13
!!    end crustal_filtering_ctl
!!  end dynamo_vizs_control
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_dynamo_vizs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_sections
      use t_ctl_data_crust_filter
!
      implicit  none
!
!
!>      Structures of zonal mean controls
      type sph_dynamo_viz_controls
!>        Structure of zonal mean sectioning controls
        type(section_controls) :: zm_psf_ctls
!>        Structure of zonal RMS sectioning controls
        type(section_controls) :: zRMS_psf_ctls
!>        Structure of crustal filtering of mangeitc field
        type(clust_filtering_ctl) :: crust_filter_ctl
!
        integer (kind=kint) :: i_viz_ctl = 0
      end type sph_dynamo_viz_controls
!
!     lavel for volume rendering
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_zm_section = 'zonal_mean_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_zRMS_section = 'zonal_RMS_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_crustal_filtering = 'crustal_filtering_ctl'
!
      private :: hd_zm_section, hd_zRMS_section
      private :: hd_crustal_filtering
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_dynamo_viz_control                                &
     &         (id_control, hd_block, zm_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(zm_ctls%i_viz_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_single_section_ctl(id_control, hd_zm_section,         &
     &      zm_ctls%zm_psf_ctls, c_buf)
        call read_single_section_ctl(id_control, hd_zRMS_section,       &
     &      zm_ctls%zRMS_psf_ctls, c_buf)
        call read_crustal_filtering_ctl                                 &
     &     (id_control, hd_crustal_filtering,                           &
     &      zm_ctls%crust_filter_ctl, c_buf)
      end do
      zm_ctls%i_viz_ctl = 1
!
      end subroutine read_dynamo_viz_control
!
!   --------------------------------------------------------------------
!
      subroutine bcast_dynamo_viz_control(zm_ctls)
!
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
!
      call bcast_files_4_psf_ctl(zm_ctls%zm_psf_ctls)
      call bcast_files_4_psf_ctl(zm_ctls%zRMS_psf_ctls)
      call bcast_crustal_filtering_ctl(zm_ctls%crust_filter_ctl)
!
      end subroutine bcast_dynamo_viz_control
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
      end module t_control_data_dynamo_vizs
