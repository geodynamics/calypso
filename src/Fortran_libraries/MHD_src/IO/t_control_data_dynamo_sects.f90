!>@file   t_control_data_dynamo_sects.f90
!!@brief  module t_control_data_dynamo_sects
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2017
!
!> @brief Control data structure for zonal mean visualization controls
!!
!!@verbatim
!!      subroutine read_dynamo_sects_control                            &
!!     &         (id_control, hd_block, zm_sects, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_dynamo_section_controls), intent(inout) :: zm_sects
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_dynamo_sects_control                           &
!!     &         (id_control, hd_block, zm_sects, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_dynamo_section_controls), intent(in) :: zm_sects
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dealloc_dynamo_sects_control(zm_sects)
!!        type(sph_dynamo_section_controls), intent(in) :: zm_sects
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin dynamo_vizs_control
!!    begin crustal_filtering_ctl
!!      truncation_degree_ctl        13
!!    end crustal_filtering_ctl
!!
!!    file  zonal_mean_section_ctl
!!    begin  zonal_RMS_section_ctl
!!      ....
!!    end zonal_RMS_section_ctl
!!  end dynamo_vizs_control
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_dynamo_sects
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_sections
      use t_ctl_data_crust_filter
!
      implicit  none
!
!
!>      Structures of zonal mean controls
      type sph_dynamo_section_controls
!>        Structure of crustal filtering of mangeitc field
        type(clust_filtering_ctl) :: crust_filter_ctl
!
!>        Structure of zonal mean sectioning controls
        type(section_controls) :: zm_psf_ctls
!>        Structure of zonal RMS sectioning controls
        type(section_controls) :: zRMS_psf_ctls
!
        integer (kind=kint) :: i_viz_ctl = 0
      end type sph_dynamo_section_controls
!
!     lavel for volume rendering
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_crustal_filtering = 'crustal_filtering_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_zm_section = 'zonal_mean_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_zRMS_section = 'zonal_RMS_section_ctl'
!
      private :: hd_zm_section, hd_zRMS_section
      private :: hd_crustal_filtering
      private :: read_single_sect_ctl, write_single_sect_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_dynamo_sects_control                              &
     &         (id_control, hd_block, zm_sects, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_dynamo_section_controls), intent(inout) :: zm_sects
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(zm_sects%i_viz_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_crustal_filtering_ctl                                 &
     &     (id_control, hd_crustal_filtering,                           &
     &      zm_sects%crust_filter_ctl, c_buf)
!
        call read_single_sect_ctl(id_control, hd_zm_section,            &
     &      zm_sects%zm_psf_ctls, c_buf)
        call read_single_sect_ctl(id_control, hd_zRMS_section,          &
     &      zm_sects%zRMS_psf_ctls, c_buf)
      end do
      zm_sects%i_viz_ctl = 1
!
      end subroutine read_dynamo_sects_control
!
!   --------------------------------------------------------------------
!
      subroutine write_dynamo_sects_control                             &
     &         (id_control, hd_block, zm_sects, level)
!
      use t_read_control_elements
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_dynamo_section_controls), intent(in) :: zm_sects
      integer(kind = kint), intent(inout) :: level
!
!
      if(zm_sects%i_viz_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_crustal_filtering_ctl(id_control,                      &
     &    hd_crustal_filtering, zm_sects%crust_filter_ctl, level)
!
      call write_single_sect_ctl(id_control, hd_zm_section,             &
     &    zm_sects%zm_psf_ctls, level)
      call write_single_sect_ctl(id_control, hd_zRMS_section,           &
     &    zm_sects%zRMS_psf_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_dynamo_sects_control
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_dynamo_sects_control(zm_sects)
!
      type(sph_dynamo_section_controls), intent(inout) :: zm_sects
!
!
      call reset_crustal_filtering_ctl(zm_sects%crust_filter_ctl)
      call dealloc_psf_ctl_stract(zm_sects%zm_psf_ctls)
      call dealloc_psf_ctl_stract(zm_sects%zRMS_psf_ctls)
!
      end subroutine dealloc_dynamo_sects_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_single_sect_ctl                                   &
     &          (id_control, hd_section, psf_ctls, c_buf)
!
      use t_read_control_elements
      use t_control_data_sections
      use ctl_data_section_IO
      use ctl_file_sections_IO
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_section
      type(section_controls), intent(inout) :: psf_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(psf_ctls%num_psf_ctl .gt. 0) return
!
      if(check_file_flag(c_buf, hd_section)                             &
     &     .or.  check_begin_flag(c_buf, hd_section)) then
        psf_ctls%num_psf_ctl = 1
        call alloc_psf_ctl_stract(psf_ctls)
!
        call write_multi_ctl_file_message                               &
     &     (hd_section, psf_ctls%num_psf_ctl, c_buf%level)
        call sel_read_control_4_psf_file(id_control, hd_section,        &
     &      psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl),               &
     &      psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl), c_buf)
      end if
!
      end subroutine read_single_sect_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_single_sect_ctl                                  &
     &          (id_control, hd_section, psf_ctls, level)
!
      use t_control_data_sections
      use ctl_file_sections_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_section
      type(section_controls), intent(in) :: psf_ctls
      integer(kind = kint), intent(inout) :: level
!
!
      if(psf_ctls%num_psf_ctl .gt. 0) return
      call sel_write_control_4_psf_file(id_control, hd_section,         &
     &    psf_ctls%fname_psf_ctl(1), psf_ctls%psf_ctl_struct(1), level)
!
      end subroutine write_single_sect_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_dynamo_sects
