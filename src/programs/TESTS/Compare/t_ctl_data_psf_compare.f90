!>@file   t_ctl_data_psf_compare.f90
!!@brief  module  
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine read_control_file_psf_compare(my_rank, psf_cmp_ctl)
!!      subroutine read_ctl_data_psf_compare                            &
!!     &         (id_control, hd_block, psf_cmp_ctl, c_buf)
!!      subroutine reset_ctl_data_psf_compare(psf_cmp_ctl)
!!        type(psf_file_control), intent(inout) :: psf_cmp_ctl
!!      subroutine copy_ctl_data_psf_compare(org_psf_cmp, new_psf_cmp)
!!        type(psf_compare_control), intent(in) :: org_psf_cmp
!!        type(psf_compare_control), intent(inout) :: new_psf_cmp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin compare_surface_file
!!    begin first_file_ctl
!!      surface_file_prefix    'isosurface/iso_w10n'
!!      surface_file_format            VTK
!!    end first_file_ctl
!!    begin second_file_ctl
!!      surface_file_prefix    'isosurface/iso_w10n'
!!      surface_file_format            VTK
!!    end second_file_ctl
!!
!!    i_step_surface_ctl      10
!!  end compare_surface_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_psf_compare
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!
!>        Control file name
      character(len = kchara), parameter, private                       &
     &             :: fname_ctl_psf_compare = 'control_compare_psf'
!>        Control file ID
      integer(kind = kint), parameter, private :: id_control = 11
!
!>        Structures of surface data control
      type psf_file_control
!>   Putput field file format
        type(read_character_item) :: file_prefix_ctl
!>   Putput field file format
        type(read_character_item) :: file_format_ctl
!
        integer (kind=kint) :: i_psf_file_control = 0
      end type psf_file_control
!
!>        Structures of surface data comparison
      type psf_compare_control
!>   Putput field file format
        type(psf_file_control) :: first_psf
!>   Putput field file format
        type(psf_file_control) :: second_psf
!
!>   Increment for field data output
        type(read_integer_item) :: i_step_surface_ctl
!
        integer (kind=kint) :: i_psf_compare_control = 0
      end type psf_compare_control
!
      character(len=kchara), parameter, private                         &
     &             :: hd_compare_psf_file = 'compare_surface_file'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_first_file = 'first_file_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_second_file = 'second_file_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_step_surface = 'i_step_surface_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_surface_file_prefix = 'surface_file_prefix'
      character(len=kchara), parameter, private                         &
     &             :: hd_surface_file_format = 'surface_file_format'
!
      private :: read_ctl_data_psf_file, reset_ctl_data_psf_file
      private :: copy_ctl_data_psf_file
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_file_psf_compare(my_rank, psf_cmp_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(psf_compare_control), intent(inout) :: psf_cmp_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_psf_compare, status='old')
        do
          call load_one_line_from_control                               &
     &       (id_control, hd_compare_psf_file, c_buf1)
          if(c_buf1%iend .gt. 0) exit
!
          call read_ctl_data_psf_compare                                &
     &       (id_control, hd_compare_psf_file, psf_cmp_ctl, c_buf1)
          if(psf_cmp_ctl%i_psf_compare_control .gt. 0) exit
        end do
        close(id_control)
      end if
      if(c_buf1%iend .gt. 0)                                            &
     &              psf_cmp_ctl%i_psf_compare_control = c_buf1%iend
!
      end subroutine read_control_file_psf_compare
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_psf_compare                              &
     &         (id_control, hd_block, psf_cmp_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(psf_compare_control), intent(inout) :: psf_cmp_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(psf_cmp_ctl%i_psf_compare_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_ctl_data_psf_file                                     &
     &     (id_control, hd_first_file, psf_cmp_ctl%first_psf, c_buf)
        call read_ctl_data_psf_file                                     &
     &     (id_control, hd_second_file, psf_cmp_ctl%second_psf, c_buf)
!
        call read_integer_ctl_type(c_buf, hd_step_surface,              &
     &      psf_cmp_ctl%i_step_surface_ctl)
      end do
      psf_cmp_ctl%i_psf_compare_control = 1
!
      end subroutine read_ctl_data_psf_compare
!
!   --------------------------------------------------------------------
!
      subroutine reset_ctl_data_psf_compare(psf_cmp_ctl)
!
      type(psf_compare_control), intent(inout) :: psf_cmp_ctl
!
!
      call reset_ctl_data_psf_file(psf_cmp_ctl%first_psf)
      call reset_ctl_data_psf_file(psf_cmp_ctl%second_psf)
      psf_cmp_ctl%i_step_surface_ctl%iflag =   0
!
      psf_cmp_ctl%i_psf_compare_control =      0
!
      end subroutine reset_ctl_data_psf_compare
!
!   --------------------------------------------------------------------
!
      subroutine copy_ctl_data_psf_compare(org_psf_cmp, new_psf_cmp)
!
      type(psf_compare_control), intent(in) :: org_psf_cmp
      type(psf_compare_control), intent(inout) :: new_psf_cmp
!
!
      call copy_ctl_data_psf_file(org_psf_cmp%first_psf,                &
     &                            new_psf_cmp%first_psf)
      call copy_ctl_data_psf_file(org_psf_cmp%second_psf,               &
     &                            new_psf_cmp%second_psf)
      call copy_integer_ctl(org_psf_cmp%i_step_surface_ctl,             &
     &                      new_psf_cmp%i_step_surface_ctl)
!
      new_psf_cmp%i_psf_compare_control                                 &
     &      = org_psf_cmp%i_psf_compare_control
!
      end subroutine copy_ctl_data_psf_compare
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_psf_file                                 &
     &         (id_control, hd_block, first_psf, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(psf_file_control), intent(inout) :: first_psf
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(first_psf%i_psf_file_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_surface_file_prefix,         &
     &      first_psf%file_prefix_ctl)
        call read_chara_ctl_type(c_buf, hd_surface_file_format,         &
     &      first_psf%file_format_ctl)
      end do
      first_psf%i_psf_file_control = 1
!
      end subroutine read_ctl_data_psf_file
!
!   --------------------------------------------------------------------
!
      subroutine reset_ctl_data_psf_file(first_psf)
!
      type(psf_file_control), intent(inout) :: first_psf
!
!
      first_psf%file_prefix_ctl%iflag =   0
      first_psf%file_format_ctl%iflag =   0
!
      first_psf%i_psf_file_control =      0
!
      end subroutine reset_ctl_data_psf_file
!
!   --------------------------------------------------------------------
!
      subroutine copy_ctl_data_psf_file(first_psf, second_psf)
!
      type(psf_file_control), intent(in) :: first_psf
      type(psf_file_control), intent(inout) :: second_psf
!
!
      call copy_chara_ctl(first_psf%file_prefix_ctl,                    &
     &                    second_psf%file_prefix_ctl)
      call copy_chara_ctl(first_psf%file_format_ctl,                    &
     &                    second_psf%file_format_ctl)
!
      second_psf%i_psf_file_control = first_psf%i_psf_file_control
!
      end subroutine copy_ctl_data_psf_file
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_psf_compare
