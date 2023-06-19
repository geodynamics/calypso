!>@file   t_ctl_data_sph_dipolarity.f90
!!        module t_ctl_data_sph_dipolarity
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_sph_dipolarity_ctl                              &
!!     &         (id_control, hd_block, fdip_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_dipolarity_control), intent(inout) :: fdip_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_dipolarity_ctl                             &
!!     &         (id_control, hd_block, fdip_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_dipolarity_control), intent(in) :: fdip_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dealloc_sph_dipolarity_ctl(fdip_ctl)
!!        type(sph_dipolarity_control), intent(inout) :: fdip_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_dipolarity_ctl
!!    dipolarity_file_prefix        'monitor/dipolarity'
!!    dipolarity_file_format        'gzip'
!!
!!    array dipolarity_truncation_ctl
!!      dipolarity_truncation_ctl     13
!!    end array dipolarity_truncation_ctl
!!  end sph_dipolarity_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_sph_dipolarity
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_sph_vol_spectr
      use skip_comment_f
!
      implicit  none
!
!
!>        Structure for dipolarity setting
      type sph_dipolarity_control
!>        Structure for truncation lavel for dipolarity
        type(ctl_array_int) :: fdip_truncation_ctl
!
!>        Structure for dipolarity file prefix
        type(read_character_item) :: fdip_file_prefix_ctl
!
!>        Structure for dipolarity file format
        type(read_character_item) :: fdip_file_format_ctl
!
        integer (kind = kint) :: i_dipolarity_ctl = 0
      end type sph_dipolarity_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &            :: hd_fdip_truncation = 'dipolarity_truncation_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_fdip_file_prefix = 'dipolarity_file_prefix'
      character(len=kchara), parameter, private                         &
     &            :: hd_fdip_file_format = 'dipolarity_file_format'
!
!   labels for item
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_dipolarity_ctl                                &
     &         (id_control, hd_block, fdip_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_dipolarity_control), intent(inout) :: fdip_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fdip_ctl%i_dipolarity_ctl  .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_i1(id_control, hd_fdip_truncation,      &
     &                             fdip_ctl%fdip_truncation_ctl, c_buf)
        call read_chara_ctl_type(c_buf, hd_fdip_file_prefix,            &
     &      fdip_ctl%fdip_file_prefix_ctl)
        call read_chara_ctl_type(c_buf, hd_fdip_file_format,            &
     &      fdip_ctl%fdip_file_format_ctl)
      end do
      fdip_ctl%i_dipolarity_ctl = 1
!
      end subroutine read_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_dipolarity_ctl                               &
     &         (id_control, hd_block, fdip_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_dipolarity_control), intent(in) :: fdip_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(fdip_ctl%i_dipolarity_ctl .le. 0) return
!
      maxlen = len_trim(hd_fdip_truncation)
      maxlen = max(maxlen, len_trim(hd_fdip_file_prefix))
      maxlen = max(maxlen, len_trim(hd_fdip_file_format))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_i1(id_control, level,                    &
     &    hd_fdip_truncation, fdip_ctl%fdip_truncation_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_fdip_file_prefix,fdip_ctl%fdip_file_prefix_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_fdip_file_format, fdip_ctl%fdip_file_format_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_dipolarity_ctl(fdip_ctl)
!
      type(sph_dipolarity_control), intent(inout) :: fdip_ctl
!
!
      fdip_ctl%i_dipolarity_ctl = 0
!
      call dealloc_control_array_int(fdip_ctl%fdip_truncation_ctl)
      fdip_ctl%fdip_file_prefix_ctl%iflag = 0
      fdip_ctl%fdip_file_format_ctl%iflag = 0
!
      end subroutine dealloc_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_dipolarity
