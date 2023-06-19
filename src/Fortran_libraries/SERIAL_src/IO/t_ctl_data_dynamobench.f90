!>@file   t_ctl_data_dynamobench.f90
!!        module t_ctl_data_dynamobench
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!>@brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine reset_ctl_data_dynamobench(dbench_ctl)
!!        type(dynamobench_control), intent(inout) :: dbench_ctl
!!      subroutine read_ctl_data_dynamobench                            &
!!     &         (id_control, hd_block, dbench_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(dynamobench_control), intent(inout) :: dbench_ctl
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_ctl_data_dynamobench                           &
!!     &         (id_control, hd_block, dbench_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(dynamobench_control), intent(in) :: dbench_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin dynamo_benchmark_data_ctl
!!    dynamo_benchmark_file_prefix     'monitor/dynamobench'
!!    dynamo_benchmark_file_format     'gzip'
!!
!!    detail_dynamobench_file_prefix   'monitor/detail_dbench'
!!    dynamobench_field_prefix         'monitor/dbench_field'
!!    dynamobench_spectr_prefix        'monitor/dbench_spectr'
!!
!!    nphi_mid_eq_ctl                   500
!!  end dynamo_benchmark_data_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_dynamobench
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
      type dynamobench_control
!>        Structure for dynanmo benchmark data file prefix
        type(read_character_item) :: dynamobench_file_ctl
!>        Structure for dynanmo benchmark data file prefix
        type(read_character_item) :: dynamobench_format_ctl
!
!>        Structure for detailed dynanmo benchmark data file prefix
        type(read_character_item) :: detailed_dbench_file_ctl
!>        Structure for dynanmo benchmark field data file prefix
        type(read_character_item) :: dbench_field_file_ctl
!>        Structure for dynanmo benchmark spectr data file prefix
        type(read_character_item) :: dbench_spectr_file_ctl
!
!>        Structure for Number of zonal points for benchamek check
        type(read_integer_item) :: nphi_mid_eq_ctl
!
        integer (kind = kint) :: i_dynamobench_ctl = 0
      end type dynamobench_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &    :: hd_dbench_prefix = 'dynamo_benchmark_file_prefix'
      character(len=kchara), parameter, private                         &
     &    :: hd_dbench_format = 'dynamo_benchmark_file_format'
!
      character(len=kchara), parameter, private                         &
     &    :: hd_dbench_detail_prefix = 'detail_dynamobench_file_prefix'
      character(len=kchara), parameter, private                         &
     &    :: hd_dbench_field_prefix =  'dynamobench_field_prefix'
      character(len=kchara), parameter, private                         &
     &    :: hd_dbench_spectr_prefix = 'dynamobench_spectr_prefix'
!
      character(len=kchara), parameter, private                         &
     &    :: hd_nphi_mid_eq = 'nphi_mid_eq_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reset_ctl_data_dynamobench(dbench_ctl)
!
      type(dynamobench_control), intent(inout) :: dbench_ctl
!
      dbench_ctl%dynamobench_file_ctl%iflag =   0
      dbench_ctl%dynamobench_format_ctl%iflag = 0
!
      dbench_ctl%detailed_dbench_file_ctl%iflag = 0
      dbench_ctl%dbench_field_file_ctl%iflag =    0
      dbench_ctl%dbench_spectr_file_ctl%iflag =   0
!
      dbench_ctl%nphi_mid_eq_ctl%iflag = 0
!
      dbench_ctl%i_dynamobench_ctl = 0
!
      end subroutine reset_ctl_data_dynamobench
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_dynamobench                              &
     &         (id_control, hd_block, dbench_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(dynamobench_control), intent(inout) :: dbench_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(dbench_ctl%i_dynamobench_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_dbench_prefix,               &
     &      dbench_ctl%dynamobench_file_ctl)
        call read_chara_ctl_type(c_buf, hd_dbench_format,               &
     &      dbench_ctl%dynamobench_format_ctl)
!
        call read_chara_ctl_type(c_buf, hd_dbench_detail_prefix,        &
     &      dbench_ctl%detailed_dbench_file_ctl)
        call read_chara_ctl_type(c_buf, hd_dbench_field_prefix,         &
     &      dbench_ctl%dbench_field_file_ctl)
        call read_chara_ctl_type(c_buf, hd_dbench_spectr_prefix,        &
     &      dbench_ctl%dbench_spectr_file_ctl)
!
        call read_integer_ctl_type(c_buf, hd_nphi_mid_eq,               &
     &      dbench_ctl%nphi_mid_eq_ctl)
      end do
      dbench_ctl%i_dynamobench_ctl = 1
!
      end subroutine read_ctl_data_dynamobench
!
! -----------------------------------------------------------------------
!
      subroutine write_ctl_data_dynamobench                             &
     &         (id_control, hd_block, dbench_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(dynamobench_control), intent(in) :: dbench_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(dbench_ctl%i_dynamobench_ctl .le. 0) return
!
      maxlen = len_trim(hd_dbench_prefix)
      maxlen = max(maxlen, len_trim(hd_dbench_format))
      maxlen = max(maxlen, len_trim(hd_dbench_detail_prefix))
      maxlen = max(maxlen, len_trim(hd_dbench_field_prefix))
      maxlen = max(maxlen, len_trim(hd_dbench_spectr_prefix))
      maxlen = max(maxlen, len_trim(hd_nphi_mid_eq))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_dbench_prefix, dbench_ctl%dynamobench_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_dbench_format, dbench_ctl%dynamobench_format_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_dbench_detail_prefix, dbench_ctl%detailed_dbench_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_dbench_field_prefix, dbench_ctl%dbench_field_file_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_dbench_spectr_prefix, dbench_ctl%dbench_spectr_file_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_nphi_mid_eq, dbench_ctl%nphi_mid_eq_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ctl_data_dynamobench
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_dynamobench
