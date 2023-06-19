!>@file   t_ctl_data_stratified_model.f90
!!@brief  module t_ctl_data_stratified_model
!!
!!@author H. Matsui
!>@brief   Control of reference temperature for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine read_takepiro_ctl                                    &
!!     &         (id_control, hd_block, takepiro_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(takepiro_model_control), intent(inout) :: takepiro_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_takepiro_ctl                                   &
!!     &         (id_control, hd_block, takepiro_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(takepiro_model_control), intent(in) :: takepiro_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine bcast_takepiro_ctl(takepiro_ctl)
!!      subroutine reset_takepiro_ctl(takepiro_ctl)
!!        type(takepiro_model_control), intent(inout) :: takepiro_ctl
!!
!!!!!!!!! model for stratification !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    stratified_ctl:   0...off  1...on
!!     stratified_sigma_ctl: intense ofstratification
!!     stratified_width_ctl: width of stratification
!!     stratified_outer_r_ctl: outer boundary of stratification
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      begin takepiro_model_ctl
!!        stratified_sigma_ctl         0.000   end
!!        stratified_width_ctl         0.000   end
!!        stratified_outer_r_ctl       0.000   end
!!      end  takepiro_model_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_stratified_model
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!
      type takepiro_model_control
        type(read_real_item) :: stratified_sigma_ctl
        type(read_real_item) :: stratified_width_ctl
        type(read_real_item) :: stratified_outer_r_ctl
!
        integer (kind=kint) :: i_takepiro_t_ctl = 0
      end type takepiro_model_control
!
!   5th level for temperature define
!
      character(len=kchara), parameter, private                         &
     &       :: hd_strat_sigma = 'stratified_sigma_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_strat_width = 'stratified_width_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_strat_outer = 'stratified_outer_r_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_takepiro_ctl                                      &
     &         (id_control, hd_block, takepiro_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(takepiro_model_control), intent(inout) :: takepiro_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(takepiro_ctl%i_takepiro_t_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_strat_sigma, takepiro_ctl%stratified_sigma_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_strat_width, takepiro_ctl%stratified_width_ctl)
        call read_real_ctl_type(c_buf, hd_strat_outer,                  &
     &      takepiro_ctl%stratified_outer_r_ctl)
      end do
      takepiro_ctl%i_takepiro_t_ctl = 1
!
      end subroutine read_takepiro_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_takepiro_ctl                                     &
     &         (id_control, hd_block, takepiro_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(takepiro_model_control), intent(in) :: takepiro_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(takepiro_ctl%i_takepiro_t_ctl .le. 0) return
!
      maxlen = len_trim(hd_strat_sigma)
      maxlen = max(maxlen, len_trim(hd_strat_width))
      maxlen = max(maxlen, len_trim(hd_strat_outer))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_strat_sigma, takepiro_ctl%stratified_sigma_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_strat_width, takepiro_ctl%stratified_width_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_strat_outer, takepiro_ctl%stratified_outer_r_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_takepiro_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine reset_takepiro_ctl(takepiro_ctl)
!
      type(takepiro_model_control), intent(inout) :: takepiro_ctl
!
      takepiro_ctl%stratified_sigma_ctl%iflag =   0
      takepiro_ctl%stratified_width_ctl%iflag =   0
      takepiro_ctl%stratified_outer_r_ctl%iflag = 0
!
      takepiro_ctl%i_takepiro_t_ctl = 0
!
      end subroutine reset_takepiro_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_stratified_model
