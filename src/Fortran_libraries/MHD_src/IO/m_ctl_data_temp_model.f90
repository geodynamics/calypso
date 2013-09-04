!m_ctl_data_temp_model.f90
!      module m_ctl_data_temp_model
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine read_temp_def
!
!    begin temperature_define
!!!!!!!!! model for stratification !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    ref_temp_ctl: none           (No reference of temperature)
!                  spherical_shell ( for spherical shell model)
!                  linear_x        ( propotional to x-direction )
!                  linear_y        ( propotional to x-direction )
!                  linear_z        ( propotional to x-direction )
!
!
!    stratified_ctl:   0...off  1...on
!     stratified_sigma_ctl: intense ofstratification
!     stratified_width_ctl: width of stratification
!     stratified_outer_r_ctl: outer boundary of stratification
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      ref_temp_ctl              spherical_shell
!      begin low_temp_ctl
!           depth         1.5384615384615384
!           temperature   0.0d0
!      end  low_temp_ctl
!      begin high_temp_ctl
!           depth         0.5384615384615384
!           temperature   1.0d0
!      end  high_temp_ctl
!
!      stratified_ctl            Off
!      stratified_sigma_ctl         0.000   end
!      stratified_width_ctl         0.000   end
!      stratified_outer_r_ctl       0.000   end
!    end  temperature_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      module m_ctl_data_temp_model
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      character (len=kchara)  :: ref_temp_ctl
      real (kind = kreal) :: low_temp_ctl
      real (kind = kreal) :: high_temp_ctl
      real (kind = kreal) :: depth_low_t_ctl
      real (kind = kreal) :: depth_high_t_ctl
!
      character (len=kchara) :: stratified_ctl
      real (kind = kreal) :: stratified_sigma_ctl
      real (kind = kreal) :: stratified_width_ctl
      real (kind = kreal) :: stratified_outer_r_ctl
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_temp_def =     'temperature_define'
      integer (kind=kint) :: i_temp_def =      0
!
!   4th level for temperature define
!
      character(len=kchara), parameter                                  &
     &       :: hd_ref_temp =    'ref_temp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_low_temp =    'low_temp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_high_temp =   'high_temp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_strat_ctl =   'stratified_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_strat_sigma = 'stratified_sigma_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_strat_width = 'stratified_width_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_strat_outer = 'stratified_outer_r_ctl'
!
      integer (kind=kint) :: i_ref_temp = 0
      integer (kind=kint) :: i_low_temp = 0
      integer (kind=kint) :: i_high_temp = 0
      integer (kind=kint) :: i_strat_ctl = 0
      integer (kind=kint) :: i_strat_sigma = 0
      integer (kind=kint) :: i_strat_width = 0
      integer (kind=kint) :: i_strat_outer = 0
!
!    5th level for lower temp position
!
      character(len=kchara), parameter                                  &
     &       :: hd_low_temp_posi =  'depth'
      character(len=kchara), parameter                                  &
     &       :: hd_low_temp_value = 'temperature'
      integer (kind=kint) :: i_low_temp_posi = 0
      integer (kind=kint) :: i_low_temp_value = 0
!
!    5th level for higher temp position
!
      character(len=kchara), parameter                                  &
     &       :: hd_high_temp_posi =  'depth'
      character(len=kchara), parameter                                  &
     &       :: hd_high_temp_value = 'temperature'
      integer (kind=kint) :: i_high_temp_posi = 0
      integer (kind=kint) :: i_high_temp_value = 0
!
      private :: hd_temp_def, i_temp_def
      private :: hd_ref_temp, hd_low_temp, hd_high_temp
      private :: hd_strat_ctl, hd_strat_sigma
      private :: hd_strat_width, hd_strat_outer
      private :: hd_low_temp_posi, hd_low_temp_value
      private :: hd_high_temp_posi, hd_high_temp_value
      private :: i_low_temp, i_high_temp
!
      private :: read_low_temp_ctl, read_high_temp_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_temp_def
!
!
      if(right_begin_flag(hd_temp_def) .eq. 0) return
      if (i_temp_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_temp_def, i_temp_def)
        if(i_temp_def .gt. 0) exit
!
!
        call  read_low_temp_ctl
        call  read_high_temp_ctl
!
!
        call read_character_ctl_item(hd_ref_temp,                       &
     &          i_ref_temp, ref_temp_ctl)
        call read_character_ctl_item(hd_strat_ctl,                      &
     &        i_strat_ctl, stratified_ctl)
!
        call read_real_ctl_item(hd_strat_sigma,                         &
     &        i_strat_sigma, stratified_sigma_ctl)
        call read_real_ctl_item(hd_strat_width,                         &
     &        i_strat_width, stratified_width_ctl)
        call read_real_ctl_item(hd_strat_outer,                         &
     &        i_strat_outer, stratified_outer_r_ctl)
      end do
!
      end subroutine read_temp_def
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_low_temp_ctl
!
!
      if(right_begin_flag(hd_low_temp) .eq. 0) return
      if (i_low_temp .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_low_temp, i_low_temp)
        if(i_low_temp .gt. 0) exit
!
        call read_real_ctl_item(hd_low_temp_posi,                       &
     &        i_low_temp_posi, depth_low_t_ctl)
        call read_real_ctl_item(hd_low_temp_value,                      &
     &        i_low_temp_value, low_temp_ctl)
      end do
!
      end subroutine read_low_temp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_high_temp_ctl
!
!
      if(right_begin_flag(hd_high_temp) .eq. 0) return
      if (i_high_temp .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_high_temp, i_high_temp)
        if(i_high_temp .gt. 0) exit
!
        call read_real_ctl_item(hd_high_temp_posi,                      &
     &        i_high_temp_posi, depth_high_t_ctl)
        call read_real_ctl_item(hd_high_temp_value,                     &
     &        i_high_temp_value, high_temp_ctl)
      end do
!
      end subroutine read_high_temp_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_temp_model
