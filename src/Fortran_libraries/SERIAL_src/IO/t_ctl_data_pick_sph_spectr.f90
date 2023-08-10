!>@file   t_ctl_data_pick_sph_spectr.f90
!!        module t_ctl_data_pick_sph_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2016
!!
!!
!> @brief Control data for spectr data monitoring
!!
!!@verbatim
!!      subroutine dealloc_pick_spectr_control(pspec_ctl)
!!      subroutine init_pickup_spectr_ctl_labels(hd_block, pspec_ctl)
!!      subroutine read_pickup_spectr_ctl                               &
!!     &         (id_control, hd_block, pspec_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pick_spectr_control), intent(inout) :: pspec_ctl
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_pickup_spectr_ctl(id_control, pspec_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pick_spectr_control), intent(in) :: pspec_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spectr data
!!
!!  begin pickup_spectr_ctl
!!    picked_sph_prefix            'sph_spectr/picked_mode'
!!    picked_sph_format            'gzip' or 'ASCII'
!!!
!!!     if pick_layer_ctl = 0 or negative:
!!!           output all layer and volume average
!!    array pick_layer_ctl
!!      pick_layer_ctl  62
!!    end array pick_layer_ctl
!!
!!    array pick_radius_ctl
!!       pick_radius_ctl   0.5385
!!       pick_radius_ctl   1.03846
!!       pick_radius_ctl   1.5384
!!    end array pick_layer_ctl
!!
!!
!!    array pick_sph_spectr_ctl
!!      pick_sph_spectr_ctl   2  -2
!!      pick_sph_spectr_ctl   2   2
!!    end array pick_sph_spectr_ctl
!!
!!    array pick_sph_degree_ctl
!!      pick_sph_degree_ctl   2
!!      pick_sph_degree_ctl   2
!!    end array pick_sph_degree_ctl
!!
!!    array pick_sph_order_ctl
!!      pick_sph_order_ctl  -2
!!      pick_sph_order_ctl   2
!!    end array pick_sph_order_ctl
!!  end pickup_spectr_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_pick_sph_spectr
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_integer2
      use skip_comment_f
!
      implicit  none
!
!
!>      Structure for spectr data pickup
      type pick_spectr_control
!>        Block name
        character(len=kchara) :: block_name = 'pickup_spectr_ctl'
!>        Structure for picked spectrum file prefix
        type(read_character_item) :: picked_mode_head_ctl
!>        Structure for picked spectrum file format (ascii or gzip)
        type(read_character_item) :: picked_mode_fmt_ctl
!
!>        Structure for list of radial grid of spectr data output
!!@n        idx_pick_layer_ctl%num:   Number of grid
!!@n        idx_pick_layer_ctl%ivec: list of radial ID of spectr data
        type(ctl_array_int) ::  idx_pick_layer_ctl
!>        Structure for list of radial grid of spectr data output
!!@n        pick_radius_ctl%num:   Number of grid
!!@n        pick_radius_ctl%ivec: list of radius of spectr data
        type(ctl_array_real) :: pick_radius_ctl
!
!
!>        Structure for list of mode of spectr data output
!!@n        idx_pick_sph_ctl%num:   Number of mode
!!@n        idx_pick_sph_ctl%int1: list of degree of spectr data
!!@n        idx_pick_sph_ctl%int2: list of order of spectr data
        type(ctl_array_i2) :: idx_pick_sph_ctl
!
!>        Structure for list of degree of spectr data output
!!@n        idx_pick_sph_l_ctl%num:   Number of degree
!!@n        idx_pick_sph_l_ctl%ivec: list of degree of spectr data
        type(ctl_array_int) :: idx_pick_sph_l_ctl
!
!>        Structure for list of order of spectr data output
!!@n        idx_pick_sph_m_ctl%num:   Number of order
!!@n        idx_pick_sph_m_ctl%ivec: list of order of spectr data
        type(ctl_array_int) :: idx_pick_sph_m_ctl
!
        integer (kind = kint) :: i_pick_sph = 0
      end type pick_spectr_control
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_picked_mode_head =   'picked_sph_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_picked_mode_format = 'picked_sph_format'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_pick_layer =   'pick_layer_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_pick_radius =  'pick_radius_ctl'
!
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_lm =   'pick_sph_spectr_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_l =     'pick_sph_degree_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_m =     'pick_sph_order_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_spectr_control(pspec_ctl)
!
      type(pick_spectr_control), intent(inout) :: pspec_ctl
!
!
      call dealloc_control_array_i2(pspec_ctl%idx_pick_sph_ctl)
      call dealloc_control_array_int(pspec_ctl%idx_pick_sph_m_ctl)
      call dealloc_control_array_int(pspec_ctl%idx_pick_sph_l_ctl)
      call dealloc_control_array_int(pspec_ctl%idx_pick_layer_ctl)
      call dealloc_control_array_real(pspec_ctl%pick_radius_ctl)
!
      pspec_ctl%picked_mode_head_ctl%iflag = 0
      pspec_ctl%picked_mode_fmt_ctl%iflag =  0
      pspec_ctl%i_pick_sph = 0
!
      end subroutine dealloc_pick_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine read_pickup_spectr_ctl                                 &
     &         (id_control, hd_block, pspec_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pick_spectr_control), intent(inout) :: pspec_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(pspec_ctl%i_pick_sph .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_i1(id_control,                          &
     &      hd_pick_layer, pspec_ctl%idx_pick_layer_ctl, c_buf)
        call read_control_array_r1(id_control,                          &
     &      hd_pick_radius, pspec_ctl%pick_radius_ctl, c_buf)
!
        call read_control_array_i2(id_control,                          &
     &      hd_pick_sph_lm, pspec_ctl%idx_pick_sph_ctl, c_buf)
        call read_control_array_i1(id_control,                          &
     &      hd_pick_sph_l, pspec_ctl%idx_pick_sph_l_ctl, c_buf)
        call read_control_array_i1(id_control,                          &
     &      hd_pick_sph_m, pspec_ctl%idx_pick_sph_m_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_picked_mode_head,            &
     &      pspec_ctl%picked_mode_head_ctl)
        call read_chara_ctl_type(c_buf, hd_picked_mode_format,          &
     &      pspec_ctl%picked_mode_fmt_ctl)
      end do
      pspec_ctl%i_pick_sph = 1
!
      end subroutine read_pickup_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_pickup_spectr_ctl(id_control, pspec_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(pick_spectr_control), intent(in) :: pspec_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pspec_ctl%i_pick_sph .le. 0) return
!
      maxlen = len_trim(hd_picked_mode_head)
      maxlen = max(maxlen, len_trim(hd_picked_mode_format))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 pspec_ctl%block_name)
      call write_control_array_i1(id_control, level,                    &
     &    pspec_ctl%idx_pick_layer_ctl)
      call write_control_array_r1(id_control, level,                    &
     &    pspec_ctl%pick_radius_ctl)
!
      call write_control_array_i2(id_control, level,                    &
     &    pspec_ctl%idx_pick_sph_ctl)
      call write_control_array_i1(id_control, level,                    &
     &    pspec_ctl%idx_pick_sph_l_ctl)
      call write_control_array_i1(id_control, level,                    &
     &    pspec_ctl%idx_pick_sph_m_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pspec_ctl%picked_mode_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pspec_ctl%picked_mode_fmt_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                pspec_ctl%block_name)
!
      end subroutine write_pickup_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_pickup_spectr_ctl_labels(hd_block, pspec_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(pick_spectr_control), intent(inout) :: pspec_ctl
!
      pspec_ctl%block_name = hd_block
!
        call init_int_ctl_array_label                                   &
     &     (hd_pick_layer, pspec_ctl%idx_pick_layer_ctl)
        call init_real_ctl_array_label                                  &
     &     (hd_pick_radius, pspec_ctl%pick_radius_ctl)
!
        call init_int2_ctl_array_label                                  &
     &     (hd_pick_sph_lm, pspec_ctl%idx_pick_sph_ctl)
        call init_int_ctl_array_label                                   &
     &     (hd_pick_sph_l, pspec_ctl%idx_pick_sph_l_ctl)
        call init_int_ctl_array_label                                   &
     &     (hd_pick_sph_m, pspec_ctl%idx_pick_sph_m_ctl)
!
        call init_chara_ctl_item_label(hd_picked_mode_head,             &
     &      pspec_ctl%picked_mode_head_ctl)
        call init_chara_ctl_item_label(hd_picked_mode_format,           &
     &      pspec_ctl%picked_mode_fmt_ctl)
!
      end subroutine  init_pickup_spectr_ctl_labels
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_pick_sph_spectr
