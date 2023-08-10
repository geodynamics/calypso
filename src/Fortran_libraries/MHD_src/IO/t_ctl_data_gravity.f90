!>@file   t_ctl_data_gravity.f90
!!        module t_ctl_data_gravity
!!
!!@author H. Matsui
!!@date   Programmed in March, 2006
!!
!!
!> @brief Control data for gravity define
!!
!!@verbatim
!!      subroutine init_gravity_ctl_label(hd_block, g_ctl)
!!      subroutine read_gravity_ctl(id_control, hd_block, g_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(gravity_control), intent(inout) :: g_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_gravity_ctl(id_control, hd_block, g_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(gravity_control), intent(in) :: g_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_gravity_ctl(g_ctl)
!!        type(gravity_control), intent(inout) :: g_ctl
!!
!! !!!! gravity_type !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      0: constant
!!      1: constant_radial (constant intensity)
!!      2: radial (propotional to radius)
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin gravity_define
!!      FEM_gravity_model_ctl    element
!!      gravity_type_ctl          radial
!!
!! !!!! direction of gravity (opposite direction to that of buoyancy)
!!      array gravity_vec  3
!!        gravity_vec  x     0.000   end
!!        gravity_vec  y     0.000   end
!!        gravity_vec  z     -1.000   end
!!      end array gravity_vec
!!    end  gravity_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_gravity
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_charareal
      use skip_comment_f
!
      implicit  none
!
!
!>      Structure for gravity definistion
      type gravity_control
!>        Block name
        character(len=kchara) :: block_name = 'gravity_define'
!>        Coliolis force modeling in FEM
!!@n        element: Coriolis force in element
!!@n        node:    Coriolis force at node
        type(read_character_item) :: FEM_gravity_model
!>        Gravity type
        type(read_character_item) :: gravity
!
!>        Structure for constant gravity vector
!!@n        gravity_vector%c_tbl:  Direction of gravity vector
!!@n        gravity_vector%vect:   Amplitude of gravity vector
        type(ctl_array_cr) :: gravity_vector
!
        integer (kind=kint) :: i_gravity_ctl =   0
      end type gravity_control
!
!   4th level for gravity
      character(len=kchara), parameter, private                         &
     &        :: hd_FEM_gravity_mode = 'FEM_gravity_model_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_gravity_type = 'gravity_type_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_gravity_vect = 'gravity_vec'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_gravity_ctl(id_control, hd_block, g_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(gravity_control), intent(inout) :: g_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(g_ctl%i_gravity_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_gravity_vect, g_ctl%gravity_vector, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_gravity_type, g_ctl%gravity)
        call read_chara_ctl_type(c_buf, hd_FEM_gravity_mode,            &
     &      g_ctl%FEM_gravity_model)
      end do
      g_ctl%i_gravity_ctl = 1
!
      end subroutine read_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_gravity_ctl(id_control, hd_block, g_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(gravity_control), intent(in) :: g_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if( g_ctl%i_gravity_ctl .le. 0) return
!
      maxlen = len_trim(hd_FEM_gravity_mode)
      maxlen = max(maxlen, len_trim(hd_gravity_type))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    g_ctl%FEM_gravity_model)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    g_ctl%gravity)
!
      call write_control_array_c_r(id_control, level,                 &
     &    g_ctl%gravity_vector)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_gravity_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_gravity_ctl_label(hd_block, g_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(gravity_control), intent(inout) :: g_ctl
!
      g_ctl%block_name = hd_block
        call init_c_r_ctl_array_label                                   &
     &     (hd_gravity_vect, g_ctl%gravity_vector)
        call init_chara_ctl_item_label(hd_gravity_type, g_ctl%gravity)
        call init_chara_ctl_item_label(hd_FEM_gravity_mode,             &
     &      g_ctl%FEM_gravity_model)
!
      end subroutine init_gravity_ctl_label
!
!   --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_gravity_ctl(g_ctl)
!
      type(gravity_control), intent(inout) :: g_ctl
!
!
      call dealloc_control_array_c_r(g_ctl%gravity_vector)
      g_ctl%gravity%iflag = 0
      g_ctl%i_gravity_ctl = 0
!
      end subroutine dealloc_gravity_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gravity
