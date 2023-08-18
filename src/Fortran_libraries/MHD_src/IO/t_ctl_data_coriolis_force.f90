!>@file   t_ctl_data_coriolis_force.f90
!!        module t_ctl_data_coriolis_force
!!
!!@author H. Matsui
!!@date   Programmed in March, 2006
!!
!!
!> @brief Control data for magnetic field controls
!!
!!@verbatim
!!      subroutine init_coriolis_ctl_label(hd_block, cor_ctl)
!!      subroutine read_coriolis_ctl                                    &
!!     &         (id_control, hd_block, cor_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(coriolis_control), intent(inout) :: cor_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_coriolis_ctl                                   &
!!     &         (id_control, hd_block, cor_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(coriolis_control), intent(in) :: cor_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_coriolis_ctl(cor_ctl)
!!        type(coriolis_control), intent(inout) :: cor_ctl
!!
!! !!!! direction of rotation vector for Coriolis force !!!!!!!!!!!!!
!!
!!    begin Coriolis_define
!!      FEM_Coriolis_model_ctl       element
!!      FEM_Coriolis_implicit_ctl    On
!!
!!      array rotation_vec
!!        rotation_vec  x   0.000    end
!!        rotation_vec  y   0.000    end
!!        rotation_vec  z   1.000    end
!!      end array rotation_vec
!!    end  Coriolis_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_coriolis_force
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
!>      Structure for Coriolis force
      type coriolis_control
!>        Block name
        character(len=kchara) :: block_name = 'Coriolis_define'
!>        Coliolis force modeling in FEM
!!@n        element: Coriolis force in element
!!@n        node:    Coriolis force at node
        type(read_character_item) :: FEM_coriolis_model
!>        Use implicit scheme for Coliolis force modeling in FEM
!!@n        On: Coriolis force is solved implicitly
        type(read_character_item) :: FEM_coriolis_implicit
!>        Structure for rotation of system
!!@n        system_rotation%c_tbl:  Direction of rotation vector
!!@n        system_rotation%vect:   Amplitude of rotation vector
        type(ctl_array_cr) :: system_rotation
!
        integer (kind=kint) :: i_coriolis_ctl =  0
      end type coriolis_control
!
!   4th level for time steps
      character(len=kchara), parameter, private                         &
     &        :: hd_FEM_Coriolis_model = 'FEM_Coriolis_model_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_FEM_Coriolis_imp =   'FEM_Coriolis_implicit_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_rotation_vec =        'rotation_vec'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_coriolis_ctl                                      &
     &         (id_control, hd_block, cor_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(coriolis_control), intent(inout) :: cor_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cor_ctl%i_coriolis_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_FEM_Coriolis_model,          &
     &      cor_ctl%FEM_coriolis_model)
        call read_chara_ctl_type(c_buf, hd_FEM_Coriolis_imp,            &
     &      cor_ctl%FEM_coriolis_implicit)
!
        call read_control_array_c_r(id_control, hd_rotation_vec,        &
     &      cor_ctl%system_rotation, c_buf)
      end do
      cor_ctl%i_coriolis_ctl = 1
!
      end subroutine read_coriolis_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_coriolis_ctl                                     &
     &         (id_control, hd_block, cor_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(coriolis_control), intent(in) :: cor_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(cor_ctl%i_coriolis_ctl .le. 0) return
!
      maxlen = len_trim(hd_FEM_Coriolis_model)
      maxlen = max(maxlen, len_trim(hd_FEM_Coriolis_imp))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    cor_ctl%FEM_coriolis_model)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    cor_ctl%FEM_coriolis_implicit)
!
      call write_control_array_c_r(id_control, level,                   &
     &    cor_ctl%system_rotation)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_coriolis_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_coriolis_ctl_label(hd_block, cor_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(coriolis_control), intent(inout) :: cor_ctl
!
!
      cor_ctl%block_name = hd_block
        call init_chara_ctl_item_label(hd_FEM_Coriolis_model,           &
     &      cor_ctl%FEM_coriolis_model)
        call init_chara_ctl_item_label(hd_FEM_Coriolis_imp,             &
     &      cor_ctl%FEM_coriolis_implicit)
!
        call init_c_r_ctl_array_label(hd_rotation_vec,                  &
     &      cor_ctl%system_rotation)
      end subroutine init_coriolis_ctl_label
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_coriolis_ctl(cor_ctl)
!
      type(coriolis_control), intent(inout) :: cor_ctl
!
!
      call dealloc_control_array_c_r(cor_ctl%system_rotation)
      cor_ctl%i_coriolis_ctl = 0
!
      end subroutine dealloc_coriolis_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_coriolis_force
