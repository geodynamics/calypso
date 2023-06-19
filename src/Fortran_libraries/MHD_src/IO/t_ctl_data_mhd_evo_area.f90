!>@file   t_ctl_data_mhd_evo_area.f90
!!@brief  module t_ctl_data_mhd_evo_area
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of time integration flags
!!
!!@verbatim
!!      subroutine read_mhd_layer_ctl                                   &
!!     &         (id_control, hd_block, earea_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_evo_area_control), intent(inout) :: earea_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_mhd_layer_ctl                                  &
!!     &         (id_control, hd_block, earea_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_evo_area_control), intent(in) :: earea_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_ele_area_grp_ctl(earea_ctl)
!!        type(mhd_evo_area_control), intent(inout) :: earea_ctl
!!
!! ----------------------------------------------------------------------
!!
!!!!!!  physical values for time evolution !!!!!!!!!!!!!!!!!!
!! aviable valuables: velocity, temperature, magnetic_field
!!                    vector_potential, composition
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin time_evolution_ctl
!!      array time_evo_ctl   4
!!        time_evo_ctl  temperature
!!        time_evo_ctl  velocity
!!        time_evo_ctl  vector_potential
!!        time_evo_ctl  composition
!!      end array time_evo_ctl
!!    end  time_evolution_ctl
!!
!! !!!  setting for layers
!!
!!    begin layers_ctl
!!      array fluid_ele_grp    1
!!        fluid_ele_grp    outer_core
!!      end array fluid_ele_grp
!!
!!      array conduct_ele_grp    2
!!         conduct_ele_grp    inner_core
!!         conduct_ele_grp    outer_core
!!      end array conduct_ele_grp
!!    end  layers_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_mhd_evo_area
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use skip_comment_f
!
      implicit  none
!
      type mhd_evo_area_control
!>        Structure for list of element group for time evolution in fluid
!!@n       evo_fluid_group_ctl%num:   Number of groups
!!@n       evo_fluid_group_ctl%c_tbl: Name list of groups
       type(ctl_array_chara) :: evo_fluid_group_ctl
!
!>        Structure for list of element group for time evolution
!!              of magnettic field
!!@n       evo_conduct_group_ctl%num:   Number of groups
!!@n       evo_conduct_group_ctl%c_tbl: Name list of groups
        type(ctl_array_chara) :: evo_conduct_group_ctl
!
        integer (kind=kint) :: i_layers_ctl =    0
      end type mhd_evo_area_control
!
!   4th level for layers
!
      character(len=kchara), parameter, private                         &
     &        :: hd_fluid_grp =   'fluid_ele_grp'
      character(len=kchara), parameter, private                         &
     &        :: hd_conduct_grp = 'conduct_ele_grp'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_mhd_layer_ctl                                     &
     &         (id_control, hd_block, earea_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(earea_ctl%i_layers_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control,                          &
     &      hd_fluid_grp, earea_ctl%evo_fluid_group_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_conduct_grp, earea_ctl%evo_conduct_group_ctl, c_buf)
      end do
      earea_ctl%i_layers_ctl = 1
!
      end subroutine read_mhd_layer_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_mhd_layer_ctl                                    &
     &         (id_control, hd_block, earea_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_evo_area_control), intent(in) :: earea_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(earea_ctl%i_layers_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c1(id_control, level,                    &
     &    hd_fluid_grp, earea_ctl%evo_fluid_group_ctl)
      call write_control_array_c1(id_control, level,                    &
     &    hd_conduct_grp, earea_ctl%evo_conduct_group_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_mhd_layer_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_area_grp_ctl(earea_ctl)
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
!
      call dealloc_control_array_chara(earea_ctl%evo_fluid_group_ctl)
      call dealloc_control_array_chara(earea_ctl%evo_conduct_group_ctl)
      earea_ctl%i_layers_ctl = 0
!
      end subroutine dealloc_ele_area_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_evo_area
