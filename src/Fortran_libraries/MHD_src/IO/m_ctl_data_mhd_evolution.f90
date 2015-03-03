!>@file   m_ctl_data_mhd_evolution.f90
!!@brief  module m_ctl_data_mhd_evolution
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control brog for field definition
!!
!!@verbatim
!!      subroutine dealloc_t_evo_name_ctl
!!      subroutine dealloc_ele_fl_grp_ctl
!!      subroutine dealloc_ele_cd_grp_ctl
!!
!!      subroutine read_mhd_time_evo_ctl
!!      subroutine read_mhd_layer_ctl
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
      module m_ctl_data_mhd_evolution
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
!>      Structure for list of field for time evolution
!!@n      t_evo_field_ctl%icou:  Read flag for 'time_evolution_ctl'
!!@n      t_evo_field_ctl%num:   Number of field
!!@n      t_evo_field_ctl%c_tbl: Name list of field
      type(ctl_array_chara), save :: t_evo_field_ctl
! 
!
!>      Structure for list of element group for time evolution in fluid
!!@n      evo_fluid_group_ctl%num:   Number of groups
!!@n      evo_fluid_group_ctl%c_tbl: Name list of groups
      type(ctl_array_chara), save :: evo_fluid_group_ctl
!
!>      Structure for list of element group for time evolution
!!              of magnettic field
!!@n      evo_conduct_group_ctl%num:   Number of groups
!!@n      evo_conduct_group_ctl%c_tbl: Name list of groups
      type(ctl_array_chara), save :: evo_conduct_group_ctl
!
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_evo =     'time_evolution_ctl'
      integer (kind=kint) :: i_time_evo =      0
!
      character(len=kchara), parameter :: hd_layers_ctl = 'layers_ctl'
      integer (kind=kint) :: i_layers_ctl =    0
!
!   4th level for time evolution
!
      character(len=kchara), parameter                                  &
     &        :: hd_t_evo_field = 'time_evo_ctl'
!
!   4th level for layers
!
      character(len=kchara), parameter                                  &
     &        :: hd_fluid_grp =   'fluid_ele_grp'
      character(len=kchara), parameter                                  &
     &        :: hd_conduct_grp = 'conduct_ele_grp'
!
      private :: hd_time_evo, hd_layers_ctl
      private :: i_time_evo,  i_layers_ctl
      private :: hd_t_evo_field
      private :: hd_fluid_grp, hd_conduct_grp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_t_evo_name_ctl
!
      call dealloc_control_array_chara(t_evo_field_ctl)
!
      end subroutine dealloc_t_evo_name_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_fl_grp_ctl
!
      call dealloc_control_array_chara(evo_fluid_group_ctl)
!
      end subroutine dealloc_ele_fl_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_cd_grp_ctl
!
      call dealloc_control_array_chara(evo_conduct_group_ctl)
!
      end subroutine dealloc_ele_cd_grp_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_mhd_time_evo_ctl
!
!
      if(right_begin_flag(hd_time_evo) .eq. 0) return
      if (i_time_evo .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_time_evo, i_time_evo)
        if(i_time_evo .gt. 0) exit
!
        call read_control_array_c1(hd_t_evo_field, t_evo_field_ctl)
      end do
!
      end subroutine read_mhd_time_evo_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_mhd_layer_ctl
!
!
      if(right_begin_flag(hd_layers_ctl) .eq. 0) return
      if (i_layers_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_layers_ctl, i_layers_ctl)
        if(i_layers_ctl .gt. 0) exit
!
        call read_control_array_c1                                      &
     &      (hd_fluid_grp, evo_fluid_group_ctl)
        call read_control_array_c1                                      &
     &      (hd_conduct_grp, evo_conduct_group_ctl)
      end do
!
      end subroutine read_mhd_layer_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_evolution
