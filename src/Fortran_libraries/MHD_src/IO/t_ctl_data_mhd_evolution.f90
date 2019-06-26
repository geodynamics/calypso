!>@file   t_ctl_data_mhd_evolution.f90
!!@brief  module t_ctl_data_mhd_evolution
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of time integration flags
!!
!!@verbatim
!!      subroutine dealloc_t_evo_name_ctl(evo_ctl)
!!      subroutine read_mhd_time_evo_ctl                                &
!!     &         (id_control, hd_block, evo_ctl, c_buf)
!!      subroutine bcast_mhd_time_evo_ctl(hd_block, iflag, evo_ctl)
!!      subroutine bcast_mhd_time_evo_ctl(evo_ctl)
!!        type(mhd_evolution_control), intent(inout) :: evo_ctl
!!
!!      subroutine dealloc_ele_area_grp_ctl(earea_ctl)
!!      subroutine read_mhd_layer_ctl                                   &
!!     &         (id_control, hd_block, earea_ctl, c_buf)
!!      subroutine bcast_mhd_layer_ctl(earea_ctl)
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
      module t_ctl_data_mhd_evolution
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
!
      type mhd_evolution_control
!>        Structure for list of field for time evolution
!!@n       t_evo_field_ctl%icou:  Read flag for 'time_evolution_ctl'
!!@n       t_evo_field_ctl%num:   Number of field
!!@n       t_evo_field_ctl%c_tbl: Name list of field
        type(ctl_array_chara) :: t_evo_field_ctl
!
        integer (kind=kint) :: i_time_evo =      0
      end type mhd_evolution_control
! 
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
      private :: hd_t_evo_field, hd_fluid_grp, hd_conduct_grp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_t_evo_name_ctl(evo_ctl)
!
      type(mhd_evolution_control), intent(inout) :: evo_ctl
!
      call dealloc_control_array_chara(evo_ctl%t_evo_field_ctl)
      evo_ctl%i_time_evo = 0
!
      end subroutine dealloc_t_evo_name_ctl
!
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
!   --------------------------------------------------------------------
!
      subroutine read_mhd_time_evo_ctl                                  &
     &         (id_control, hd_block, evo_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_evolution_control), intent(inout) :: evo_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(evo_ctl%i_time_evo .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control,                          &
     &      hd_t_evo_field, evo_ctl%t_evo_field_ctl, c_buf)
      end do
      evo_ctl%i_time_evo = 1
!
      end subroutine read_mhd_time_evo_ctl
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
        call load_one_line_from_control(id_control, c_buf)
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
!   --------------------------------------------------------------------
!
      subroutine bcast_mhd_time_evo_ctl(evo_ctl)
!
      use bcast_control_arrays
!
      type(mhd_evolution_control), intent(inout) :: evo_ctl
!
!
      call bcast_ctl_array_c1(evo_ctl%t_evo_field_ctl)
!
      call MPI_BCAST(evo_ctl%i_time_evo, 1,                             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_mhd_time_evo_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_mhd_layer_ctl(earea_ctl)
!
      use bcast_control_arrays
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
!
!
      call bcast_ctl_array_c1(earea_ctl%evo_fluid_group_ctl)
      call bcast_ctl_array_c1(earea_ctl%evo_conduct_group_ctl)
!
      call MPI_BCAST(earea_ctl%i_layers_ctl, 1,                         &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_mhd_layer_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_evolution
