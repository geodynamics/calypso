!>@file   t_ctl_data_mhd_evolution.f90
!!@brief  module t_ctl_data_mhd_evolution
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of time integration flags
!!
!!@verbatim
!!      subroutine read_mhd_time_evo_ctl                                &
!!     &         (id_control, hd_block, evo_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_evolution_control), intent(inout) :: evo_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_mhd_time_evo_ctl                               &
!!     &         (id_control, hd_block, evo_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_evolution_control), intent(in) :: evo_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_t_evo_name_ctl(evo_ctl)
!!        type(mhd_evolution_control), intent(inout) :: evo_ctl
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
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
      subroutine write_mhd_time_evo_ctl                                 &
     &         (id_control, hd_block, evo_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_evolution_control), intent(in) :: evo_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(evo_ctl%i_time_evo .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c1(id_control, level,                    &
     &    hd_t_evo_field, evo_ctl%t_evo_field_ctl)
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_mhd_time_evo_ctl
!
!   --------------------------------------------------------------------
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
      end module t_ctl_data_mhd_evolution
