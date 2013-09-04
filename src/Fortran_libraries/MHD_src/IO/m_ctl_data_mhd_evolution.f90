!m_ctl_data_mhd_evolution.f90
!      module m_ctl_data_mhd_evolution
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine dealloc_t_evo_name_ctl
!!      subroutine dealloc_ele_fl_grp_ctl
!!      subroutine dealloc_ele_cd_grp_ctl
!!
!!      subroutine read_mhd_time_evo_ctl
!!      subroutine read_mhd_layer_ctl
!!
!! -----------------------------------------------------------------------
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
!
      module m_ctl_data_mhd_evolution
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
      integer (kind=kint)  :: num_t_evo_control_ctl = 0
      character (len=kchara), allocatable :: t_evo_name_ctl(:)
! 
!
      integer (kind=kint)  :: num_ele_fl_grp_ctl = 0
      character (len=kchara), allocatable :: ele_fl_grp_ctl(:)
!
      integer (kind=kint)  :: num_ele_cd_grp_ctl = 0
      character (len=kchara), allocatable :: ele_cd_grp_ctl(:)
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
     &        :: hd_num_time_evo = 'time_evo_ctl'
      integer (kind=kint) :: i_num_time_evo = 0
!
!   4th level for layers
!
      character(len=kchara), parameter                                  &
     &        :: hd_fluid_grp =   'fluid_ele_grp'
      character(len=kchara), parameter                                  &
     &        :: hd_conduct_grp = 'conduct_ele_grp'
      integer (kind=kint) :: i_fluid_grp =   0
      integer (kind=kint) :: i_conduct_grp = 0
!
      private :: hd_time_evo, hd_layers_ctl
      private :: i_time_evo,  i_layers_ctl
      private :: hd_num_time_evo
      private :: hd_fluid_grp, hd_conduct_grp
      private :: alloc_t_evo_name_ctl
      private :: alloc_ele_fl_grp_ctl, alloc_ele_cd_grp_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_t_evo_name_ctl
!
      allocate ( t_evo_name_ctl(num_t_evo_control_ctl) )
!
      end subroutine alloc_t_evo_name_ctl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_ele_fl_grp_ctl
!
      allocate ( ele_fl_grp_ctl(num_ele_fl_grp_ctl) )
!
      end subroutine alloc_ele_fl_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_ele_cd_grp_ctl
!
      allocate ( ele_cd_grp_ctl(num_ele_cd_grp_ctl) )
!
      end subroutine alloc_ele_cd_grp_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_t_evo_name_ctl
!
      deallocate ( t_evo_name_ctl )
!
      end subroutine dealloc_t_evo_name_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_fl_grp_ctl
!
      deallocate ( ele_fl_grp_ctl )
!
      end subroutine dealloc_ele_fl_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_cd_grp_ctl
!
      deallocate ( ele_cd_grp_ctl )
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
        call find_control_array_flag(hd_num_time_evo,                   &
     &      num_t_evo_control_ctl)
        if(num_t_evo_control_ctl.gt.0 .and. i_num_time_evo.eq.0) then
          call alloc_t_evo_name_ctl
          call read_control_array_chara_list(hd_num_time_evo,           &
     &       num_t_evo_control_ctl, i_num_time_evo, t_evo_name_ctl)
        end if
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
        call find_control_array_flag(hd_fluid_grp, num_ele_fl_grp_ctl)
        if(num_ele_fl_grp_ctl.gt.0 .and. i_fluid_grp.eq.0) then
          call alloc_ele_fl_grp_ctl
          call read_control_array_chara_list(hd_fluid_grp,              &
     &       num_ele_fl_grp_ctl, i_fluid_grp, ele_fl_grp_ctl)
        end if
!
        call find_control_array_flag(hd_conduct_grp,                    &
     &      num_ele_cd_grp_ctl)
        if(num_ele_cd_grp_ctl.gt.0 .and. i_conduct_grp.eq.0) then
          call alloc_ele_cd_grp_ctl
          call read_control_array_chara_list(hd_conduct_grp,            &
     &       num_ele_cd_grp_ctl, i_conduct_grp, ele_cd_grp_ctl)
        end if
!
      end do
!
      end subroutine read_mhd_layer_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_evolution
