!> @file  m_initial_field_control.f90
!!      module m_initial_field_control
!!
!! @author  H. Matsui
!! @date Programmed on July, 2001
!
!> @brief Control flags for initial data
!
!!@verbatim
!!      subroutine set_initial_field_id(restart_flag_ctl,               &
!!     &                                iflag_restart_mode)
!!        type(read_character_item), intent(in) :: restart_flag_ctl
!!        integer(kind = kint), intent(inout) :: iflag_restart_mode
!!      subroutine check_set_initial_time(iflag_restart_mode, tctl,     &
!!     &                                  time_init)
!!        type(time_data_control), intent(in) :: tctl
!!        integer(kind = kint), intent(in) :: iflag_restart_mode
!!        real(kind = kreal), intent(inout) :: time_init
!!@endverbatim
!
      module m_initial_field_control
!
      use m_precision
      use m_error_IDs
      use m_machine_parameter
!
      implicit none
!
!>      Label for starting from zero fields
      character(len=kchara), parameter :: No_restart = 'no_data'
!>      Label for starting from saved restart data
      character(len=kchara), parameter                                  &
     &                      :: From_restart = 'start_from_rst_file'
!
!>      Label for dynamo benchmark Case 0
      character(len=kchara), parameter, private                         &
     &                      :: dynamobench_0 = 'dynamo_benchmark_0'
!>      Label for dynamo benchmark Case 1
      character(len=kchara), parameter, private                         &
     &                      :: dynamobench_1 = 'dynamo_benchmark_1'
!>      Label for dynamo benchmark Case 2
      character(len=kchara), parameter, private                         &
     &                      :: dynamobench_2 = 'dynamo_benchmark_2'
!>      Label for  pseudo vacuume dynamo benchmar
      character(len=kchara), parameter, private                         &
     &                      :: pseudo_bench = 'pseudo_vacuum_benchmark'
!
!>      Label to change initial zonal mode
!!           based on dynamo benchmark Case 0
      character(len=kchara), parameter, private                         &
     &          :: dynamobench_0_with_m = 'dynamo_benchmark_0_given_m'
!>      Label to change initial zonal mode
!!           based on dynamo benchmark Case 1
      character(len=kchara), parameter, private                         &
     &          :: dynamobench_1_with_m = 'dynamo_benchmark_1_given_m'
!>      Label to change initial zonal mode
!!           based on dynamo benchmark Case 2
      character(len=kchara), parameter, private                         &
     &          :: dynamobench_2_with_m = 'dynamo_benchmark_2_given_m'
!
!>      Label for solid body rotation around x-axis
      character(len=kchara), parameter, private                         &
     &                      :: rotate_x = 'rotate_x'
!>      Label for solid body rotation around y-axis
      character(len=kchara), parameter, private                         &
     &                      :: rotate_y = 'rotate_y'
!>      Label for solid body rotation around z-axis
      character(len=kchara), parameter, private                         &
     &                      :: rotate_z = 'rotate_z'
!
!>      Label for kinematic dynamo
      character(len=kchara), parameter :: kinematic = 'kinematic'
!>      Label for  linear convection model
      character(len=kchara), parameter                                  &
     &                      :: liear_cv = 'linear_conveciton'
!
!
!>      Start ID from zero fields
      integer(kind=kint), parameter :: i_rst_no_file =  0
!>      Start ID from saved restart data
      integer(kind=kint), parameter :: i_rst_by_file =  1
!
!>      Use initial field for  dynamo benchmark Case 0
      integer(kind=kint), parameter :: i_rst_dbench0 = -1
!>      Use initial field for  dynamo benchmark Case 1
      integer(kind=kint), parameter :: i_rst_dbench1 = -2
!>      Use initial field for  dynamo benchmark Case 2
      integer(kind=kint), parameter :: i_rst_dbench2 = -3
!
!>      initial data ID to set initial zonal mode
!!           based on dynamo benchmark Case 0
      integer(kind=kint), parameter :: i_rst_val_m_dbench0 = -100
!>      initial data ID to set initial zonal mode
!!           based on dynamo benchmark Case 1
      integer(kind=kint), parameter :: i_rst_val_m_dbench1 = -101
!>      initial data ID to set initial zonal mode
!!           based on dynamo benchmark Case 2
      integer(kind=kint), parameter :: i_rst_val_m_dbench2 = -102
!
!>      Use initial field for pseudo vacuume dynamo benchmark
      integer(kind=kint), parameter :: i_rst_dbench_qcv = -31
!
!>      Start ID from solid body rotation around x-axis
      integer(kind=kint), parameter :: i_rst_rotate_x =  -11
!>      Start ID from solid body rotation around y-axis
      integer(kind=kint), parameter :: i_rst_rotate_y =  -12
!>      Start ID from solid body rotation around z-axis
      integer(kind=kint), parameter :: i_rst_rotate_z =  -13
!
!>     Initial field for kinematic dynamo
      integer(kind=kint), parameter :: i_rst_kinematic =  20
!>     Initial field for linear convection model
      integer(kind=kint), parameter :: i_rst_licv =  -20
!
      private :: No_restart, From_restart
      private :: kinematic, liear_cv
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_field_id(restart_flag_ctl,                 &
     &                                iflag_restart_mode)
!
      use calypso_mpi
      use t_control_array_character
      use skip_comment_f
!
      type(read_character_item), intent(in) :: restart_flag_ctl
!
      integer(kind = kint), intent(inout) :: iflag_restart_mode
!
      character(len=kchara) :: tmpchara
!
!
      if(restart_flag_ctl%iflag .eq. 0) then
        e_message  = 'Set initial condition'
        call calypso_MPI_abort(ierr_evo, e_message)
      else
        tmpchara = restart_flag_ctl%charavalue
!
        if(     cmp_no_case(tmpchara, No_restart)                       &
     &       .or. tmpchara .eq. '0') then
          iflag_restart_mode = i_rst_no_file
        else if(cmp_no_case(tmpchara, From_restart)                     &
     &       .or. tmpchara .eq. '1') then
          iflag_restart_mode = i_rst_by_file
!
        else if(cmp_no_case(tmpchara, dynamobench_0)                    &
     &       .or. tmpchara .eq. '-0') then
          iflag_restart_mode = i_rst_dbench0
        else if(cmp_no_case(tmpchara, dynamobench_1)                    &
     &       .or. tmpchara .eq. '-1') then
          iflag_restart_mode = i_rst_dbench1
        else if(cmp_no_case(tmpchara, dynamobench_2)                    &
     &       .or. tmpchara .eq. '-2') then
          iflag_restart_mode = i_rst_dbench2
        else if(cmp_no_case(tmpchara, pseudo_bench)                     &
     &       .or. tmpchara .eq. '-3') then
          iflag_restart_mode = i_rst_dbench_qcv
!
        else if(cmp_no_case(tmpchara, dynamobench_0_with_m)             &
     &       .or. tmpchara .eq. '-100') then
          iflag_restart_mode = i_rst_val_m_dbench0
        else if(cmp_no_case(tmpchara, dynamobench_1_with_m)             &
     &       .or. tmpchara .eq. '-101') then
          iflag_restart_mode = i_rst_val_m_dbench1
        else if(cmp_no_case(tmpchara, dynamobench_2_with_m)             &
     &       .or. tmpchara .eq. '-102') then
          iflag_restart_mode = i_rst_val_m_dbench2
!
        else if(cmp_no_case(tmpchara, rotate_x)                         &
     &       .or. tmpchara .eq. '-11') then
          iflag_restart_mode = i_rst_rotate_x
        else if(cmp_no_case(tmpchara, rotate_y)                         &
     &       .or. tmpchara .eq. '-12') then
          iflag_restart_mode = i_rst_rotate_y
        else if(cmp_no_case(tmpchara, rotate_z)                         &
     &       .or. tmpchara .eq. '-13') then
          iflag_restart_mode = i_rst_rotate_z
        else if(cmp_no_case(tmpchara, kinematic)                        &
     &       .or. tmpchara .eq. '20') then
          iflag_restart_mode = i_rst_kinematic
        else if(cmp_no_case(tmpchara, liear_cv)                         &
     &       .or. tmpchara .eq. '-20') then
          iflag_restart_mode = i_rst_licv
        end if
      end if
!
      end subroutine set_initial_field_id
!
!-----------------------------------------------------------------------
!
      subroutine check_set_initial_time(iflag_restart_mode, tctl,       &
     &                                  time_init)
!
      use calypso_mpi
      use t_control_array_character
      use t_ctl_data_4_time_steps
      use skip_comment_f
!
      type(time_data_control), intent(in) :: tctl
      integer(kind = kint), intent(in) :: iflag_restart_mode
!
      real(kind = kreal), intent(inout) :: time_init
!
!
      if(iflag_restart_mode .eq. i_rst_no_file) then
        if(tctl%time_init_ctl%iflag .eq. 0) then
          e_message  = 'Set initial time'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          time_init = tctl%time_init_ctl%realvalue
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_restart_mode ',iflag_restart_mode
        write(*,*) 'time_init ',time_init
      end if
!
      end subroutine check_set_initial_time
!
!-----------------------------------------------------------------------
!
      end module m_initial_field_control
