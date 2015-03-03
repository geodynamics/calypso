!> @file  m_initial_field_control.f90
!!      module m_initial_field_control
!!
!! @author  H. Matsui
!! @date Programmed on July, 2001
!
!> @brief Control flags for initial data
!
!!@verbatim
!!      subroutine set_initial_field_id
!!@endverbatim
!
      module m_initial_field_control
!
      use m_precision
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
      character(len=kchara), parameter                                  &
     &                      :: dynamobench_0 = 'dynamo_benchmark_0'
!>      Label for dynamo benchmark Case 1
      character(len=kchara), parameter                                  &
     &                      :: dynamobench_1 = 'dynamo_benchmark_1'
!>      Label for dynamo benchmark Case 2
      character(len=kchara), parameter                                  &
     &                      :: dynamobench_2 = 'dynamo_benchmark_2'
!>      Label for  pseudo vacuume dynamo benchmar
      character(len=kchara), parameter                                  &
     &                      :: pseudo_bench = 'pseudo_vacuum_benchmark'
!
!>      Label for solid body rotation around x-axis
      character(len=kchara), parameter :: rotate_x = 'rotate_x'
!>      Label for solid body rotation around y-axis
      character(len=kchara), parameter :: rotate_y = 'rotate_y'
!>      Label for solid body rotation around z-axis
      character(len=kchara), parameter :: rotate_z = 'rotate_z'
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
!
!>     flag for the initial field
      integer(kind=kint) :: iflag_restart = i_rst_no_file
!
      private :: No_restart, From_restart
      private :: dynamobench_0, dynamobench_1, dynamobench_2
      private :: pseudo_bench, kinematic, liear_cv
      private :: rotate_x, rotate_y, rotate_z
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_field_id
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_t_step_parameter
      use m_ctl_data_mhd_evo_scheme
      use m_ctl_data_4_time_steps
      use skip_comment_f
!
!
      if (i_rst_flag .eq. 0) then
        e_message  = 'Set initial condition'
        call calypso_MPI_abort(ierr_evo, e_message)
      else
        if(     cmp_no_case(restart_flag_ctl, No_restart)               &
     &       .or. restart_flag_ctl .eq. '0') then
          iflag_restart = i_rst_no_file
        else if(cmp_no_case(restart_flag_ctl, From_restart)             &
     &       .or. restart_flag_ctl .eq. '1') then
          iflag_restart = i_rst_by_file
        else if(cmp_no_case(restart_flag_ctl, dynamobench_0)            &
     &       .or. restart_flag_ctl .eq. '-1') then
          iflag_restart = i_rst_dbench0
        else if(cmp_no_case(restart_flag_ctl, dynamobench_1)            &
     &       .or. restart_flag_ctl .eq. '-2') then
          iflag_restart = i_rst_dbench1
        else if(cmp_no_case(restart_flag_ctl, dynamobench_2)            &
     &       .or. restart_flag_ctl .eq. '-2') then
          iflag_restart = i_rst_dbench2
        else if(cmp_no_case(restart_flag_ctl, pseudo_bench)             &
     &       .or. restart_flag_ctl .eq. '-3') then
          iflag_restart = i_rst_dbench_qcv
        else if(cmp_no_case(restart_flag_ctl, rotate_x)                 &
     &       .or. restart_flag_ctl .eq. '-11') then
          iflag_restart = i_rst_rotate_x
        else if(cmp_no_case(restart_flag_ctl, rotate_y)                 &
     &       .or. restart_flag_ctl .eq. '-12') then
          iflag_restart = i_rst_rotate_y
        else if(cmp_no_case(restart_flag_ctl, rotate_z)                 &
     &       .or. restart_flag_ctl .eq. '-13') then
          iflag_restart = i_rst_rotate_z
        else if(cmp_no_case(restart_flag_ctl, kinematic)                &
     &       .or. restart_flag_ctl .eq. '20') then
          iflag_restart = i_rst_kinematic
        else if(cmp_no_case(restart_flag_ctl, liear_cv)                 &
     &       .or. restart_flag_ctl .eq. '-20') then
          iflag_restart = i_rst_licv
        end if
      end if
!
      if (iflag_restart .eq. i_rst_no_file) then
        if (time_init_ctl%iflag .eq. 0) then
          e_message  = 'Set initial time'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          time_init = time_init_ctl%realvalue
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_restart ',iflag_restart
        write(*,*) 'time_init ',time_init
      end if
!
      end subroutine set_initial_field_id
!
!-----------------------------------------------------------------------
!
      end module m_initial_field_control
