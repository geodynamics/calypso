!> @file  m_initial_field_control.f90
!!      module m_initial_field_control
!!
!! @author  H. Matsui
!! @date Programmed on July, 2001
!
!> @brief Control flags for initial data
!
      module m_initial_field_control
!
      use m_precision
!
!>      Start from zero except for temperature perturbation
      integer(kind=kint), parameter :: i_rst_no_file =  0
!>      Start from saved restart data
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
!>      Start from solid body rotation around x-axis
      integer(kind=kint), parameter :: i_rst_rotate_x =  -11
!>      Start from solid body rotation around y-axis
      integer(kind=kint), parameter :: i_rst_rotate_y =  -12
!>      Start from solid body rotation around z-axis
      integer(kind=kint), parameter :: i_rst_rotate_z =  -13
!
!>     Initial field for kinematic dynamo
      integer(kind=kint), parameter :: i_rst_kinematic =  20
!>     Initial field for linear convection model
      integer(kind=kint), parameter :: i_rst_licv =  -20
!
!>     flag for the initial field
      integer(kind=kint) :: iflag_restart = i_rst_no_file
!
      end module m_initial_field_control
