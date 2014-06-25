!m_ctl_data_node_boundary.f90
!-------  module m_ctl_data_node_boundary ---------------------
!
!        programmed by H.Matsui
!
!!      subroutine deallocate_bc_velo_ctl
!!      subroutine deallocate_bc_velo_ctl
!!      subroutine deallocate_bc_press_ctl
!!      subroutine deallocate_bc_composit_ctl
!!      subroutine deallocate_bc_magne_ctl
!!      subroutine deallocate_bc_magne_p_ctl
!!      subroutine deallocate_bc_vect_p_ctl
!!      subroutine deallocate_bc_current_ctl
!!
!!      subroutine read_bc_4_node
!!
!! ------------------------------------------------------------------
!!   example
!!
!!    begin bc_4_node (or boundary_condition)
!!!!!!  boundary condition for temperature  !!!!!!!!!!!!!!!!!!!!!!!!
!!  available type:  fixed, file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_temperature   2
!!        bc_temperature  fixed ICB  1.000
!!        bc_temperature  fixed CMB  0.000
!!      end array bc_temperature
!!!!!!  boundary condition for velocity  !!!!!!!!!!!!!!!!!!!!!!!!
!!  available type
!!     fix_x,  fix_y,  fix_z
!!     file_x, file_y, file_z
!!     rot_x,  rot_y,  rot_z
!!       (Note: set all compornents of the rotation vector!!)
!!     free_slip_sph
!!     specitial (you have to define the B.C. in source file)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_velocity  7
!!        bc_velocity  fix_x  ICB     0.000
!!        bc_velocity  fix_y  ICB     0.000
!!        bc_velocity  fix_z  ICB     0.000
!!        bc_velocity  fix_x  CMB     0.000
!!        bc_velocity  fix_y  CMB     0.000
!!        bc_velocity  fix_z  CMB     0.000
!!        bc_velocity  fix_z  equator 0.000
!!      end array bc_velocity
!!!!!!  boundary condition for pressure  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available type:  fixed, file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_pressure  1
!!        bc_pressure  fixed Press  0.000
!!      end array bc_pressure
!!!!!!  boundary condition for dummy scalar  !!!!!!!!!!!!!!!!!!!!!!!
!!  available type:  fixed, file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_composition  1
!!        bc_composition fixed  Press  0.000
!!      end array bc_composition
!!!!!!  boundary condition for magnetic field  !!!!!!!!!!!!!!!!!!!!!
!!  available type
!!     fix_x,  fix_y,  fix_z
!!     file_x, file_y, file_z
!!     insulator (not recommend)
!!     sph
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_magnetic_field   2
!!        bc_magnetic_field  fix_x  equator     0.000
!!        bc_magnetic_field  fix_y  equator     0.000
!!        bc_magnetic_field  fix_x  infinity     0.000
!!        bc_magnetic_field  fix_y  infinity     0.000
!!        bc_magnetic_field  fix_z  infinity     0.000
!!      end array bc_magnetic_field
!!!!!!  boundary condition for magnetic potential  !!!!!!!!!!!!!!!!!
!!  available type:  fixed, file, sph
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_electric_potential   3
!!        bc_electric_potential fixed   Centre  0.000
!!        bc_electric_potential fixed  infinity  0.000
!!        bc_electric_potential fixed  equator   0.000
!!      end array bc_electric_potential
!!!!!!  boundary condition for vector potential  !!!!!!!!!!!!!!!!!!!
!!  available type
!!     fix_x,  fix_y,  fix_z
!!     file_x, file_y, file_z
!!     sph
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_vector_potential    6
!!        bc_vector_potential fix_x   infinity  0.000
!!        bc_vector_potential fix_y   infinity  0.000
!!        bc_vector_potential fix_z   infinity  0.000
!!        bc_vector_potential insulate_shell   ICB  0.000
!!        bc_vector_potential insulate_shell   CMB 0.000
!!        bc_vector_potential fix_z   equator   0.000
!!      end array bc_vector_potential
!!!!!!  boundary condition for current density !!!!!!!!!!!!!!!!!!!!!
!!  available type
!!     fix_x,  fix_y,  fix_z
!!     file_x, file_y, file_z
!!     insulator (not recommend)
!!     sph
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array bc_current   0
!!        bc_current  fix_x  infinity     0.000
!!        bc_current  fix_y  infinity     0.000
!!        bc_current  fix_z  infinity     0.000
!!      end array bc_current
!!    end  bc_4_node (or boundary_condition)
!!
!! ------------------------------------------------------------------
!
      module m_ctl_data_node_boundary
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!>      Structure for nodal boundary conditions for temperature
!!@n      node_bc_T_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_T_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_T_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_T_ctl
!
!>      Structure for nodal boundary conditions for velocity
!!@n      node_bc_U_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_U_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_U_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_U_ctl
!
!>      Structure for nodal boundary conditions for pressure
!!@n      node_bc_P_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_P_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_P_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_P_ctl
!
!>      Structure for nodal boundary conditions for composition variation
!!@n      node_bc_C_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_C_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_C_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_C_ctl
!
!>      Structure for nodal boundary conditions for magnetic field
!!@n      node_bc_B_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_B_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_B_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_B_ctl
!
!>      Structure for nodal boundary conditions
!!                           for magnetic scalar potential
!!@n      node_bc_MP_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_MP_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_MP_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_MP_ctl
!
!>      Structure for nodal boundary conditions
!!                           for magnetic vector potential
!!@n      node_bc_A_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_A_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_A_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_A_ctl
!
!>      Structure for nodal boundary conditions for current density
!!@n      node_bc_J_ctl%c1_tbl:  Type of boundary conditions
!!@n      node_bc_J_ctl%c2_tbl:  Node (radial) group name for boundary
!!@n      node_bc_J_ctl%vect:    boundary condition value
      type(ctl_array_c2r), save :: node_bc_J_ctl
!
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_node =          'bc_4_node'
      character(len=kchara), parameter                                  &
     &      :: hd_boundary_condition = 'boundary_condition'
      integer (kind=kint) :: i_bc_4_node =     0
!
!   4th level for nodal boundary
!
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_temp =    'bc_temperature'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_velo =    'bc_velocity'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_press =   'bc_pressure'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_composit = 'bc_composition'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_magne =    'bc_magnetic_field'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_mag_p =   'bc_electric_potential'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_vect_p =  'bc_vector_potential'
      character(len=kchara), parameter                                  &
     &        :: hd_n_bc_currect = 'bc_current'
!
      private :: hd_bc_4_node, hd_boundary_condition, i_bc_4_node
      private :: hd_n_bc_temp, hd_n_bc_velo, hd_n_bc_press
      private :: hd_n_bc_magne, hd_n_bc_mag_p, hd_n_bc_vect_p
      private :: hd_n_bc_composit, hd_n_bc_currect
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_temp_ctl
!
      call dealloc_control_array_c2_r(node_bc_T_ctl)
!
      end subroutine deallocate_bc_temp_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_velo_ctl
!
      call dealloc_control_array_c2_r(node_bc_U_ctl)
!
       end subroutine deallocate_bc_velo_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_press_ctl
!
      call dealloc_control_array_c2_r(node_bc_P_ctl)
!
      end subroutine deallocate_bc_press_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_composit_ctl
!
      call dealloc_control_array_c2_r(node_bc_C_ctl)
!
      end subroutine deallocate_bc_composit_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_magne_ctl
!
      call dealloc_control_array_c2_r(node_bc_B_ctl)
!
      end subroutine deallocate_bc_magne_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_magne_p_ctl
!
      call dealloc_control_array_c2_r(node_bc_MP_ctl)
!
      end subroutine deallocate_bc_magne_p_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_vect_p_ctl
!
      call dealloc_control_array_c2_r(node_bc_A_ctl)
!
      end subroutine deallocate_bc_vect_p_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_bc_current_ctl
!
      call dealloc_control_array_c2_r(node_bc_J_ctl)
!
      end subroutine deallocate_bc_current_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_bc_4_node
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(       right_begin_flag(hd_boundary_condition) .eq. 0          &
     &    .and. right_begin_flag(hd_bc_4_node).eq. 0) return
      if(i_bc_4_node .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_boundary_condition, i_bc_4_node)
        if(i_bc_4_node .gt. 0) exit
        call find_control_end_flag(hd_bc_4_node, i_bc_4_node)
        if(i_bc_4_node .gt. 0) exit
!
!
        call read_control_array_c2_r(hd_n_bc_temp, node_bc_T_ctl)
        call read_control_array_c2_r(hd_n_bc_velo, node_bc_U_ctl)
        call read_control_array_c2_r(hd_n_bc_press, node_bc_P_ctl)
        call read_control_array_c2_r(hd_n_bc_composit, node_bc_C_ctl)
        call read_control_array_c2_r(hd_n_bc_magne, node_bc_B_ctl)
        call read_control_array_c2_r(hd_n_bc_mag_p, node_bc_MP_ctl)
        call read_control_array_c2_r(hd_n_bc_vect_p, node_bc_A_ctl)
        call read_control_array_c2_r(hd_n_bc_currect, node_bc_J_ctl)
      end do
!
      end subroutine read_bc_4_node
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_node_boundary
