!>@file   ctl_data_node_boundary_IO.f90
!!@brief  module ctl_data_node_boundary_IO
!!
!!@author H. Matsui
!>@brief   Control of nodal boundary conditions for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine init_bc_4_node_ctl_label(hd_block, nbc_ctl)
!!      subroutine read_bc_4_node_ctl                                   &
!!     &         (id_control, hd_block, nbc_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(node_bc_control), intent(inout) :: nbc_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_bc_4_node_ctl                                  &
!!     &         (id_control, hd_block, nbc_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(node_bc_control), intent(in) :: nbc_ctl
!!        integer(kind = kint), intent(inout) :: level
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
!!@endverbatim
!
      module ctl_data_node_boundary_IO
!
      use m_precision
      use m_machine_parameter
      use t_control_array_chara2real
      use t_ctl_data_node_boundary
!
      implicit  none
!
!   4th level for nodal boundary
!
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_temp =    'bc_temperature'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_velo =    'bc_velocity'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_press =   'bc_pressure'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_composit = 'bc_composition'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_magne =    'bc_magnetic_field'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_mag_p =   'bc_electric_potential'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_vect_p =  'bc_vector_potential'
      character(len=kchara), parameter, private                         &
     &        :: hd_n_bc_currect = 'bc_current'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_bc_4_node_ctl                                     &
     &         (id_control, hd_block, nbc_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(node_bc_control), intent(inout) :: nbc_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(nbc_ctl%i_bc_4_node .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_temp, nbc_ctl%node_bc_T_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_velo, nbc_ctl%node_bc_U_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_press, nbc_ctl%node_bc_P_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_composit, nbc_ctl%node_bc_C_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_magne, nbc_ctl%node_bc_B_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_mag_p, nbc_ctl%node_bc_MP_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_vect_p, nbc_ctl%node_bc_A_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_currect, nbc_ctl%node_bc_J_ctl, c_buf)
      end do
      nbc_ctl%i_bc_4_node = 1
!
      end subroutine read_bc_4_node_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_bc_4_node_ctl                                    &
     &         (id_control, hd_block, nbc_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(node_bc_control), intent(in) :: nbc_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(nbc_ctl%i_bc_4_node .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_T_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_U_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_P_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_C_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_B_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_MP_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_A_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    nbc_ctl%node_bc_J_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_bc_4_node_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_bc_4_node_ctl_label(hd_block, nbc_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(node_bc_control), intent(inout) :: nbc_ctl
!
!
      nbc_ctl%block_name = hd_block
!
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_temp, nbc_ctl%node_bc_T_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_velo, nbc_ctl%node_bc_U_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_press, nbc_ctl%node_bc_P_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_composit, nbc_ctl%node_bc_C_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_magne, nbc_ctl%node_bc_B_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_mag_p, nbc_ctl%node_bc_MP_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_vect_p, nbc_ctl%node_bc_A_ctl)
        call init_c2_r_ctl_array_label                                  &
     &     (hd_n_bc_currect, nbc_ctl%node_bc_J_ctl)
!
      end subroutine init_bc_4_node_ctl_label
!
!   --------------------------------------------------------------------
!
      end module ctl_data_node_boundary_IO
