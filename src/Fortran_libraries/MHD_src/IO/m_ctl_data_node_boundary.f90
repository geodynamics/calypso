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
!
      implicit  none
!
!
      integer(kind=kint) :: num_bc_e_ctl = 0
      character (len=kchara), allocatable :: bc_e_name_ctl(:)
      character (len=kchara), allocatable :: bc_e_type_ctl(:)
      real (kind=kreal), allocatable :: bc_e_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_v_ctl = 0
      character (len=kchara), allocatable :: bc_v_name_ctl(:)
      character (len=kchara), allocatable :: bc_v_type_ctl(:)
      real (kind=kreal), allocatable :: bc_v_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_p_ctl = 0
      character (len=kchara), allocatable :: bc_p_name_ctl(:)
      character (len=kchara), allocatable :: bc_p_type_ctl(:)
      real (kind=kreal), allocatable :: bc_p_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_composit_ctl = 0
      character (len=kchara), allocatable :: bc_composit_name_ctl(:)
      character (len=kchara), allocatable :: bc_composit_type_ctl(:)
      real (kind=kreal), allocatable :: bc_composit_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_b_ctl = 0
      character (len=kchara), allocatable :: bc_b_name_ctl(:)
      character (len=kchara), allocatable :: bc_b_type_ctl(:)
      real (kind=kreal), allocatable :: bc_b_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_mag_p_ctl = 0
      character (len=kchara), allocatable :: bc_mag_p_name_ctl(:)
      character (len=kchara), allocatable :: bc_mag_p_type_ctl(:)
      real (kind=kreal), allocatable :: bc_mag_p_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_vp_ctl = 0
      character (len=kchara), allocatable :: bc_vp_name_ctl(:)
      character (len=kchara), allocatable :: bc_vp_type_ctl(:)
      real (kind=kreal), allocatable :: bc_vp_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_j_ctl = 0
      character (len=kchara), allocatable :: bc_j_name_ctl(:)
      character (len=kchara), allocatable :: bc_j_type_ctl(:)
      real (kind=kreal), allocatable :: bc_j_magnitude_ctl(:)
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
      integer (kind=kint) :: i_n_bc_temp =     0
      integer (kind=kint) :: i_n_bc_velo =     0
      integer (kind=kint) :: i_n_bc_press =    0
      integer (kind=kint) :: i_n_bc_composit = 0
      integer (kind=kint) :: i_n_bc_magne =    0
      integer (kind=kint) :: i_n_bc_mag_p =    0
      integer (kind=kint) :: i_n_bc_vect_p =   0
      integer (kind=kint) :: i_n_bc_currect =  0
!
      private :: hd_bc_4_node, hd_boundary_condition, i_bc_4_node
      private :: hd_n_bc_temp, hd_n_bc_velo, hd_n_bc_press
      private :: hd_n_bc_magne, hd_n_bc_mag_p, hd_n_bc_vect_p
      private :: hd_n_bc_composit, hd_n_bc_currect
!
      private :: allocate_bc_temp_ctl, allocate_bc_composit_ctl
      private :: allocate_bc_velo_ctl, allocate_bc_press_ctl
      private :: allocate_bc_magne_ctl, allocate_bc_magne_p_ctl
      private :: allocate_bc_vect_p_ctl, allocate_bc_current_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_temp_ctl
!
        allocate(bc_e_magnitude_ctl(num_bc_e_ctl))
        allocate(bc_e_type_ctl(num_bc_e_ctl))
        allocate(bc_e_name_ctl(num_bc_e_ctl))
        if(num_bc_e_ctl .gt. 0) bc_e_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_temp_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_velo_ctl
!
        allocate(bc_v_magnitude_ctl(num_bc_v_ctl))
        allocate(bc_v_name_ctl(num_bc_v_ctl))
        allocate(bc_v_type_ctl(num_bc_v_ctl))
        if(num_bc_v_ctl .gt. 0) bc_v_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_velo_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_press_ctl
!
        allocate(bc_p_magnitude_ctl( num_bc_p_ctl))
        allocate(bc_p_name_ctl(num_bc_p_ctl))
        allocate(bc_p_type_ctl(num_bc_p_ctl))
        if(num_bc_p_ctl .gt. 0) bc_p_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_press_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_composit_ctl
!
        allocate(bc_composit_magnitude_ctl( num_bc_composit_ctl))
        allocate(bc_composit_name_ctl(num_bc_composit_ctl))
        allocate(bc_composit_type_ctl(num_bc_composit_ctl))
        if(num_bc_composit_ctl .gt. 0)                                 &
     &               bc_composit_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_composit_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne_ctl
!
        allocate(bc_b_magnitude_ctl( num_bc_b_ctl))
        allocate(bc_b_name_ctl(num_bc_b_ctl))
        allocate(bc_b_type_ctl(num_bc_b_ctl))
        if(num_bc_b_ctl .gt. 0) bc_b_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_magne_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne_p_ctl
!
        allocate(bc_mag_p_magnitude_ctl( num_bc_mag_p_ctl))
        allocate(bc_mag_p_name_ctl(num_bc_mag_p_ctl))
        allocate(bc_mag_p_type_ctl(num_bc_mag_p_ctl))
        if(num_bc_mag_p_ctl .gt. 0) bc_mag_p_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_magne_p_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vect_p_ctl
!
        allocate(bc_vp_magnitude_ctl( num_bc_vp_ctl))
        allocate(bc_vp_name_ctl(num_bc_vp_ctl))
        allocate(bc_vp_type_ctl(num_bc_vp_ctl))
        if(num_bc_vp_ctl .gt. 0) bc_vp_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_vect_p_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_current_ctl
!
        allocate(bc_j_magnitude_ctl( num_bc_j_ctl))
        allocate(bc_j_name_ctl(num_bc_j_ctl))
        allocate(bc_j_type_ctl(num_bc_j_ctl))
        if(num_bc_j_ctl .gt. 0) bc_j_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_current_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_temp_ctl
!
        deallocate(bc_e_magnitude_ctl)
        deallocate(bc_e_type_ctl, bc_e_name_ctl)
!
       end subroutine deallocate_bc_temp_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_velo_ctl
!
        deallocate(bc_v_magnitude_ctl)
        deallocate(bc_v_name_ctl, bc_v_type_ctl)
!
       end subroutine deallocate_bc_velo_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_press_ctl
!
        deallocate(bc_p_magnitude_ctl)
        deallocate(bc_p_name_ctl, bc_p_type_ctl)
!
       end subroutine deallocate_bc_press_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_composit_ctl
!
        deallocate(bc_composit_magnitude_ctl)
        deallocate(bc_composit_name_ctl, bc_composit_type_ctl)
!
       end subroutine deallocate_bc_composit_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_magne_ctl
!
        deallocate(bc_b_magnitude_ctl)
        deallocate(bc_b_name_ctl, bc_b_type_ctl)
!
       end subroutine deallocate_bc_magne_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_magne_p_ctl
!
        deallocate(bc_mag_p_magnitude_ctl)
        deallocate(bc_mag_p_name_ctl, bc_mag_p_type_ctl)
!
       end subroutine deallocate_bc_magne_p_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_vect_p_ctl
!
        deallocate(bc_vp_magnitude_ctl)
        deallocate(bc_vp_name_ctl, bc_vp_type_ctl)
!
       end subroutine deallocate_bc_vect_p_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_current_ctl
!
        deallocate(bc_j_magnitude_ctl)
        deallocate(bc_j_name_ctl, bc_j_type_ctl)
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
!-----read b. c. for temp.-----------------------------
!
        call find_control_array_flag(hd_n_bc_temp, num_bc_e_ctl)
        if(num_bc_e_ctl.gt.0 .and. i_n_bc_temp.eq.0) then
          call allocate_bc_temp_ctl
          call read_control_array_c2_r_list(hd_n_bc_temp, num_bc_e_ctl, &
     &        i_n_bc_temp, bc_e_type_ctl, bc_e_name_ctl,                &
     &        bc_e_magnitude_ctl)
        end if
!
!-----read b.c. for velocity -------------
!
        call find_control_array_flag(hd_n_bc_velo, num_bc_v_ctl)
        if(num_bc_v_ctl.gt.0 .and. i_n_bc_velo.eq.0) then
          call allocate_bc_velo_ctl
          call read_control_array_c2_r_list(hd_n_bc_velo, num_bc_v_ctl, &
     &        i_n_bc_velo, bc_v_type_ctl, bc_v_name_ctl,                &
     &        bc_v_magnitude_ctl)
        end if
!
!-------read b.c. for pressure ---------------------------
!
        call find_control_array_flag(hd_n_bc_press, num_bc_p_ctl)
        if(num_bc_p_ctl.gt.0 .and. i_n_bc_press.eq.0) then
          call allocate_bc_press_ctl
          call read_control_array_c2_r_list(hd_n_bc_press,              &
     &        num_bc_p_ctl, i_n_bc_press, bc_p_type_ctl,                &
     &        bc_p_name_ctl, bc_p_magnitude_ctl)
        end if
!
!-------read b.c. for Dummy Scalar ---------------------------
!
        call find_control_array_flag(hd_n_bc_composit,                  &
     &      num_bc_composit_ctl)
        if(num_bc_composit_ctl.gt.0 .and. i_n_bc_composit.eq.0) then
          call allocate_bc_composit_ctl
          call read_control_array_c2_r_list(hd_n_bc_composit,           &
     &        num_bc_composit_ctl, i_n_bc_composit,                     &
     &        bc_composit_type_ctl, bc_composit_name_ctl,               &
     &        bc_composit_magnitude_ctl)
        end if
!
!-------read b.c. for magnetic field ---------------------------
!
        call find_control_array_flag(hd_n_bc_magne, num_bc_b_ctl)
        if(num_bc_b_ctl.gt.0 .and. i_n_bc_magne.eq.0) then
          call allocate_bc_magne_ctl
          call read_control_array_c2_r_list(hd_n_bc_magne,              &
     &        num_bc_b_ctl, i_n_bc_magne, bc_b_type_ctl,                &
     &        bc_b_name_ctl, bc_b_magnitude_ctl)
        end if
!
!-------read b.c. for magnetic potential ---------------------------
!
        call find_control_array_flag(hd_n_bc_mag_p, num_bc_mag_p_ctl)
        if(num_bc_mag_p_ctl.gt.0 .and. i_n_bc_mag_p.eq.0) then
          call allocate_bc_magne_p_ctl
          call read_control_array_c2_r_list(hd_n_bc_mag_p,              &
     &        num_bc_mag_p_ctl, i_n_bc_mag_p, bc_mag_p_type_ctl,        &
     &        bc_mag_p_name_ctl,  bc_mag_p_magnitude_ctl)
        end if
!
!-------read b.c. for vector potential ---------------------------
!
        call find_control_array_flag(hd_n_bc_vect_p, num_bc_vp_ctl)
        if(num_bc_vp_ctl.gt.0 .and. i_n_bc_vect_p.eq.0) then
          call allocate_bc_vect_p_ctl
          call read_control_array_c2_r_list(hd_n_bc_vect_p,             &
     &        num_bc_vp_ctl, i_n_bc_vect_p, bc_vp_type_ctl,             &
     &        bc_vp_name_ctl, bc_vp_magnitude_ctl)
        end if
!
!-------read b.c. for current density ---------------------------
!
        call find_control_array_flag(hd_n_bc_currect,  num_bc_j_ctl)
        if(num_bc_j_ctl.gt.0 .and. i_n_bc_currect.eq.0) then
          call allocate_bc_current_ctl
          call read_control_array_c2_r_list(hd_n_bc_currect,            &
     &        num_bc_j_ctl, i_n_bc_currect, bc_j_type_ctl,              &
     &            bc_j_name_ctl, bc_j_magnitude_ctl)
        end if
      end do
!
      end subroutine read_bc_4_node
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_node_boundary
