!>@file   t_ctl_data_mhd_forces.f90
!!        module t_ctl_data_mhd_forces
!!
!!@author H. Matsui
!!@date   Programmed in March, 2006
!!
!!
!> @brief Control data for magnetic field controls
!!
!!@verbatim
!!      subroutine read_forces_ctl(id_control, hd_block, frc_ctl, c_buf)
!!        type(forces_control), intent(inout) :: frc_ctl
!!      subroutine read_gravity_ctl(id_control, hd_block, g_ctl, c_buf)
!!        type(forces_control), intent(inout) :: g_ctl
!!      subroutine read_coriolis_ctl                                    &
!!     &         (id_control, hd_block, cor_ctl, c_buf)
!!        type(coriolis_control), intent(inout) :: cor_ctl
!!
!!      subroutine bcast_forces_ctl(frc_ctl)
!!      subroutine bcast_gravity_ctl(g_ctl)
!!      subroutine bcast_coriolis_ctl(cor_ctl)
!!
!!      subroutine dealloc_name_force_ctl(frc_ctl)
!!        type(forces_control), intent(inout) :: frc_ctl
!!      subroutine dealloc_gravity_ctl(g_ctl)
!!      subroutine dealloc_coriolis_ctl(cor_ctl)
!!
!!    begin forces_define
!!!!!  define of forces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available forces
!!     gravity, Coriolis, Lorentz, Composite_gravity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array force_ctl      4
!!        force_ctl   gravity              end
!!        force_ctl   Coriolis             end
!!        force_ctl   Lorentz              end
!!        force_ctl   Composite_gravity    end
!!      end array
!!    end  forces_define
!!
!! !!!! gravity_type !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      0: constant
!!      1: constant_radial (constant intensity)
!!      2: radial (propotional to radius)
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin gravity_define
!!      FEM_gravity_model_ctl    element
!!      gravity_type_ctl          radial
!!
!! !!!! direction of gravity (opposite direction to that of buoyancy)
!!      array gravity_vec  3
!!        gravity_vec  x     0.000   end
!!        gravity_vec  y     0.000   end
!!        gravity_vec  z     -1.000   end
!!      end array gravity_vec
!!    end  gravity_define
!!
!! !!!! direction of rotation vector for Coriolis force !!!!!!!!!!!!!
!!
!!    begin Coriolis_define
!!      FEM_Coriolis_model_ctl       element
!!      FEM_Coriolis_implicit_ctl    On
!!
!!      array rotation_vec   3
!!        rotation_vec  x   0.000    end
!!        rotation_vec  y   0.000    end
!!        rotation_vec  z   1.000    end
!!      end array rotation_vec
!!
!!      tri_sph_int_file     'rot_int.dat'
!!      sph_int_file_format     'ascii'
!!    end  Coriolis_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_mhd_forces
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_charareal
      use calypso_mpi
      use skip_comment_f
      use bcast_control_arrays
!
      implicit  none
!
!
!>      Structure for force list
      type forces_control
!>        Structure for constant force list
!!@n        force_names%c_tbl: Name of force
        type(ctl_array_chara) :: force_names
!
        integer (kind=kint) :: i_forces_ctl =    0
      end type forces_control
!
!>      Structure for gravity definistion
      type gravity_control
!>        Coliolis force modeling in FEM
!!@n        element: Coriolis force in element
!!@n        node:    Coriolis force at node
        type(read_character_item) :: FEM_gravity_model
!>        Gravity type
        type(read_character_item) :: gravity
!
!>        Structure for constant gravity vector
!!@n        gravity_vector%c_tbl:  Direction of gravity vector
!!@n        gravity_vector%vect:   Amplitude of gravity vector
        type(ctl_array_cr) :: gravity_vector
!
        integer (kind=kint) :: i_gravity_ctl =   0
      end type gravity_control
!
!>      Structure for Coriolis force
      type coriolis_control
!>        Coliolis force modeling in FEM
!!@n        element: Coriolis force in element
!!@n        node:    Coriolis force at node
        type(read_character_item) :: FEM_coriolis_model
!>        Use implicit scheme for Coliolis force modeling in FEM
!!@n        On: Coriolis force is solved implicitly
        type(read_character_item) :: FEM_coriolis_implicit
!>        Structure for rotation of system
!!@n        system_rotation%c_tbl:  Direction of rotation vector
!!@n        system_rotation%vect:   Amplitude of rotation vector
        type(ctl_array_cr) :: system_rotation
!
        integer (kind=kint) :: i_coriolis_ctl =  0
      end type coriolis_control
!
!   4th level for forces
      character(len=kchara), parameter, private                         &
     &        :: hd_num_forces =  'force_ctl'
!
!   4th level for time steps
      character(len=kchara), parameter, private                         &
     &        :: hd_FEM_gravity_mode = 'FEM_gravity_model_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_gravity_type = 'gravity_type_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_gravity_vect = 'gravity_vec'
!
!   4th level for time steps
      character(len=kchara), parameter, private                         &
     &        :: hd_FEM_Coriolis_model = 'FEM_Coriolis_model_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_FEM_Coriolis_imp =   'FEM_Coriolis_implicit_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_rotation_vec =        'rotation_vec'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_forces_ctl(id_control, hd_block, frc_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(forces_control), intent(inout) :: frc_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(frc_ctl%i_forces_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control, hd_num_forces,           &
     &      frc_ctl%force_names, c_buf)
      end do
      frc_ctl%i_forces_ctl = 1
!
      end subroutine read_forces_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_gravity_ctl(id_control, hd_block, g_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(gravity_control), intent(inout) :: g_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(g_ctl%i_gravity_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_gravity_vect, g_ctl%gravity_vector, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_gravity_type, g_ctl%gravity)
        call read_chara_ctl_type(c_buf, hd_FEM_gravity_mode,            &
     &      g_ctl%FEM_gravity_model)
      end do
      g_ctl%i_gravity_ctl = 1
!
      end subroutine read_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_coriolis_ctl                                      &
     &         (id_control, hd_block, cor_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(coriolis_control), intent(inout) :: cor_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cor_ctl%i_coriolis_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_FEM_Coriolis_model,          &
     &      cor_ctl%FEM_coriolis_model)
        call read_chara_ctl_type(c_buf, hd_FEM_Coriolis_imp,            &
     &      cor_ctl%FEM_coriolis_implicit)
!
        call read_control_array_c_r(id_control, hd_rotation_vec,        &
     &      cor_ctl%system_rotation, c_buf)
      end do
      cor_ctl%i_coriolis_ctl = 1
!
      end subroutine read_coriolis_ctl
!
!   --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_forces_ctl(frc_ctl)
!
      use calypso_mpi_int
!
      type(forces_control), intent(inout) :: frc_ctl
!
!
      call bcast_ctl_array_c1(frc_ctl%force_names)
!
      call calypso_mpi_bcast_one_int(frc_ctl%i_forces_ctl, 0)
!
      end subroutine bcast_forces_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_gravity_ctl(g_ctl)
!
      use calypso_mpi_int
!
      type(gravity_control), intent(inout) :: g_ctl
!
!
      call bcast_ctl_array_cr(g_ctl%gravity_vector)
      call bcast_ctl_type_c1(g_ctl%gravity)
!
      call calypso_mpi_bcast_one_int(g_ctl%i_gravity_ctl, 0)
!
      end subroutine bcast_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_coriolis_ctl(cor_ctl)
!
      use calypso_mpi_int
!
      type(coriolis_control), intent(inout) :: cor_ctl
!
!
      call bcast_ctl_array_cr(cor_ctl%system_rotation)
!
      call calypso_mpi_bcast_one_int(cor_ctl%i_coriolis_ctl, 0)
!
      end subroutine bcast_coriolis_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_name_force_ctl(frc_ctl)
!
      type(forces_control), intent(inout) :: frc_ctl
!
!
      call dealloc_control_array_chara(frc_ctl%force_names)
      frc_ctl%i_forces_ctl = 0
!
      end subroutine dealloc_name_force_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gravity_ctl(g_ctl)
!
      type(gravity_control), intent(inout) :: g_ctl
!
!
      call dealloc_control_array_c_r(g_ctl%gravity_vector)
      g_ctl%gravity%iflag = 0
      g_ctl%i_gravity_ctl = 0
!
      end subroutine dealloc_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_coriolis_ctl(cor_ctl)
!
      type(coriolis_control), intent(inout) :: cor_ctl
!
!
      call dealloc_control_array_c_r(cor_ctl%system_rotation)
      cor_ctl%i_coriolis_ctl = 0
!
      end subroutine dealloc_coriolis_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_forces
