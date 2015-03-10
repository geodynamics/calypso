!m_ctl_data_mhd_forces.f90
!      module m_ctl_data_mhd_forces
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine deallocate_name_force_ctl
!
!      subroutine read_forces_ctl
!      subroutine read_gravity_ctl
!      subroutine read_coriolis_ctl
!      subroutine read_magneto_ctl
!
!    begin forces_define
!!!!!  define of forces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  available forces
!     gravity, Coriolis, Lorentz, Composite_gravity
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
!!      gravity_type_ctl     radial
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
!!      array rotation_vec   3
!!        rotation_vec  x   0.000    end
!!        rotation_vec  y   0.000    end
!!        rotation_vec  z   1.000    end
!!      end array rotation_vec
!!
!!      tri_sph_int_file     'rot_int.dat'
!!      sph_int_file_format     'ascii'
!!    end  Coriolis_define
!!
!!!!!!!!!!  magnetoconvection model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array ext_magne_vec:   0...off  more than 1...On
!!     ext_magne_vec: external field (constant)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin Magneto_convection_def
!!      magneto_cv_ctl    On
!!      array ext_magne_vec   3
!!        ext_magne_vec  x     0.000   end
!!        ext_magne_vec  y     1.000   end
!!        ext_magne_vec  z     0.000   end
!!      end array ext_magne_vec
!!    end  Magneto_convection_def
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      module m_ctl_data_mhd_forces
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use calypso_mpi
      use skip_comment_f
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for constant force list
!!@n      force_names_ctl%c_tbl: Name of force
      type(ctl_array_chara), save :: force_names_ctl
!
      character(len=kchara) :: gravity_ctl
!
!>      Structure for constant gravity vector
!!@n      gravity_vector_ctl%c_tbl:  Direction of gravity vector
!!@n      gravity_vector_ctl%vect:   Amplitude of gravity vector
      type(ctl_array_cr), save :: gravity_vector_ctl
!
      character (len=kchara) :: sph_cor_file_name_ctl
      character (len=kchara) :: sph_cor_file_fmt_ctl
!
!>      Structure for rotation of system
!!@n      system_rotation_ctl%c_tbl:  Direction of rotation vector
!!@n      system_rotation_ctl%vect:   Amplitude of rotation vector
      type(ctl_array_cr), save :: system_rotation_ctl
!
      character(len=kchara) :: magneto_cv_ctl
!
!>      Structure for external magnetic field control
!!@n      ext_magne_ctl%c_tbl:  Direction of external magnetic field
!!@n      ext_magne_ctl%vect:   Amplitude of external magnetic field
      type(ctl_array_cr), save :: ext_magne_ctl
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_forces_ctl =   'forces_define'
      integer (kind=kint) :: i_forces_ctl =    0
!
      character(len=kchara), parameter                                  &
     &      :: hd_gravity_ctl =  'gravity_define'
      integer (kind=kint) :: i_gravity_ctl =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
      integer (kind=kint) :: i_coriolis_ctl =  0
!
      character(len=kchara), parameter                                  &
     &      :: hd_magneto_ctl =  'Magneto_convection_def'
      integer (kind=kint) :: i_magneto_ctl =   0
!
!   4th level for forces
!
      character(len=kchara), parameter                                  &
     &        :: hd_num_forces =  'force_ctl'
!
!   4th level for time steps
!
      character(len=kchara), parameter                                  &
     &        :: hd_gravity_type = 'gravity_type_ctl'
      character(len=kchara), parameter                                  &
     &        :: hd_gravity_vect = 'gravity_vec'
      integer (kind=kint) :: i_gravity_type = 0
!
!   4th level for time steps
!
      character(len=kchara), parameter                                  &
     &        :: hd_rotation_vec =   'rotation_vec'
      character(len=kchara), parameter                                  &
     &        :: hd_sph_coriolis_file  = 'tri_sph_int_file'
      character(len=kchara), parameter                                  &
     &        :: hd_sph_coriolis_fmt = 'sph_int_file_format'
      integer (kind=kint) :: i_sph_coriolis_file = 0
      integer (kind=kint) :: i_sph_coriolis_fmt = 0
!
!   4th level for external magnetic field
!
      character(len=kchara), parameter                                  &
     &        :: hd_magneto_cv = 'magneto_cv_ctl'
      character(len=kchara), parameter                                  &
     &        :: hd_magne_vect = 'ext_magne_vec'
      integer (kind=kint) :: i_magneto_cv = 0
!
!
      private :: hd_forces_ctl, i_forces_ctl
      private :: hd_gravity_ctl, hd_coriolis_ctl, hd_magneto_ctl
      private :: i_gravity_ctl,  i_coriolis_ctl,  i_magneto_ctl
      private :: hd_num_forces, hd_sph_coriolis_file
      private :: hd_sph_coriolis_fmt
      private :: hd_gravity_type, hd_gravity_vect
      private :: hd_magneto_cv, hd_magne_vect
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_name_force_ctl
!
      call dealloc_control_array_chara(force_names_ctl)
!
      end subroutine deallocate_name_force_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_forces_ctl
!
!
      if(right_begin_flag(hd_forces_ctl) .eq. 0) return
      if (i_forces_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_forces_ctl, i_forces_ctl)
        if(i_forces_ctl .gt. 0) exit
!
        call read_control_array_c1(hd_num_forces, force_names_ctl)
      end do
!
      end subroutine read_forces_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_gravity_ctl
!
!
      if(right_begin_flag(hd_gravity_ctl) .eq. 0) return
      if (i_gravity_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_gravity_ctl, i_gravity_ctl)
        if(i_gravity_ctl .gt. 0) exit
!
        call read_control_array_c_r                                     &
     &     (hd_gravity_vect, gravity_vector_ctl)
!
        call read_character_ctl_item(hd_gravity_type,                   &
     &        i_gravity_type, gravity_ctl)
      end do
!
      end subroutine read_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_coriolis_ctl
!
!
      if(right_begin_flag(hd_coriolis_ctl) .eq. 0) return
      if (i_coriolis_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_coriolis_ctl, i_coriolis_ctl)
        if(i_coriolis_ctl .gt. 0) exit
!
!
        call read_control_array_c_r                                     &
     &     (hd_rotation_vec, system_rotation_ctl)
!
        call read_character_ctl_item(hd_sph_coriolis_file,              &
     &        i_sph_coriolis_file, sph_cor_file_name_ctl)
        call read_character_ctl_item(hd_sph_coriolis_fmt,               &
     &        i_sph_coriolis_fmt, sph_cor_file_fmt_ctl)
      end do
!
      end subroutine read_coriolis_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_magneto_ctl
!
!
      if(right_begin_flag(hd_magneto_ctl) .eq. 0) return
      if (i_magneto_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_magneto_ctl, i_magneto_ctl)
        if(i_magneto_ctl .gt. 0) exit
!
        call read_control_array_c_r(hd_magne_vect, ext_magne_ctl)
!
        call read_character_ctl_item(hd_magneto_cv,                     &
     &        i_magneto_cv, magneto_cv_ctl)
      end do
!
      end subroutine read_magneto_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_forces
