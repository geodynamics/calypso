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
      use m_parallel_var_dof
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind=kint) :: num_force_ctl
      character (len=kchara), allocatable :: name_force_ctl(:)
!
      character(len=kchara) :: gravity_ctl
      integer(kind=kint) :: num_g_vect_ctl = 0
      real(kind=kreal) :: g_vect_ctl(3)
      character (len=kchara) :: g_dir_name_ctl(3)
!
      integer(kind=kint) :: num_angular_vect_ctl = 0
      real(kind=kreal) :: angular_vect_ctl(3) = (/zero,zero,one/)
      character (len=kchara) :: angular_dir_name_ctl(3)
      character (len=kchara) :: sph_cor_file_name_ctl
      character (len=kchara) :: sph_cor_file_fmt_ctl
!
      character(len=kchara) :: magneto_cv_ctl
      integer(kind=kint) :: num_magne_vect_ctl = 0
      real(kind=kreal) :: magne_vect_ctl(3)
      character (len=kchara) :: magne_dir_name_ctl(3)
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
      integer (kind=kint) :: i_num_forces =     0
!
!   4th level for time steps
!
      character(len=kchara), parameter                                  &
     &        :: hd_gravity_type = 'gravity_type_ctl'
      character(len=kchara), parameter                                  &
     &        :: hd_gravity_vect = 'gravity_vec'
      integer (kind=kint) :: i_gravity_type = 0
      integer (kind=kint) :: i_gravity_vect = 0
!
!   4th level for time steps
!
      character(len=kchara), parameter                                  &
     &        :: hd_rotation_vec =   'rotation_vec'
      character(len=kchara), parameter                                  &
     &        :: hd_sph_coriolis_file  = 'tri_sph_int_file'
      character(len=kchara), parameter                                  &
     &        :: hd_sph_coriolis_fmt = 'sph_int_file_format'
      integer (kind=kint) :: i_rotation_vec = 0
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
      integer (kind=kint) :: i_magne_vect = 0
!
!
      private :: hd_forces_ctl, i_forces_ctl
      private :: hd_gravity_ctl, hd_coriolis_ctl, hd_magneto_ctl
      private :: i_gravity_ctl,  i_coriolis_ctl,  i_magneto_ctl
      private :: hd_num_forces, hd_sph_coriolis_file
      private :: hd_sph_coriolis_fmt
      private :: hd_gravity_type, hd_gravity_vect
      private :: hd_magneto_cv, hd_magne_vect
      private :: allocate_name_force_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_name_force_ctl
!
      allocate(name_force_ctl(num_force_ctl))
!
      end subroutine allocate_name_force_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_name_force_ctl
!
      deallocate(name_force_ctl)
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
!
        call find_control_array_flag(hd_num_forces, num_force_ctl)
        if(num_force_ctl.gt.0 .and. i_num_forces.eq.0) then
          call allocate_name_force_ctl
          call read_control_array_chara_list(hd_num_forces,             &
     &        num_force_ctl, i_num_forces, name_force_ctl)
        end if
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
        call find_control_array_flag(hd_gravity_vect, num_g_vect_ctl)
        if(num_g_vect_ctl.gt.0 .and. i_gravity_vect.eq.0) then
          if(num_g_vect_ctl .gt. 3) call parallel_abort(10,             &
     &          'gravity vector should be 3 components')
          call read_control_array_vect_list(hd_gravity_vect,            &
     &        num_g_vect_ctl, i_gravity_vect,                           &
     &        g_dir_name_ctl, g_vect_ctl)
        end if
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
        call find_control_array_flag(hd_rotation_vec,                   &
     &      num_angular_vect_ctl)
        if(num_angular_vect_ctl.gt.0 .and. i_rotation_vec.eq.0) then
          if(num_angular_vect_ctl .gt. 3) call parallel_abort(10,       &
     &          'rotation vector should be 3 components')
          call read_control_array_vect_list(hd_rotation_vec,            &
     &        num_angular_vect_ctl, i_rotation_vec,                     &
     &        angular_dir_name_ctl, angular_vect_ctl)
        end if
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
        call find_control_array_flag(hd_magne_vect, num_magne_vect_ctl)
        if(num_magne_vect_ctl.gt.0 .and. i_magne_vect.eq.0) then
          if(num_magne_vect_ctl .ne. 3) call parallel_abort(10,         &
     &          'external magnetic field should be 3 components')
          call read_control_array_vect_list(hd_magne_vect,              &
     &        num_magne_vect_ctl, i_magne_vect,                         &
     &        magne_dir_name_ctl, magne_vect_ctl)
        end if
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
