!>@file   m_field_component_labels.f90
!!        module m_field_component_labels
!!
!!@author H. Matsui (UC Davis)
!!
!!@date   Programmed in Jan., 2020
!!@n      Modified in July, 2021
!!
!> @brief Labels and addresses of vector components
!!
!!@verbatim
!!      logical function check_field_comp_list(field_name)
!!
!!      subroutine set_field_component_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  product of field component names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field component names 
!!
!!   r_velocity                [i_velo_r]:
!!   theta_velocity            [i_velo_t]:
!!   phi_velocity              [i_velo_p]:
!!   cyl_r_velocity            [i_velo_s]:
!!   x_velocity                [i_velo_x]:
!!   y_velocity                [i_velo_y]:
!!   z_velocity                [i_velo_z]:
!!
!!   r_magnetic_f              [i_magne_r]:
!!   theta_magnetic_f          [i_magne_t]:
!!   phi_magnetic_f            [i_magne_p]:
!!   cyl_r_magnetic_f          [i_magne_s]:
!!   x_magnetic_f              [i_magne_x]:
!!   y_magnetic_f              [i_magne_y]:
!!   z_magnetic_f              [i_magne_z]:
!!
!!   temperature_from_CMB     [i_temp_from_CMB]
!!   composition_from_CMB     [i_light_from_CMB]
!!   entropy_from_CMB         [i_entropy_from_CMB]
!!   density_from_CMB         [i_density_from_CMB]
!!   aspherical_pressure      [i_asph_pressure]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_field_component_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!>        Field label for radial velocity @f$ u_{r} @f$
      type(field_def), parameter :: r_velocity                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'r_velocity',                              &
     &                math = '$ u_{r} $')
!>        Field label for theta component of velocity @f$ u_{\theta} @f$
      type(field_def), parameter :: theta_velocity                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'theta_velocity',                          &
     &                math = '$ u_{\theta} $')
!>        Field label for phi-component of  velocity @f$ u_{\phi} @f$
      type(field_def), parameter :: phi_velocity                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'phi_velocity',                            &
     &                math = '$ u_{\phi} $')
!>        Field label for cylindrical radial velocity @f$ u_{s} @f$
      type(field_def), parameter :: cyl_r_velocity                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'cyl_r_velocity',                          &
     &                math = '$ u_{s} $')
!>        Field label for z-componennt of velocity @f$ u_{x} @f$
      type(field_def), parameter :: x_velocity                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'x_velocity',                              &
     &                math = '$ u_{x} $')
!>        Field label for z-componennt of velocity @f$ u_{y} @f$
      type(field_def), parameter :: y_velocity                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'y_velocity',                              &
     &                math = '$ u_{y} $')
!>        Field label for z-componennt of velocity @f$ u_{z} @f$
      type(field_def), parameter :: z_velocity                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'z_velocity',                              &
     &                math = '$ u_{z} $')
!
!>        Field label for radial magnetic field @f$ B_{r} @f$
      type(field_def), parameter :: r_magnetic_f                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'r_magnetic_f',                            &
     &                math = '$ B_{r} $')
!>        Field label for theta component of magnetic field
!!         @f$ B_{\theta} @f$
      type(field_def), parameter :: theta_magnetic_f                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'theta_magnetic_f',                        &
     &                math = '$ B_{\theta} $')
!>        Field label for phi-component of  magnetic field 
!!         @f$ B_{\phi} @f$
      type(field_def), parameter :: phi_magnetic_f                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'phi_magnetic_f',                          &
     &                math = '$ B_{\phi} $')
!>        Field label for cylindrical radial magnetic field
!!          @f$ B_{s} @f$
      type(field_def), parameter :: cyl_r_magnetic_f                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'cyl_r_magnetic_f',                        &
     &                math = '$ B_{s} $')
!>        Field label for x-component of magnetic field
!!          @f$ B_{x} @f$
      type(field_def), parameter :: x_magnetic_f                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'x_magnetic_f',                            &
     &                math = '$ B_{x} $')
!>        Field label for y-component of magnetic field
!!          @f$ B_{y} @f$
      type(field_def), parameter :: y_magnetic_f                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'y_magnetic_f',                            &
     &                math = '$ B_{y} $')
!>        Field label for z-component of magnetic field
!!          @f$ B_{z} @f$
      type(field_def), parameter :: z_magnetic_f                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'z_magnetic_f',                            &
     &                math = '$ B_{z} $')
!
!
!>        Field label for temperature from CMB average
!!         @f$ T-|T(r_{o})| @f$
      type(field_def), parameter :: temperature_from_CMB                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temperature_from_CMB',                    &
     &                math = '$ T-|T(r_{o})| $')
!
!>        Field label for light element from CMB average
!!         @f$ C-|C(r_{o})| @f$
      type(field_def), parameter :: composition_from_CMB                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition_from_CMB',                    &
     &                math = '$ C-|C(r_{o})| $')
!
!>        Field label for entropy from CMB average
!!         @f$ S-|S(r_{o})| @f$
      type(field_def), parameter :: entropy_from_CMB                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'entropy_from_CMB',                        &
     &                math = '$ S-|S(r_{o})| $')
!
!>        Field label for density from CMB average
!!         @f$ \rho-|\rho(r_{o})| @f$
      type(field_def), parameter :: density_from_CMB                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'density_from_CMB',                        &
     &                math = '$ \rho-|\rho(r_{o})| $')
!
!>        Field label for aspherical component of pressure
!!         @f$ P-|\int P dS| @f$
      type(field_def), parameter :: aspherical_pressure                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'aspherical_pressure',                     &
     &                math = '$ P-\int P dS| $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_field_comp_list(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_field_comp_list = .FALSE.
      if (    (field_name .eq. r_velocity%name)                         &
     &   .or. (field_name .eq. theta_velocity%name)                     &
     &   .or. (field_name .eq. phi_velocity%name)                       &
     &   .or. (field_name .eq. cyl_r_velocity%name)                     &
     &   .or. (field_name .eq. x_velocity%name)                         &
     &   .or. (field_name .eq. y_velocity%name)                         &
     &   .or. (field_name .eq. z_velocity%name)                         &
!
     &   .or. (field_name .eq. r_magnetic_f%name)                       &
     &   .or. (field_name .eq. theta_magnetic_f%name)                   &
     &   .or. (field_name .eq. phi_magnetic_f%name)                     &
     &   .or. (field_name .eq. cyl_r_magnetic_f%name)                   &
     &   .or. (field_name .eq. x_magnetic_f%name)                       &
     &   .or. (field_name .eq. y_magnetic_f%name)                       &
     &   .or. (field_name .eq. z_magnetic_f%name)                       &
!
     &   .or. (field_name .eq. temperature_from_CMB%name)               &
     &   .or. (field_name .eq. composition_from_CMB%name)               &
     &   .or. (field_name .eq. entropy_from_CMB%name)                   &
     &   .or. (field_name .eq. density_from_CMB%name)                   &
     &   .or. (field_name .eq. aspherical_pressure%name)                &
     &      )   check_field_comp_list = .TRUE.
!
      end function check_field_comp_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_component_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(r_velocity,  array_c2i)
      call set_field_label_to_ctl(theta_velocity,  array_c2i)
      call set_field_label_to_ctl(phi_velocity,  array_c2i)
      call set_field_label_to_ctl(cyl_r_velocity,  array_c2i)
      call set_field_label_to_ctl(x_velocity,  array_c2i)
      call set_field_label_to_ctl(y_velocity,  array_c2i)
      call set_field_label_to_ctl(z_velocity,  array_c2i)
!
      call set_field_label_to_ctl(r_magnetic_f,  array_c2i)
      call set_field_label_to_ctl(theta_magnetic_f,  array_c2i)
      call set_field_label_to_ctl(phi_magnetic_f,  array_c2i)
      call set_field_label_to_ctl(cyl_r_magnetic_f,  array_c2i)
      call set_field_label_to_ctl(x_magnetic_f,  array_c2i)
      call set_field_label_to_ctl(y_magnetic_f,  array_c2i)
      call set_field_label_to_ctl(z_magnetic_f,  array_c2i)
!
      call set_field_label_to_ctl(temperature_from_CMB,     array_c2i)
      call set_field_label_to_ctl(composition_from_CMB,     array_c2i)
      call set_field_label_to_ctl(entropy_from_CMB,         array_c2i)
      call set_field_label_to_ctl(density_from_CMB,         array_c2i)
      call set_field_label_to_ctl(aspherical_pressure,      array_c2i)
!
      end subroutine set_field_component_names
!
! ----------------------------------------------------------------------
!
      end module m_field_component_labels
