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
!!      integer(kind = kint) function num_field_comp_list()
!!      subroutine set_field_component_labels(n_comps, names, maths)
!!
!! !!!!!  product of field component names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field component names 
!!
!!   r_velocity                [i_velo_r]:
!!   theta_velocity            [i_velo_t]:
!!   phi_velocity              [i_velo_p]:
!!   cyl_r_velocity            [i_velo_s]:
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
      integer(kind = kint), parameter, private :: nfid_comps = 12
!
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
     &   .or. (field_name .eq. z_velocity%name)                         &
!
     &   .or. (field_name .eq. r_magnetic_f%name)                       &
     &   .or. (field_name .eq. theta_magnetic_f%name)                   &
     &   .or. (field_name .eq. phi_magnetic_f%name)                     &
     &   .or. (field_name .eq. cyl_r_magnetic_f%name)                   &
     &   .or. (field_name .eq. x_magnetic_f%name)                       &
     &   .or. (field_name .eq. y_magnetic_f%name)                       &
     &   .or. (field_name .eq. z_magnetic_f%name)                       &
     &      )   check_field_comp_list = .TRUE.
!
      end function check_field_comp_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_field_comp_list()
      num_field_comp_list = nfid_comps
      return
      end function num_field_comp_list
!
! ----------------------------------------------------------------------
!
      subroutine set_field_component_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nfid_comps)
      character(len = kchara), intent(inout) :: names(nfid_comps)
      character(len = kchara), intent(inout) :: maths(nfid_comps)
!
!
      call set_field_labels(r_velocity,                                 &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(theta_velocity,                             &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(phi_velocity,                               &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(cyl_r_velocity,                             &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(z_velocity,                                 &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(r_magnetic_f,                               &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(theta_magnetic_f,                           &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(phi_magnetic_f,                             &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(cyl_r_magnetic_f,                           &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(x_magnetic_f,                               &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(y_magnetic_f,                               &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(z_magnetic_f,                               &
     &    n_comps(12), names(12), maths(12))
!
      end subroutine set_field_component_labels
!
! ----------------------------------------------------------------------
!
      end module m_field_component_labels
