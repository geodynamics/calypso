!>@file  m_rot_force_labels.f90
!!       module m_rot_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!> @brief Labels and addresses for rotation of forces
!!
!!@verbatim
!!      logical function check_rot_force(field_name)
!!
!!      subroutine set_rot_force_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!! !!!!!  difference of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   rot_inertia              [rot_forces%i_m_advect]
!!   rot_Coriolis_force       [rot_forces%i_Coriolis]
!!   rot_Lorentz_force        [rot_forces%i_lorentz]
!!   rot_buoyancy             [rot_forces%i_buoyancy]
!!   rot_composite_buoyancy   [rot_forces%i_comp_buo]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_rot_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
!  rotation of momentum equations
!>        Field label for curl of advection
!!         @f$-e_{ijk} \partial_{j}
!!            \left(e_{klm} \omega_{l} u_{m} \right) @f$
      type(field_def), parameter :: rot_inertia                         &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_inertia',                             &
     &                math = '$ e_{ijk} \partial_{j}'                   &
     &               // ' \left(e_{kkm} \omega_{l} u_{m} \right)$')
!>        Field label for curl of Lorentz force
!!         @f$ -2 e_{ijk} \partial_{j} 
!!            \left(e_{klm} J_{l} B_{m} \right) @f$
      type(field_def), parameter :: rot_Coriolis_force                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_Coriolis_force',                      &
     &                math = '$ -2 e_{ijk} \partial_{j}'                &
     &               // '\left(e_{kkm} \Omega_{l} u_{m} \right) $')
!>        Field label for curl of Lorentz force
!!         @f$ e_{ijk} \partial_{j} \left(e_{klm} J_{l} B_{m}\right) @f$
      type(field_def), parameter :: rot_Lorentz_force                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_Lorentz_force',                       &
     &                math = '$ e_{ijk} \partial_{j}'                   &
     &               // '\left(e_{kkm} J_{l} B_{m} \right) $')
!
!>        Field label for curl of filtered buoyancy
!!        @f$ -e_{ijk} \partial_{j} \alpha_{T} T g_{k} @f$
      type(field_def), parameter :: rot_buoyancy                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_buoyancy',                            &
     &                math = '$-e_{ijk} \partial_{j} \alpha_{T}'        &
     &                    // ' T g_{k}$')
!>        Field label for curl of compositional buoyancy
!!        @f$ -e_{ijk} \partial_{j} \alpha_{C} C g_{k} @f$
      type(field_def), parameter :: rot_composite_buoyancy              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_composite_buoyancy',                  &
     &                math = '$-e_{ijk} \partial_{j} \alpha_{C}'        &
     &                    // ' C g_{k}$')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_rot_force(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_rot_force                                                   &
     &   =    (field_name .eq. rot_inertia%name)                        &
     &   .or. (field_name .eq. rot_Coriolis_force%name)                 &
     &   .or. (field_name .eq. rot_Lorentz_force%name)                  &
     &   .or. (field_name .eq. rot_buoyancy%name)                       &
     &   .or. (field_name .eq. rot_composite_buoyancy%name)
!
      end function check_rot_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_force_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(rot_inertia,            array_c2i)
      call set_field_label_to_ctl(rot_Coriolis_force,     array_c2i)
      call set_field_label_to_ctl(rot_Lorentz_force,      array_c2i)
      call set_field_label_to_ctl(rot_buoyancy,           array_c2i)
      call set_field_label_to_ctl(rot_composite_buoyancy, array_c2i)
!
      end subroutine set_rot_force_names
!
! ----------------------------------------------------------------------
!
      end module m_rot_force_labels
