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
!!      subroutine set_rot_force_addresses                              &
!!     &         (i_phys, field_name, rot_forces, flag)
!!        type(base_force_address), intent(inout) :: rot_forces
!!
!!      integer(kind = kint) function num_rot_forces()
!!      subroutine set_rot_force_labels(n_comps, names, maths)
!!
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
      use t_base_force_labels
!
!>      Number of field labels
      integer(kind = kint), parameter, private :: nrot_force = 5
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
      subroutine set_rot_force_addresses                                &
     &         (i_phys, field_name, rot_forces, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: rot_forces
      logical, intent(inout) :: flag
!
!
      flag = check_rot_force(field_name)
      if(flag) then
        if (field_name .eq. rot_inertia%name) then
          rot_forces%i_m_advect =   i_phys
        else if (field_name .eq. rot_Coriolis_force%name) then
          rot_forces%i_Coriolis =   i_phys
        else if (field_name .eq. rot_Lorentz_force%name) then
          rot_forces%i_lorentz =    i_phys
!
        else if (field_name .eq. rot_buoyancy%name) then
          rot_forces%i_buoyancy =   i_phys
        else if (field_name .eq. rot_composite_buoyancy%name) then
          rot_forces%i_comp_buo =   i_phys
        end if
      end if
!
      end subroutine set_rot_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_rot_forces()
      num_rot_forces = nrot_force
      return
      end function num_rot_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_force_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nrot_force)
      character(len = kchara), intent(inout) :: names(nrot_force)
      character(len = kchara), intent(inout) :: maths(nrot_force)
!
!
      call set_field_labels(rot_inertia,                                &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(rot_Coriolis_force,                         &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(rot_Lorentz_force,                          &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(rot_buoyancy,                               &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(rot_composite_buoyancy,                     &
     &    n_comps( 5), names( 5), maths( 5))
!
      end subroutine set_rot_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_rot_force_labels
