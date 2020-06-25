!>@file  m_div_force_labels.f90
!!       module m_div_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!> @brief Labels and addresses for divergence of forces by filtered field
!!
!!@verbatim
!!      logical function check_div_force(field_name)
!!      logical function check_div_flux_tensor(field_name)
!!      logical function check_div_scalar_flux(field_name)
!!
!!      subroutine set_div_force_addresses                              &
!!     &         (i_phys, field_name, div_force, flag)
!!        type(base_force_address), intent(inout) :: div_force
!!
!!      integer(kind = kint) function num_div_forces()
!!      subroutine set_div_force_labels(n_comps, names, maths)
!!
!! !!!!!  divergence of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   div_inertia                [div_forces%i_m_advect]
!!   div_Coriolis_force         [div_forces%i_Coriolis]
!!   div_Lorentz_force          [div_forces%i_lorentz]
!!   div_buoyancy               [div_forces%i_buoyancy]
!!   div_composite_buoyancy     [div_forces%i_comp_buo]
!!
!!   div_momentum_flux          [div_forces%i_m_flux]
!!   div_maxwell_tensor         [div_forces%i_maxwell]
!!   div_induction_tensor       [div_forces%i_induct_t]
!!
!!   div_heat_flux              [div_forces%i_h_flux]
!!   div_pert_heat_flux         [div_forces%i_ph_flux]
!!   div_composition_flux       [div_forces%i_c_flux]
!!   div_pert_composition_flux  [div_forces%i_pc_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_div_force_labels
!
      use m_precision
      use m_phys_constants
      use t_base_force_labels
!
!>      Number of field labels
      integer(kind = kint), parameter, private :: ndiv_force = 12
!
!  divergence of momentum equations
!>        Field label for divergence of advection
!!         @f$ - \partial_{i} 
!!           \left(e_{ijk} \omega_{j} u_{k} \right) @f$
      type(field_def), parameter :: div_inertia                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_inertia',                             &
     &                math = '$ \partial_{i}'                           &
     &                // '(e_{ijk} \omega_{j} u_{k})$')
!>        Field label for divergence of Lorentz force
!!         @f$ \partial_{i}
!!            \left(e_{ijk} J_{j} B_{k} \right) @f$
      type(field_def), parameter :: div_Coriolis_force                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_Coriolis_force',                      &
     &                math = '$ -2 \partial_{i}'                        &
     &                // '(e_{ijk} \Omega_{j} u_{k})$')
!>        Field label for divergence of Lorentz force
!!         @f$ \partial_{i}
!!            \left(e_{ijk} J_{j} B_{k} \right) @f$
      type(field_def), parameter :: div_Lorentz_force                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_Lorentz_force',                       &
     &                math = '$ \partial_{i}'                           &
     &                    // '(e_{ijk} J_{j} B_{k})$')
!
!>        Field label for divergence of filtered buoyancy
!!         @f$ -\partial_{i} \alpha_{T} g_{i} T @f$
      type(field_def), parameter :: div_buoyancy                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_buoyancy',                            &
     &                math = '$-\partial_{i} \alpha_{T} T g_{i}$')
!>        Field label for divergence of filtered compositional buoyancy
!!         @f$ -\partial_{i} \alpha_{C} g_{i} C @f$
      type(field_def), parameter :: div_composite_buoyancy              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_composite_buoyancy',                  &
     &                math = '$-\partial_{i} \alpha_{C} C g_{i}$')
!
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} (u_{i} u_{j}) @f$
      type(field_def), parameter :: div_momentum_flux                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_momentum_flux',                       &
     &                math = '$-\partial_{j} (u_{i} u_{j})$')
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} (B_{i} B_{j}) @f$
      type(field_def), parameter :: div_maxwell_tensor                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_maxwell_tensor',                      &
     &                math = '$ \partial_{j} (B_{i} B_{j})$')
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} (u_{i} B_{j} - B_{i} u]_{J}) @f$
      type(field_def), parameter :: div_induction_tensor                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_induction_tensor',                    &
     &                math = '$ \partial_{i} (u_{i} B_{j}'              &
     &                  //' - B_{i} u_{J})$')
!
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} (u_{i} T) @f$
      type(field_def), parameter :: div_heat_flux                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_heat_flux',                           &
     &                math = '$-\partial_{i} u_{i} T $')
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} (u_{i} \Theta) @f$
      type(field_def), parameter :: div_pert_heat_flux                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_pert_heat_flux',                      &
     &                math = '$-\partial_{i} u_{i} \Therta $')
!
!>        Field label for divergence of composition flux
!!         @f$ -\partial_{i} u_{i} C @f$
      type(field_def), parameter :: div_composition_flux                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_composition_flux',                    &
     &                math = '$-\partial_{i} u_{i} C $')
!>        Field label for divergence of perturbation of compopstion flux
!!         @f$ -\partial_{i} u_{i} \Theta_{C} @f$
      type(field_def), parameter :: div_pert_composition_flux           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_pert_composition_flux',               &
     &                math = '$-\partial_{i} u_{i} \Theta_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_div_force(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_force                                                   &
     &   =    (field_name .eq. div_inertia%name)                        &
     &   .or. (field_name .eq. div_Coriolis_force%name)                 &
     &   .or. (field_name .eq. div_Lorentz_force%name)                  &
     &   .or. (field_name .eq. div_buoyancy%name)                       &
     &   .or. (field_name .eq. div_composite_buoyancy%name)
!
      end function check_div_force
!
! ----------------------------------------------------------------------
!
      logical function check_div_flux_tensor(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_flux_tensor                                             &
     &   =    (field_name .eq. div_momentum_flux%name)                  &
     &   .or. (field_name .eq. div_maxwell_tensor%name)                 &
     &   .or. (field_name .eq. div_induction_tensor%name)
!
      end function check_div_flux_tensor
!
! ----------------------------------------------------------------------
!
      logical function check_div_scalar_flux(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_scalar_flux                                             &
     &   =    (field_name .eq. div_heat_flux%name)                      &
     &   .or. (field_name .eq. div_pert_heat_flux%name)                 &
     &   .or. (field_name .eq. div_composition_flux%name)               &
     &   .or. (field_name .eq. div_pert_composition_flux%name)
!
      end function check_div_scalar_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_force_addresses                                &
     &         (i_phys, field_name, div_forces, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: div_forces
      logical, intent(inout) :: flag
!
!
      flag = check_div_force(field_name)                                &
     &      .or. check_div_flux_tensor(field_name)                      &
     &      .or. check_div_scalar_flux(field_name)
      if(flag) then
        if (field_name .eq. div_inertia%name) then
          div_forces%i_m_advect =   i_phys
        else if (field_name .eq. div_Coriolis_force%name) then
          div_forces%i_Coriolis =   i_phys
        else if (field_name .eq. div_Lorentz_force%name) then
          div_forces%i_lorentz =    i_phys
!
        else if (field_name .eq. div_buoyancy%name) then
          div_forces%i_buoyancy =   i_phys
        else if (field_name .eq. div_composite_buoyancy%name) then
          div_forces%i_comp_buo =   i_phys
!
        else if (field_name .eq. div_heat_flux%name) then
          div_forces%i_h_flux =    i_phys
        else if (field_name .eq. div_pert_heat_flux%name) then
          div_forces%i_ph_flux =   i_phys
!
        else if (field_name .eq. div_composition_flux%name) then
          div_forces%i_c_flux =    i_phys
        else if (field_name .eq. div_pert_composition_flux%name) then
          div_forces%i_pc_flux =   i_phys
!
        else if (field_name .eq. div_momentum_flux%name) then
          div_forces%i_m_flux =   i_phys
        else if (field_name .eq. div_maxwell_tensor%name) then
          div_forces%i_maxwell =  i_phys
        else if (field_name .eq. div_induction_tensor%name) then
          div_forces%i_induct_t = i_phys
        end if
      end if
!
      end subroutine set_div_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_div_forces()
      num_div_forces = ndiv_force
      return
      end function num_div_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_div_force_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ndiv_force)
      character(len = kchara), intent(inout) :: names(ndiv_force)
      character(len = kchara), intent(inout) :: maths(ndiv_force)
!
!
      call set_field_labels(div_inertia,                                &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(div_Coriolis_force,                         &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(div_Lorentz_force,                          &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(div_buoyancy,                               &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(div_composite_buoyancy,                     &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(div_momentum_flux,                          &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(div_maxwell_tensor,                         &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(div_induction_tensor,                       &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(div_heat_flux,                              &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(div_pert_heat_flux,                         &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(div_composition_flux,                       &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(div_pert_composition_flux,                  &
     &    n_comps(12), names(12), maths(12))
!
      end subroutine set_div_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_div_force_labels
