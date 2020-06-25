!>@file   m_force_control_labels.f90
!!        module m_force_control_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Force labels
!!
!!@verbatim
!!      integer(kind = kint) function num_advection_controls()
!!      integer(kind = kint) function num_force_controls()
!!      subroutine set_advection_control_labels(n_comps, names, maths)
!!      subroutine set_force_control_labels(n_comps, names, maths)
!!
!! !!!!! Force names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   Advection names  (Other possible expression)
!!
!!     inertia
!!     magnetic_induction   (induction)
!!     heat_advect          (heat)
!!     composition_advect   (comp_flux)
!!
!!   Force names
!!
!!     Coriolis_force       (Coriolis)
!!     Lorentz_force        (Lorentz)
!!
!!     buoyancy             (Thermal_buoyancy, Thermal_gravity, gravity)
!!     composite_buoyancy   (Compositional_buoyancy, composite_gravity
!!                           compositional_gravity)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_force_control_labels
!
      use m_precision
!
      use t_base_force_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nadvect_label = 4
      integer(kind = kint), parameter, private :: nforce_label =  4
!
!       Opthional names of force control labels
!
!>        Term label for induction term
      character(len=kchara), parameter :: induction_1 =  'induction'
!>        Term label for heat equation
      character(len=kchara), parameter :: heat_flux_1 =  'heat'
!>        Term label for cpmpositional flux term
      character(len=kchara), parameter :: comp_flux_1 =  'comp_flux'
!
!>       Coriolis force label
      character(len=kchara), parameter :: coriolis_e1 = 'Coriolis'
      character(len=kchara), parameter :: coriolis_n1 = 'Coriolis_node'
      character(len=kchara), parameter :: coriolis_i1 = 'Coriolis_imp'
!
!>       Lorentz force label
      character(len=kchara), parameter :: lorentz_label = 'Lorentz'
!
!>       Thermal buoyancy label
      character(len=kchara), parameter                                  &
     &             :: gravity_label = 'Thermal_buoyancy'
!
      character(len=kchara), parameter :: gravity_e1 =  'gravity'
      character(len=kchara), parameter :: gravity_e2 =  'buoyancy'
      character(len=kchara), parameter                                  &
     &             :: gravity_e5 = 'Thermal_gravity'
!
!
!>       Compositional buoyancy label
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_label = 'Compositional_buoyancy'
!
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e1 = 'Compositional_gravity'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e5 = 'Composite_gravity'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e6 = 'Composite_buoyancy'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_advection_controls()
      num_advection_controls = nadvect_label
      return
      end function num_advection_controls
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_force_controls()
      num_force_controls = nforce_label
      return
      end function num_force_controls
!
! ----------------------------------------------------------------------
!
      subroutine set_advection_control_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nadvect_label)
      character(len = kchara), intent(inout) :: names(nadvect_label)
      character(len = kchara), intent(inout) :: maths(nadvect_label)
!
!
      call set_field_labels(inertia,                                    &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(magnetic_induction,                         &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(heat_advect,                                &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(composition_advect,                         &
     &    n_comps( 4), names( 4), maths( 4))
!
      end subroutine set_advection_control_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_force_control_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nforce_label)
      character(len = kchara), intent(inout) :: names(nforce_label)
      character(len = kchara), intent(inout) :: maths(nforce_label)
!
!
      call set_field_labels(Coriolis_force,                             &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(Lorentz_force,                              &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(buoyancy,                                   &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(composite_buoyancy,                         &
     &    n_comps( 4), names( 4), maths( 4))
!
      end subroutine set_force_control_labels
!
! ----------------------------------------------------------------------
!
      end module m_force_control_labels
