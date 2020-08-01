!>@file   m_diffusion_term_labels.f90
!!        module m_diffusion_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for diffusion terms and diffusivities
!!
!!@verbatim
!!      logical function check_vector_diffusion(field_name)
!!      logical function check_scalar_diffusion(field_name)
!!      logical function check_diffusivity(field_name)
!!
!!      integer(kind = kint) function num_base_diffusions()
!!      integer(kind = kint) function num_base_diffusivities()
!!      subroutine set_base_diffusion_labels(n_comps, names, maths)
!!      subroutine set_base_diffusivity_labels(n_comps, names, maths)
!!
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   viscous_diffusion             [diffusion%i_v_diffuse]:
!!   vorticity_diffusion           [diffusion%i_w_diffuse]:
!!   vector_potential_diffusion    [diffusion%i_vp_diffuse]:
!!   magnetic_diffusion            [diffusion%i_b_diffuse]:
!!   thermal_diffusion             [diffusion%i_t_diffuse]:
!!   composition_diffusion         [diffusion%i_c_diffuse]:
!!
!!   div_viscousity                [diffusion%i_div_viscous]:
!!
!!   viscosity                     [diffusivity%i_viscosity]:
!!   thermal_conductivity          [diffusivity%i_T_conductivity]:
!!   chemical_conductivity         [diffusivity%i_C_conductivity]:
!!
!!   kinetic_viscosity             [diffusivity%i_K_viscosity]:
!!   magnetic_diffusivity          [diffusivity%i_B_diffusivity]:
!!   thermal_diffusivity           [diffusivity%i_T_diffusivity]:
!!   chemical_diffusivity          [diffusivity%i_C_diffusivity]:
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_diffusion_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>      Numbder of diffusion terms
      integer(kind = kint), parameter, private :: ndiffusion = 7
!>      Numbder of diffusivities
      integer(kind = kint), parameter, private :: ndiffusivities = 7
!
!
!>        Field label for viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{i} @f$
      type(field_def), parameter :: viscous_diffusion                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'viscous_diffusion',                       &
     &                math = '$ \nu \partial_{j}\partial_{j} u_{i} $')
!>        Field label for diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{i} @f$
      type(field_def), parameter :: vorticity_diffusion                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vorticity_diffusion',                     &
     &                math = '$ \nu \partial_{j}\partial_{j}'           &
     &                    // ' \omega_{i} $')
!>        Field label for diffusion of vetor potential
!!         @f$ -J_{i} = \eta \partial_{j}\partial_{j} A_{i} @f$
      type(field_def), parameter :: vector_potential_diffusion          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vector_potential_diffusion',              &
     &                math = '$ -J_{i}'                                 &
     &                   // ' = \eta \partial_{j}\partial_{j} A_{i} $')
!>        Field label for magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{i} @f$
      type(field_def), parameter :: magnetic_diffusion                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_diffusion',                      &
     &                math = '$ \eta \partial_{j}\partial_{j} B_{i} $')
!>        Field label for thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T @f$
      type(field_def), parameter :: thermal_diffusion                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'thermal_diffusion',                       &
     &              math = '$ \kappa_{T} \partial_{j}\partial_{j} T $')
!>        Field label for compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C @f$
      type(field_def), parameter :: composition_diffusion               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition_diffusion',                   &
     &              math = '$ \kappa_{C} \partial_{j}\partial_{j} C $')
!
!
!>        Field label for divergence of viscousity
!!          @f$ \nu \partial_{i} \partial_{j}\partial_{j} u_{i} @f$
      type(field_def), parameter :: div_viscousity                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_viscousity',                          &
     &                math = '$ \nu \partial_{i}'                       &
     &                     // ' \partial_{j}\partial_{j} u_{i} $')
!
!   --------------------------------------------------------------------
!
!>        Field label for kinetic viscosity
!>                               @f$ \nu = \mu / \bar{\rho} @f$
      type(field_def), parameter :: kinetic_viscosity                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'kinetic_viscosity',                       &
     &                math = '$ \nu = \mu / \bar{\rho} $')
!
!>        Field label for magnetic diffusivity @f$ \eta @f$
      type(field_def), parameter :: magnetic_diffusivity                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_diffusivity',                    &
     &                math = '$ \eta $')
!>        Field label for thermal diffusivity 
!!                               @f$ \kappa_{T} = k / \bar{\rho} @f$

      type(field_def), parameter :: thermal_diffusivity                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'thermal_diffusivity',                     &
     &                math = '$ \kappa_{T} = k / \bar{\rho} $')
!>        Field label for chemical diffusivity  @f$ \kappa_{C} @f$
      type(field_def), parameter :: chemical_diffusivity                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'chemical_diffusivity',                    &
     &                math = '$ \kappa_{C} $')
!
!>        Field label for viscosity   @f$ \mu @f$
      type(field_def), parameter :: viscosity                           &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'viscosity',                               &
     &                math = '$ \mu $')
!>        Field label for thermal diffusivity @f$ k_{T} @f$
      type(field_def), parameter :: thermal_conductivity                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'thermal_conductivity',                    &
     &                math = '$ k_{T} $')
!>        Field label for chemical diffusivity @f$ k_{C} @f$
      type(field_def), parameter :: chemical_conductivity               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'chemical_conductivity',                   &
     &                math = '$ k_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_vector_diffusion(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_diffusion                                            &
     &   =    (field_name .eq. viscous_diffusion%name)                  &
     &   .or. (field_name .eq. vorticity_diffusion%name)                &
     &   .or. (field_name .eq. vector_potential_diffusion%name)         &
     &   .or. (field_name .eq. magnetic_diffusion%name)
!
      end function check_vector_diffusion
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_diffusion(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_diffusion                                            &
     &   =    (field_name .eq. thermal_diffusion%name)                  &
     &   .or. (field_name .eq. composition_diffusion%name)              &
     &   .or. (field_name .eq. div_viscousity%name)
!
      end function check_scalar_diffusion
!
! ----------------------------------------------------------------------
!
      logical function check_diffusivity(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_diffusivity                                                 &
     &   =    (field_name .eq. kinetic_viscosity%name)                  &
     &   .or. (field_name .eq. magnetic_diffusivity%name)               &
     &   .or. (field_name .eq. thermal_diffusivity%name)                &
     &   .or. (field_name .eq. chemical_diffusivity%name)               &
     &   .or. (field_name .eq. viscosity%name)                          &
     &   .or. (field_name .eq. thermal_conductivity%name)               &
     &   .or. (field_name .eq. chemical_conductivity%name)
!
      end function check_diffusivity
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_base_diffusions()
      num_base_diffusions = ndiffusion
      return
      end function num_base_diffusions
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_base_diffusivities()
      num_base_diffusivities = ndiffusivities
      return
      end function num_base_diffusivities
!
! ----------------------------------------------------------------------
!
      subroutine set_base_diffusion_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ndiffusion)
      character(len = kchara), intent(inout) :: names(ndiffusion)
      character(len = kchara), intent(inout) :: maths(ndiffusion)
!
!
      call set_field_labels(viscous_diffusion,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(vorticity_diffusion,                        &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(vector_potential_diffusion,                 &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(magnetic_diffusion,                         &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(thermal_diffusion,                          &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(composition_diffusion,                      &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(div_viscousity,                             &
     &    n_comps( 7), names( 7), maths( 7))
!
      end subroutine set_base_diffusion_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_base_diffusivity_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ndiffusivities)
      character(len = kchara), intent(inout) :: names(ndiffusivities)
      character(len = kchara), intent(inout) :: maths(ndiffusivities)
!
!
      call set_field_labels(kinetic_viscosity,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(magnetic_diffusivity,                       &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(thermal_diffusivity,                        &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(chemical_diffusivity,                       &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(viscosity,                                  &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(thermal_conductivity,                       &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(chemical_conductivity,                      &
     &    n_comps( 7), names( 7), maths( 7))
!
      end subroutine set_base_diffusivity_labels
!
! ----------------------------------------------------------------------
!
      end module m_diffusion_term_labels
