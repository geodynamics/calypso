!>@file   t_diffusion_term_labels.f90
!!        module t_diffusion_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for diffusion terms and diffusivities
!!
!!@verbatim
!!      subroutine set_base_diffusion_addresses                         &
!!     &         (i_phys, field_name, diffusion, flag)
!!        type(diffusion_address), intent(inout) :: diffusion
!!      subroutine set_diffusivity_addresses                            &
!!     &         (i_phys, field_name, diffusivity, flag)
!!        type(diffusivity_adress), intent(inout) :: diffusivity
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
      module t_diffusion_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
      type diffusion_address
!>        start address for viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{i} @f$
        integer (kind=kint) :: i_v_diffuse =       izero
!>        start address for diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{i} @f$
        integer (kind=kint) :: i_w_diffuse =       izero
!>        start address for diffusion of vetor potential
!!         @f$ -J_{i} = \eta \partial_{j}\partial_{j} A_{i} @f$
        integer (kind=kint) :: i_vp_diffuse =      izero
!>        start address for magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{i} @f$
        integer (kind=kint) :: i_b_diffuse =       izero
!>        start address for thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T @f$
        integer (kind=kint) :: i_t_diffuse =       izero
!>        start address for compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C @f$
        integer (kind=kint) :: i_c_diffuse =       izero
!
!  divergence of momentum equations
!>        start address for divergence of viscousity
        integer (kind=kint) :: i_div_viscous =    izero
      end type diffusion_address
!
      type diffusivity_adress
!>        start address for viscosity   @f$ \mu @f$
        integer (kind=kint) :: i_viscosity =   izero
!>        start address for thermal diffusivity @f$ k_{T} @f$
        integer (kind=kint) :: i_T_conductivity =   izero
!>        start address for thermal diffusivity @f$ k_{C} @f$
        integer (kind=kint) :: i_C_conductivity =   izero
!
!>        start address for kinetic viscosity
        integer (kind=kint) :: i_K_viscosity =   izero
!>        start address for magnetic diffusivity
        integer (kind=kint) :: i_B_diffusivity = izero
!>        start address for thermal diffusivity
        integer (kind=kint) :: i_T_diffusivity = izero
!>        start address for chemical diffusivity
        integer (kind=kint) :: i_C_diffusivity = izero
      end type diffusivity_adress
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_base_diffusion_addresses                           &
     &         (i_phys, field_name, diffusion, flag)
!
      use m_diffusion_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(diffusion_address), intent(inout) :: diffusion
      logical, intent(inout) :: flag
!
!
      flag = check_vector_diffusion(field_name)                         &
     &      .or. check_scalar_diffusion(field_name)
      if(flag) then
        if (field_name .eq. viscous_diffusion%name) then
          diffusion%i_v_diffuse =   i_phys
        else if (field_name .eq. vorticity_diffusion%name) then
          diffusion%i_w_diffuse =   i_phys
        else if (field_name .eq. vector_potential_diffusion%name) then
          diffusion%i_vp_diffuse =  i_phys
        else if (field_name .eq. magnetic_diffusion%name) then
          diffusion%i_b_diffuse =   i_phys
        else if (field_name .eq. thermal_diffusion%name) then
          diffusion%i_t_diffuse =   i_phys
        else if (field_name .eq. composition_diffusion%name) then
          diffusion%i_c_diffuse =   i_phys
!
        else if (field_name .eq. div_viscousity%name) then
          diffusion%i_div_viscous = i_phys
        end if
      end if
!
      end subroutine set_base_diffusion_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_diffusivity_addresses                              &
     &         (i_phys, field_name, diffusivity, flag)
!
      use m_diffusion_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(diffusivity_adress), intent(inout) :: diffusivity
      logical, intent(inout) :: flag
!
!
      flag = check_diffusivity(field_name)
      if(flag) then
        if (field_name .eq. kinetic_viscosity%name) then
          diffusivity%i_K_viscosity =    i_phys
        else if (field_name .eq. magnetic_diffusivity%name) then
          diffusivity%i_B_diffusivity =  i_phys
        else if (field_name .eq. thermal_diffusivity%name) then
          diffusivity%i_T_diffusivity =  i_phys
        else if (field_name .eq. chemical_diffusivity%name) then
          diffusivity%i_C_diffusivity =  i_phys
!
        else if (field_name .eq. viscosity%name) then
          diffusivity%i_viscosity =      i_phys
        else if (field_name .eq. thermal_conductivity%name) then
          diffusivity%i_T_conductivity = i_phys
        else if (field_name .eq. chemical_conductivity%name) then
          diffusivity%i_C_conductivity = i_phys
        end if
      end if
!
      end subroutine set_diffusivity_addresses
!
! ----------------------------------------------------------------------
!
      end module t_diffusion_term_labels
