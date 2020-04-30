!>@file   t_phys_address.f90
!!        module t_phys_address
!!
!! @author H. Matsui
!! @date ...
!!
!!
!> @brief Structure of field addresses
!!       These integer points adresses of fields.
!!
!
      module t_phys_address
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use t_grad_field_labels
      use t_diffusion_term_labels
      use t_field_product_labels
      use t_explicit_term_labels
      use t_diff_vector_labels
!
      implicit  none
! 
!>       Structure for start address for fields
      type phys_address
!>        Structure of base fields
        type(base_field_address) :: base
!>        Structure of forces
        type(base_force_address) :: forces
!>        Structure of forces
        type(base_force_address) :: rot_forces
!>        Structure of forces
        type(base_force_address) :: div_forces
!>        Structure of energy fluxes
        type(energy_flux_address) :: ene_flux
!>        Structure of gradient of fields
        type(gradient_field_address) :: grad_fld
!>        Structure of difference of vector
        type(diff_vector_address) :: diff_vector
!
!>        Structure of diffusion terms
        type(diffusion_address) :: diffusion
!>        Structure of diffusivities
        type(diffusivity_adress) :: diffusivity
!
!>        Structure of products of fields
        type(phys_products_address) :: prod_fld
!
!>        Structure of work area
        type(explicit_term_address) :: exp_work
!>        First check work area
        type(explicit_term_address) :: check_fld1
!>        Second check work area
        type(explicit_term_address) :: check_fld2
      end type phys_address
!
!
      end module t_phys_address
