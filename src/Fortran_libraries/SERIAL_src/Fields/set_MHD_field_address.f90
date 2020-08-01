!>@file   set_MHD_field_address.f90
!!        module set_MHD_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!!
!> @brief Set start address for field data
!!
!!@verbatim
!!      logical function check_vector_fields(phys_name_ctl)
!!      logical function check_scalar_fields(phys_name_ctl)
!!      logical function check_sym_tensor_fields(phys_name_ctl)
!!      logical function check_asym_tensor_fields(phys_name_ctl)
!!
!!      subroutine set_MHD_field_addresses                              &
!!     &         (i_fld, field_name, iphys, flag)
!!      subroutine set_old_MHD_field_addresses                          &
!!     &         (i_fld, field_name, iphys, flag)
!!        type(phys_address), intent(inout) :: iphys
!!@endverbatim
!!
!!@n @param num_field                 number of field
!!@n @param num_component(num_field)  number of components of field
!!@n @param field_name(num_field)     list of field names
!!@n @param iphys                     structure of field addresses
!
!
      module set_MHD_field_address
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      logical function check_vector_fields(phys_name_ctl)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_field_product_labels
      use t_grad_field_labels
      use t_diff_vector_labels
      use t_explicit_term_labels
!
      use m_base_field_labels
      use m_rot_force_labels
      use m_div_force_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical :: flag
!
      flag =  check_base_vector(phys_name_ctl)                          &
     &   .or. check_force_vectors(phys_name_ctl)                        &
     &   .or. check_rot_force(phys_name_ctl)                            &
     &   .or. check_div_flux_tensor(phys_name_ctl)                      &
     &   .or. check_gradient_field(phys_name_ctl)                       &
     &   .or. check_vector_diffusion(phys_name_ctl)                     &
     &   .or. check_difference_vectors(phys_name_ctl)                   &
     &   .or. check_field_product_vectors(phys_name_ctl)                &
     &   .or. check_vector_work_field(phys_name_ctl)                    &
     &   .or. check_vector_check_field(phys_name_ctl)
!
      check_vector_fields = flag
!
      end function check_vector_fields
!
! -----------------------------------------------------------------------
!
      logical function check_scalar_fields(phys_name_ctl)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use t_diffusion_term_labels
      use t_field_product_labels
      use t_grad_field_labels
      use t_explicit_term_labels
!
      use m_base_field_labels
      use m_div_force_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical :: flag
!
      flag =  check_base_scalar(phys_name_ctl)                          &
     &   .or. check_enegy_fluxes(phys_name_ctl)                         &
     &   .or. check_scalar_advection(phys_name_ctl)                     &
     &   .or. check_div_force(phys_name_ctl)                            &
     &   .or. check_div_scalar_flux(phys_name_ctl)                      &
     &   .or. check_divergence_field(phys_name_ctl)                     &
     &   .or. check_scalar_diffusion(phys_name_ctl)                     &
     &   .or. check_field_product_scalars(phys_name_ctl)                &
     &   .or. check_scalar_work_field(phys_name_ctl)                    &
     &   .or. check_scalar_check_field(phys_name_ctl)                   &
     &   .or. check_work_4_poisson(phys_name_ctl)
!
      check_scalar_fields = flag
!
      end function check_scalar_fields
!
! -----------------------------------------------------------------------
!
      logical function check_sym_tensor_fields(phys_name_ctl)
!
      use t_base_force_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
!
      check_sym_tensor_fields = check_flux_tensors(phys_name_ctl)
!
      end function check_sym_tensor_fields
!
! -----------------------------------------------------------------------
!
      logical function check_asym_tensor_fields(phys_name_ctl)
!
      use t_base_force_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
!
      check_asym_tensor_fields = check_asym_flux_tensors(phys_name_ctl)
!
      end function check_asym_tensor_fields
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_MHD_field_addresses                                &
     &         (i_fld, field_name, iphys, flag)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diff_vector_labels
      use t_field_product_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels

      use m_rot_force_labels
      use m_div_force_labels
!
      integer(kind = kint), intent(in) :: i_fld
      character(len = kchara), intent(in) :: field_name
!
      type(phys_address), intent(inout) :: iphys
      logical, intent(inout) :: flag
!
!
      call set_base_vector_addresses                                    &
     &   (i_fld, field_name, iphys%base, flag)
      if(flag) return
      call set_base_scalar_addresses                                    &
     &   (i_fld, field_name, iphys%base, flag)
      if(flag) return
!
      call set_base_force_addresses                                     &
     &   (i_fld, field_name, iphys%forces, flag)
      if(flag) return
      call set_enegy_fluxes_addresses                                   &
     &   (i_fld, field_name, iphys%ene_flux, flag)
      if(flag) return
      call set_div_force_addresses                                      &
     &   (i_fld, field_name, iphys%div_forces, flag)
      if(flag) return
      call set_base_diffusion_addresses                                 &
     &   (i_fld, field_name, iphys%diffusion, flag)
      if(flag) return
      call set_field_product_addresses                                  &
     &   (i_fld, field_name, iphys%prod_fld, flag)
      if(flag) return
!
      call set_gradient_field_addresses                                 &
     &   (i_fld, field_name, iphys%grad_fld, flag)
      if(flag) return
      call set_diff_vector_addresses                                    &
     &   (i_fld, field_name, iphys%diff_vector, flag)
      if(flag) return
!
      call set_rot_force_addresses                                      &
     &   (i_fld, field_name, iphys%rot_forces, flag)
      if(flag) return
!
      call set_work_field_addresses                                     &
     &   (i_fld, field_name, iphys%exp_work, flag)
      if(flag) return
      call set_check_field_addresses                                    &
     &   (i_fld, field_name, iphys%check_fld1, iphys%check_fld2, flag)
      if(flag) return
!
      end subroutine set_MHD_field_addresses
!
!  --------------------------------------------------------------------
!
      subroutine set_old_MHD_field_addresses                            &
     &         (i_fld, field_name, iphys, flag)
!
      integer(kind = kint), intent(in) :: i_fld
      character(len = kchara), intent(in) :: field_name
!
      type(phys_address), intent(inout) :: iphys
      logical, intent(inout) :: flag
!
!
      if(field_name .eq. buoyancy_work%name) then
        iphys%ene_flux%i_buo_gen =     i_fld
        flag = .TRUE.
      end if
      if(field_name .eq. geostrophic_balance%name) then
        iphys%prod_fld%i_geostrophic =  i_fld
        flag = .TRUE.
      end if
!
      end subroutine set_old_MHD_field_addresses
!
!  --------------------------------------------------------------------
!
      end module set_MHD_field_address
