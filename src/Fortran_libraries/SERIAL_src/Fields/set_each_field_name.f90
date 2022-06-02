!>@file   set_each_field_name.f90
!!@brief  module set_each_field_name
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!!@n     Modified by H.Matsui in Oct.,  2009
!!@n     Modified by H.Matsui in June., 2012
!!@n     Modified by H.Matsui in Jan., 2021
!!@n     Modified by T. Kera in Aug., 2021
!!
!>@brief  Set field name including SGS model from control data
!!
!!@verbatim
!!      subroutine set_vector_field_name                                &
!!     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!!      subroutine set_scalar_field_name                                &
!!     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!!      subroutine set_tensor_field_name                                &
!!     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!!      character(len = kchara), intent(in) :: phys_name_ctl
!!        logical, intent(in) :: flag_viz, flag_monitor
!!        type(phys_data), intent(inout) :: fld
!!        logical, intent(inout) :: flag
!!@endverbatim
!
      module set_each_field_name
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_field_name                                  &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use m_field_product_labels
      use set_MHD_field_address
      use set_MHD_sym_fld_address
      use append_phys_data
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_vector_fields(phys_name_ctl)                        &
     &   .or. check_sym_vector_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_vector,            &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      if(phys_name_ctl .eq. geostrophic_balance%name) then
        flag = .TRUE.
        call append_field_name_list(rest_of_geostrophic%name, n_vector, &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_vector_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name                                  &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use m_energy_flux_labels
      use set_MHD_field_address
      use set_MHD_sym_fld_address
      use append_phys_data
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_scalar_fields(phys_name_ctl)                        &
     &   .or. check_sym_scalar_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_scalar,            &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
!   Old field label... Should be deleted later!!
      if(phys_name_ctl .eq. buoyancy_work%name) then
        flag = .TRUE.
        call append_field_name_list(buoyancy_flux%name, n_scalar,       &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_scalar_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name                                  &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use set_MHD_field_address
      use set_MHD_sym_fld_address
      use append_phys_data
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_sym_tensor_fields(phys_name_ctl)                    &
     &   .or. check_sym_tensor_fields_w_sym(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_sym_tensor,        &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      flag =  check_asym_tensor_fields(phys_name_ctl)                   &
     &   .or. check_asym_tensor_fields_w_sym(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, ithree,              &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_tensor_field_name
!
! -----------------------------------------------------------------------
!
      end module set_each_field_name
