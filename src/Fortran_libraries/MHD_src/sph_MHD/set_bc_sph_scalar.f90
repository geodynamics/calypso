!>@file   set_bc_sph_scalar.f90
!!@brief  module set_bc_sph_scalar
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for scalar fields
!!
!!@verbatim
!!      subroutine s_set_sph_bc_scalar(field, flux, bc_IO,              &
!!     &          sph_params, sph_rj, radial_rj_grp,                    &
!!     &          nod_bc_list, surf_bc_list, sph_bc, bcs_S)
!!        type(field_def), intent(in) :: field, flux
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(boundary_condition_list), intent(in) :: nod_bc_list
!!        type(boundary_condition_list), intent(in) :: surf_bc_list
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_boundary_type), intent(inout) :: sph_bc
!!        type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!!@endverbatim
!
      module set_bc_sph_scalar
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use m_boundary_condition_IDs
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_group_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_field_labels
!
      implicit none
!
      private :: inner_sph_bc_scalar_sph, outer_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_sph_bc_scalar(field, flux, bc_IO,                &
     &          sph_params, sph_rj, radial_rj_grp,                      &
     &          nod_bc_list, surf_bc_list, sph_bc, bcs_S)
!
      use set_sph_homogenious_BCs
!
      type(field_def), intent(in) :: field, flux
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(boundary_condition_list), intent(in) :: nod_bc_list
      type(boundary_condition_list), intent(in) :: surf_bc_list
      type(boundary_spectra), intent(in) :: bc_IO
!
      type(sph_boundary_type), intent(inout) :: sph_bc
      type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!
      integer(kind = kint) :: igrp_icb, igrp_cmb
!
!
      call alloc_sph_scalar_bcs_data(sph_rj%nidx_rj(2), bcs_S)
!
      call find_both_sides_of_boundaries(sph_rj, radial_rj_grp,         &
     &    nod_bc_list, surf_bc_list, sph_bc, igrp_icb, igrp_cmb)
!
!      Boundary setting for inner boundary
      call inner_sph_bc_scalar_sph                                      &
     &   (field, flux, nod_bc_list, surf_bc_list, bc_IO, igrp_icb,      &
     &    sph_params%l_truncation, sph_rj,                              &
     &    sph_bc, bcs_S%ICB_Sspec, bcs_S%ICB_Sevo)
!
!      Boundary setting for outer boundary
      call outer_sph_bc_scalar_sph                                      &
     &   (field, flux, nod_bc_list, surf_bc_list, bc_IO, igrp_cmb,      &
     &    sph_rj, sph_bc, bcs_S%CMB_Sspec, bcs_S%CMB_Sevo)
!
      end subroutine s_set_sph_bc_scalar
!
! -----------------------------------------------------------------------
!
      subroutine inner_sph_bc_scalar_sph                                &
     &         (field, flux, nod_bc_list, surf_bc_list, bc_IO,          &
     &          igrp_icb, l_truncation, sph_rj,                         &
     &          sph_bc, ICB_Sspec, ICB_Sevo)
!
      use set_sph_bc_data_by_file
      use set_filter_BC_to_center
      use set_sph_homogenious_BCs
!
      integer(kind = kint), intent(in) :: igrp_icb
      integer(kind = kint), intent(in) :: l_truncation
!
      type(field_def), intent(in) :: field, flux
      type(sph_rj_grid), intent(in) :: sph_rj
      type(boundary_condition_list), intent(in) :: nod_bc_list
      type(boundary_condition_list), intent(in) :: surf_bc_list
      type(boundary_spectra), intent(in) :: bc_IO
!
      type(sph_boundary_type), intent(inout) :: sph_bc
      type(sph_scalar_BC_coef), intent(inout) :: ICB_Sspec
      type(sph_scalar_BC_evo), intent(inout) :: ICB_Sevo
!
      integer(kind = kint) :: i
!
!      Boundary setting for inner boundary
      i = abs(igrp_icb)
      if(igrp_icb .lt. 0) then
        if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (surf_bc_list%bc_name(i), surf_bc_list%bc_magnitude(i),    &
     &        sph_rj, sph_bc%icb_grp_name, sph_bc%iflag_icb,            &
     &        ICB_Sspec%S_BC)
        else if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_grad_bc_by_file(flux, sph_rj, bc_IO,           &
     &        sph_bc%icb_grp_name, sph_bc%iflag_icb, ICB_Sspec)
        else if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_evo_flux) then
          call set_evolved_grad_bc_by_file(flux, sph_rj, bc_IO,         &
     &        sph_bc%icb_grp_name, sph_bc%iflag_icb, ICB_Sevo)
!
        else if(surf_bc_list%ibc_type(i) .eq. iflag_sph_2_center        &
     &    .and. surf_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_fill_center
        else if ( surf_bc_list%ibc_type(i) .eq. iflag_fix_center        &
     &    .and. surf_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_fix_center
          sph_bc%CTR_fld =   surf_bc_list%bc_magnitude(i)
        else if ( surf_bc_list%ibc_type(i) .eq. iflag_filter_center     &
     &    .and. surf_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_filter_center
          call sph_scalar_filter_to_center(l_truncation, sph_rj,        &
     &        surf_bc_list%bc_magnitude(i), ICB_Sspec)
        end if
!
      else if(igrp_icb .gt. 0) then
        if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (nod_bc_list%bc_name(i), nod_bc_list%bc_magnitude(i),      &
     &        sph_rj, sph_bc%icb_grp_name, sph_bc%iflag_icb,            &
     &        ICB_Sspec%S_BC)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_flux) then
          call set_fixed_grad_bc_by_file(flux, sph_rj, bc_IO,           &
     &        sph_bc%icb_grp_name, sph_bc%iflag_icb, ICB_Sspec)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_evo_flux) then
          call set_evolved_grad_bc_by_file(flux, sph_rj, bc_IO,         &
     &        sph_bc%icb_grp_name, sph_bc%iflag_icb, ICB_Sevo)
!
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc                                &
     &       (nod_bc_list%bc_name(i), nod_bc_list%bc_magnitude(i),      &
     &        sph_rj, sph_bc%icb_grp_name, ICB_Sspec%S_BC,              &
     &        sph_bc%iflag_icb)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_scalar_bc_by_file(field, sph_rj,               &
     &        bc_IO, sph_bc%icb_grp_name, sph_bc%iflag_icb, ICB_Sspec)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_evo_field) then
          call set_evolved_scalar_bc_by_file(field, sph_rj, bc_IO,      &
     &        sph_bc%icb_grp_name, sph_bc%iflag_icb, ICB_Sevo)
!
        else if(nod_bc_list%ibc_type(i) .eq. iflag_sph_2_center         &
     &    .and. nod_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_fill_center
        else if(nod_bc_list%ibc_type(i) .eq. iflag_fix_center           &
     &    .and. nod_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_fix_center
          sph_bc%CTR_fld =   nod_bc_list%bc_magnitude(i)
        else if(nod_bc_list%ibc_type(i) .eq. iflag_filter_center        &
     &    .and. nod_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_filter_center
          call sph_scalar_filter_to_center(l_truncation, sph_rj,        &
     &        nod_bc_list%bc_magnitude(i), ICB_Sspec)
        end if
      end if
!
      if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        ICB_Sspec%S_BC(1:sph_rj%nidx_rj(2))                             &
     &      = -ICB_Sspec%S_BC(1:sph_rj%nidx_rj(2))
      end if
!
      end subroutine inner_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine outer_sph_bc_scalar_sph                                &
     &         (field, flux, nod_bc_list, surf_bc_list, bc_IO,          &
     &          igrp_cmb, sph_rj, sph_bc, CMB_Sspec, CMB_Sevo)
!
      use set_sph_bc_data_by_file
      use set_sph_homogenious_BCs
!
      integer(kind = kint), intent(in) :: igrp_cmb
!
      type(field_def), intent(in) :: field, flux
      type(sph_rj_grid), intent(in) :: sph_rj
      type(boundary_condition_list), intent(in) :: nod_bc_list
      type(boundary_condition_list), intent(in) :: surf_bc_list
      type(boundary_spectra), intent(in) :: bc_IO
!
      type(sph_boundary_type), intent(inout) :: sph_bc
      type(sph_scalar_BC_coef), intent(inout) :: CMB_Sspec
      type(sph_scalar_BC_evo), intent(inout) :: CMB_Sevo
!
      integer(kind = kint) :: i
!
!      Boundary setting for outer boundary
      i = abs(igrp_cmb)
      if(igrp_cmb .lt. 0) then
        if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (surf_bc_list%bc_name(i), surf_bc_list%bc_magnitude(i),    &
     &        sph_rj, sph_bc%cmb_grp_name, sph_bc%iflag_cmb,            &
     &        CMB_Sspec%S_BC)
        else if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_grad_bc_by_file(flux, sph_rj, bc_IO,           &
     &        sph_bc%cmb_grp_name, sph_bc%iflag_cmb, CMB_Sspec)
        else if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_evo_flux) then
          call set_evolved_grad_bc_by_file(flux, sph_rj, bc_IO,         &
     &        sph_bc%cmb_grp_name, sph_bc%iflag_cmb, CMB_Sevo)
        end if
!
      else if(igrp_cmb .gt. 0) then
        if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (nod_bc_list%bc_name(i), nod_bc_list%bc_magnitude(i),      &
     &        sph_rj, sph_bc%cmb_grp_name, sph_bc%iflag_cmb,            &
     &        CMB_Sspec%S_BC)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_flux) then
          call set_fixed_grad_bc_by_file(flux, sph_rj, bc_IO,           &
     &        sph_bc%cmb_grp_name, sph_bc%iflag_cmb, CMB_Sspec)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_evo_flux) then
          call set_evolved_grad_bc_by_file(flux, sph_rj, bc_IO,         &
     &        sph_bc%cmb_grp_name, sph_bc%iflag_cmb, CMB_Sevo)
!
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc                                &
     &       (nod_bc_list%bc_name(i), nod_bc_list%bc_magnitude(i),      &
     &        sph_rj, sph_bc%cmb_grp_name, CMB_Sspec%S_BC,              &
     &        sph_bc%iflag_cmb)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_scalar_bc_by_file(field, sph_rj,               &
     &        bc_IO, sph_bc%cmb_grp_name, sph_bc%iflag_cmb, CMB_Sspec)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_evo_field) then
          call set_evolved_scalar_bc_by_file(field, sph_rj, bc_IO,      &
     &        sph_bc%cmb_grp_name, sph_bc%iflag_cmb, CMB_Sevo)
        end if
      end if
!
      end subroutine outer_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_scalar
