!>@file   set_sph_bc_magne_sph.f90
!!@brief  module set_sph_bc_magne_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_sph_bc_magne_sph(bc_IO, sph_rj, radial_rj_grp, &
!!     &       CTR_nod_grp_name, CTR_sf_grp_name, magne_nod, magne_surf,&
!!     &       sph_bc_B, bcs_B)
!!        character(len=kchara), intent(in) :: CTR_nod_grp_name
!!        character(len=kchara), intent(in) :: CTR_sf_grp_name
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(boundary_condition_list), intent(in) :: magne_nod
!!        type(boundary_condition_list), intent(in) :: magne_surf
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_boundary_type), intent(inout) :: sph_bc_B
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_B
!!@endverbatim
!
      module set_sph_bc_magne_sph
!
      use m_precision
!
      use m_machine_parameter
      use m_boundary_condition_IDs
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
      use t_group_data
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_spheric_rj_data
      use t_bc_data_list
      use t_sph_boundary_input_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_sph_bc_magne_sph(bc_IO, sph_rj, radial_rj_grp,   &
     &        CTR_nod_grp_name, CTR_sf_grp_name, magne_nod, magne_surf, &
     &        sph_bc_B, bcs_B)
!
      use m_base_field_labels
      use set_bc_sph_scalars
      use set_sph_bc_data_by_file
!
      character(len=kchara), intent(in) :: CTR_nod_grp_name
      character(len=kchara), intent(in) :: CTR_sf_grp_name
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(boundary_condition_list), intent(in) :: magne_nod
      type(boundary_condition_list), intent(in) :: magne_surf
      type(boundary_spectra), intent(in) :: bc_IO
!
      type(sph_boundary_type), intent(inout) :: sph_bc_B
      type(sph_vector_boundary_data), intent(inout) :: bcs_B
!
      integer(kind = kint) :: i
      integer(kind = kint) :: igrp_icb, igrp_cmb
!
!
      call find_both_sides_of_boundaries(sph_rj, radial_rj_grp,         &
     &    magne_nod, magne_surf, sph_bc_B, igrp_icb, igrp_cmb)
!
      call alloc_sph_vector_bcs_data(sph_rj%nidx_rj(2), bcs_B)
!
      sph_bc_B%iflag_icb = iflag_sph_insulator
      sph_bc_B%iflag_cmb = iflag_sph_insulator
!
      i = abs(igrp_icb)
      if(igrp_icb .lt. 0) then
        if(sph_bc_B%icb_grp_name .eq. CTR_sf_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_sph_2_center) then
            sph_bc_B%iflag_icb =  iflag_sph_fill_center
          else if(magne_surf%ibc_type(i) .eq. iflag_sph_clip_center)    &
     &        then
            sph_bc_B%iflag_icb =  iflag_sph_fix_center
          end if
!
        else if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
          sph_bc_B%iflag_icb =  iflag_radial_magne
        end if
      else
        if(sph_bc_B%icb_grp_name .eq. CTR_nod_grp_name) then
          if(magne_nod%ibc_type(i) .eq. iflag_sph_2_center) then
            sph_bc_B%iflag_icb =  iflag_sph_fill_center
          else if(magne_nod%ibc_type(i) .eq. iflag_sph_clip_center)     &
     &        then
            sph_bc_B%iflag_icb =  iflag_sph_fix_center
          end if
!
        else if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            sph_bc_B%iflag_icb =  iflag_radial_magne
        else if(magne_nod%ibc_type(i) .eq. iflag_bc_file_s) then
          call set_fixed_vector_bc_by_file                              &
     &       (magnetic_field, sph_rj, bc_IO,                            &
     &        sph_bc_B%icb_grp_name, sph_bc_B%iflag_icb,                &
     &        bcs_B%ICB_Vspec)
        else if(magne_nod%ibc_type(i) .eq. iflag_bc_evo_field) then
          call set_evolved_vector_bc_by_file                            &
     &       (magnetic_field, sph_rj, bc_IO,                            &
     &        sph_bc_B%icb_grp_name, sph_bc_B%iflag_icb,                &
     &        bcs_B%ICB_Vevo)
        end if
      end if
!
!
      i = abs(igrp_cmb)
      if(igrp_icb .lt. 0) then
        if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
          sph_bc_B%iflag_cmb =  iflag_radial_magne
        end if
      else
        if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
          sph_bc_B%iflag_cmb =  iflag_radial_magne
        else if(magne_nod%ibc_type(i) .eq. iflag_bc_file_s) then
          call set_fixed_vector_bc_by_file                              &
     &       (magnetic_field, sph_rj, bc_IO,                            &
     &        sph_bc_B%cmb_grp_name, sph_bc_B%iflag_cmb,                &
     &        bcs_B%CMB_Vspec)
        else if(magne_nod%ibc_type(i) .eq. iflag_bc_evo_field) then
          call set_evolved_vector_bc_by_file                            &
     &       (magnetic_field, sph_rj, bc_IO,                            &
     &        sph_bc_B%cmb_grp_name, sph_bc_B%iflag_cmb,                &
     &        bcs_B%CMB_Vevo)
        end if
      end if
!
      end subroutine s_set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      end module set_sph_bc_magne_sph
