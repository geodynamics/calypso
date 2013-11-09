!>@file   set_bc_sph_mhd.f90
!!@brief  module set_bc_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd
!!@endverbatim
!
      module set_bc_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_control_params_sph_MHD
      use m_boundary_condition_IDs
      use m_phys_labels
      use m_spheric_parameter
!
      implicit none
!
      private :: set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_bc_sph_mhd
!
      use set_bc_flag_sph_velo
      use set_bc_sph_scalars
!
!
      if (iflag_t_evo_4_velo .gt.     id_no_evolution) then
        call set_sph_bc_velo_sph
      end if
!
      if (iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call set_sph_bc_temp_sph
      end if
!
      if (iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call set_sph_bc_magne_sph
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_sph_bc_composition_sph
      end if
!
!
!
      if      (iflag_icb_velocity .eq. iflag_sph_fill_center            &
     &    .or. iflag_icb_velocity .eq. iflag_sph_fix_center) then
        kr_in_U = nlayer_2_center
      else
        kr_in_U = nlayer_ICB
      end if
      kr_out_U = nlayer_CMB
!
      if      (iflag_icb_temp .eq. iflag_sph_fill_center                &
     &    .or. iflag_icb_temp .eq. iflag_sph_fix_center) then
        kr_in_T = nlayer_2_center
      else
        kr_in_T = nlayer_ICB
      end if
      kr_out_T = nlayer_CMB
!
      if      (iflag_icb_composition .eq. iflag_sph_fill_center         &
     &    .or. iflag_icb_composition .eq. iflag_sph_fix_center) then
        kr_in_C = nlayer_2_center
      else
        kr_in_C = nlayer_ICB
      end if
      kr_out_C = nlayer_CMB
!
      if      (iflag_icb_magne .eq. iflag_sph_fill_center               &
     &    .or. iflag_icb_magne .eq. iflag_sph_fix_center) then
        kr_in_B = nlayer_2_center
      else
        kr_in_B = nlayer_ICB
      end if
      kr_out_B = nlayer_CMB
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_magne_sph
!
      use m_bc_data_list
      use m_surf_data_list
!
!
      integer(kind = kint) :: i
!
!
      iflag_icb_magne = iflag_sph_insulator
      iflag_cmb_magne = iflag_sph_insulator
!
      do i = 1, magne_nod%num_bc
        if(magne_nod%bc_name(i) .eq. ICB_nod_grp_name) then
          if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_icb_magne =  iflag_radial_magne
          end if
        end if
!
        if(magne_nod%bc_name(i) .eq. CMB_nod_grp_name) then
          if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_cmb_magne =  iflag_radial_magne
          end if
        end if
!
        if(magne_nod%bc_name(i) .eq. CTR_nod_grp_name) then
          if     (magne_nod%ibc_type(i) .eq. iflag_sph_2_center) then
            iflag_icb_magne =  iflag_sph_fill_center
          else if(magne_nod%ibc_type(i) .eq. iflag_sph_clip_center)     &
     &        then
            iflag_icb_magne =  iflag_sph_fix_center
          end if
        end if
      end do
!
!
      do i = 1, magne_surf%num_bc
        if(magne_surf%bc_name(i) .eq. ICB_nod_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_icb_magne =  iflag_radial_magne
          end if
        end if
!
        if(magne_surf%bc_name(i) .eq. CMB_nod_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_cmb_magne =  iflag_radial_magne
          end if
        end if
!
        if(magne_surf%bc_name(i) .eq. CTR_nod_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_sph_2_center) then
            iflag_icb_magne =  iflag_sph_fill_center
          else if(magne_surf%ibc_type(i) .eq. iflag_sph_clip_center)    &
     &        then
            iflag_icb_magne =  iflag_sph_fix_center
          end if
        end if
      end do
!
      end subroutine set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_mhd
