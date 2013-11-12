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
      use m_phys_labels
      use m_boundary_params_sph_MHD
      use set_bc_flag_sph_velo
      use set_bc_sph_scalars
      use set_reference_sph_mhd
      use cal_sph_bc_fdm_matrix
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
      call set_radial_range_by_BC(sph_bc_U)
      call set_radial_range_by_BC(sph_bc_T)
      call set_radial_range_by_BC(sph_bc_C)
      call set_radial_range_by_BC(sph_bc_B)
!
!      Set reference temperature and adjust boundary conditions
!
      call allocate_reft_rj_data
      call set_ref_temp_sph_mhd
      call adjust_sph_temp_bc_by_reftemp
!
!      Det FDM matrices for boundaries
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_sph_bc_fdm_matrices'
      call s_cal_sph_bc_fdm_matrices
!
!      Check data
!
      if(i_debug .gt. 1) then
        call check_sph_boundary_spectra(fhd_temp,                       &
     &      nidx_rj(2), idx_gl_1d_rj_j, sph_bc_T)
      end if
      if(i_debug .gt. 1) then
        call check_sph_boundary_spectra(fhd_light,                      &
     &      nidx_rj(2), idx_gl_1d_rj_j, sph_bc_C)
      end if
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_magne_sph
!
      use m_boundary_params_sph_MHD
      use m_bc_data_list
      use m_surf_data_list
!
!
      integer(kind = kint) :: i
!
!
      sph_bc_B%iflag_icb = iflag_sph_insulator
      sph_bc_B%iflag_cmb = iflag_sph_insulator
!
      do i = 1, magne_nod%num_bc
        if(magne_nod%bc_name(i) .eq. ICB_nod_grp_name) then
          if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            sph_bc_B%iflag_icb =  iflag_radial_magne
          end if
        end if
!
        if(magne_nod%bc_name(i) .eq. CMB_nod_grp_name) then
          if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            sph_bc_B%iflag_cmb =  iflag_radial_magne
          end if
        end if
!
        if(magne_nod%bc_name(i) .eq. CTR_nod_grp_name) then
          if     (magne_nod%ibc_type(i) .eq. iflag_sph_2_center) then
            sph_bc_B%iflag_icb =  iflag_sph_fill_center
          else if(magne_nod%ibc_type(i) .eq. iflag_sph_clip_center)     &
     &        then
            sph_bc_B%iflag_icb =  iflag_sph_fix_center
          end if
        end if
      end do
!
!
      do i = 1, magne_surf%num_bc
        if(magne_surf%bc_name(i) .eq. ICB_nod_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            sph_bc_B%iflag_icb =  iflag_radial_magne
          end if
        end if
!
        if(magne_surf%bc_name(i) .eq. CMB_nod_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            sph_bc_B%iflag_cmb =  iflag_radial_magne
          end if
        end if
!
        if(magne_surf%bc_name(i) .eq. CTR_nod_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_sph_2_center) then
            sph_bc_B%iflag_icb =  iflag_sph_fill_center
          else if(magne_surf%ibc_type(i) .eq. iflag_sph_clip_center)    &
     &        then
            sph_bc_B%iflag_icb =  iflag_sph_fix_center
          end if
        end if
      end do
!
      end subroutine set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_mhd
