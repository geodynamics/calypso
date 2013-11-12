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
      call set_radial_range_by_BC(iflag_icb_velocity,                   &
     &     kr_in_U, kr_out_U)
      call set_radial_range_by_BC(iflag_icb_temp, kr_in_T, kr_out_T)
      call set_radial_range_by_BC(iflag_icb_composition,                &
     &    kr_in_C, kr_out_C)
      call set_radial_range_by_BC(iflag_icb_magne, kr_in_B, kr_out_B)
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
      if(i_debug .gt. 1) call check_sph_bc_temp_sph
      if(i_debug .gt. 1) call check_sph_bc_composition_sph
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_range_by_BC(iflag_icb_bc, kr_in, kr_out)
!
      integer(kind = kint), intent(in) :: iflag_icb_bc
      integer(kind = kint), intent(inout) :: kr_in, kr_out
!
!
      if      (iflag_icb_bc .eq. iflag_sph_fill_center                  &
     &    .or. iflag_icb_bc .eq. iflag_sph_fix_center) then
        kr_in = nlayer_2_center
      else
        kr_in = nlayer_ICB
      end if
      kr_out =  nlayer_CMB
!
      end subroutine set_radial_range_by_BC
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
      subroutine check_sph_bc_temp_sph
!
      use m_spheric_parameter
      use m_bc_data_list
      use m_surf_data_list
!
      integer(kind = kint) :: i
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'h_flux_surf%num_bc',       h_flux_surf%num_bc
        write(*,*) 'h_flux_surf%ibc_type',     h_flux_surf%ibc_type
        write(*,*) 'h_flux_surf%bc_magnitude', h_flux_surf%bc_magnitude
      end if
!
      if(i_debug .gt. 1) then
        write(*,*) 'iflag_icb_temp', iflag_icb_temp
        if(iflag_icb_temp .eq. iflag_fixed_field) then
          do i = 1, nidx_rj(2)
            write(*,*) 'temp_ICB', idx_gl_1d_rj_j(i,1:3),               &
     &                  temp_ICB_bc(i)
          end do
        end if
        if(iflag_icb_temp .eq. iflag_fixed_flux) then
          do i = 1, nidx_rj(2)
            write(*,*) 'heat_flux_ICB', idx_gl_1d_rj_j(i,1:3),          &
     &                  h_flux_ICB_bc(i)
          end do
        end if
!
        write(*,*) 'iflag_cmb_temp', iflag_cmb_temp
        if(iflag_cmb_temp .eq. iflag_fixed_field) then
          do i = 1, nidx_rj(2)
            write(*,*) 'temp_CMB', idx_gl_1d_rj_j(i,1:3),               &
     &                  temp_CMB_bc(i)
          end do
        end if
        if(iflag_cmb_temp .eq. iflag_fixed_flux) then
          do i = 1, nidx_rj(2)
            write(*,*) 'heat_flux_CMB', idx_gl_1d_rj_j(i,1:3),          &
     &                  h_flux_CMB_bc(i)
          end do
        end if
!
      end if
!
      end subroutine check_sph_bc_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_bc_composition_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: i
!
!
      if(i_debug .gt. 1) then
        write(*,*) 'iflag_icb_composition', iflag_icb_composition
        if(iflag_icb_composition .eq. iflag_fixed_field) then
          do i = 1, nidx_rj(2)
            write(*,*) 'comp_ICB', idx_gl_1d_rj_j(i,1:3),               &
     &                  composition_ICB_bc(i)
          end do
        end if
        if(iflag_icb_composition .eq. iflag_fixed_flux) then
          do i = 1, nidx_rj(2)
            write(*,*) 'comp_flux_ICB', idx_gl_1d_rj_j(i,1:3),          &
     &                  c_flux_ICB_bc(i)
          end do
        end if
!
        write(*,*) 'iflag_cmb_composition', iflag_cmb_composition
        if(iflag_cmb_composition .eq. iflag_fixed_field) then
          do i = 1, nidx_rj(2)
            write(*,*) 'comp_CMB', idx_gl_1d_rj_j(i,1:3),               &
     &                  composition_CMB_bc(i)
          end do
        end if
        if(iflag_cmb_composition .eq. iflag_fixed_flux) then
          do i = 1, nidx_rj(2)
            write(*,*) 'comp_flux_CMB', idx_gl_1d_rj_j(i,1:3),          &
     &                  c_flux_CMB_bc(i)
          end do
        end if
!
      end if
!
      end subroutine check_sph_bc_composition_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_mhd
