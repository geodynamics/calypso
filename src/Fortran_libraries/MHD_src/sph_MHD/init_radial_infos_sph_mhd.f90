!>@file   init_radial_infos_sph_mhd.f90
!!@brief  module init_radial_infos_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine init_r_infos_sph_mhd_evo(ipol, sph,                  &
!!     &          r_2nd, r_n2e_3rd, r_e2n_1st, omega_sph, MHD_prop)
!!      subroutine init_bc_infos_sph_mhd_evo                            &
!!     &         (bc_IO, sph_grps, MHD_BC, ipol, sph, r_2nd,            &
!!     &          MHD_prop, radial_variation, sph_MHD_bc)
!!      subroutine init_reference_fields(sph, ipol, r_2nd,              &
!!     &          refs, rj_fld, MHD_prop, sph_MHD_bc)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(inout) :: r_2nd
!!        type(fdm_matrices), intent(inout) :: r_n2e_3rd
!!        type(fdm_matrices), intent(inout) :: r_e2n_1st
!!        type(sph_rotation), intent(inout) :: omega_sph
!!        type(radial_reference_field), intent(inout) :: refs
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine set_delta_r_4_sph_mhd(sph_params, sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!@endverbatim
!!
!!@n @param r_hot        radius at highest temperature point
!!@n @param r_cold       radius at lowest temperature point
!!@n @param temp_hot     temperature at highest temperature point
!!@n @param temp_cold    temperature at lowest temperature point
!!@n @param rotate(3)    rotation vector
!
      module init_radial_infos_sph_mhd
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_spheric_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_poloidal_rotation
      use t_radial_reference_field
      use t_fdm_coefs
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_boundary_data_sph_MHD
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_physical_property
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_r_infos_sph_mhd_evo(ipol, sph,                    &
     &          r_2nd, r_n2e_3rd, r_e2n_1st, omega_sph, MHD_prop)
!
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
      use material_property
!
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
!
      type(fdm_matrices), intent(inout) :: r_2nd
      type(fdm_matrices), intent(inout) :: r_n2e_3rd
      type(fdm_matrices), intent(inout) :: r_e2n_1st
!
      type(sph_rotation), intent(inout) :: omega_sph
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
      integer(kind = kint), parameter :: id_check = 50
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_delta_r_4_sph_mhd'
      call set_delta_r_4_sph_mhd(sph%sph_params, sph%sph_rj)
!
!*  ----------  rotation of earth  ---------------
      if(iflag_debug .ge. iflag_routine_msg)                            &
     &                write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(sph%sph_rlm, sph%sph_rj,                 &
     &    MHD_prop%fl_prop, omega_sph)
!
!*  ---------- Coefficients of each term  -------
      if(iflag_debug.gt.0) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (sph%sph_params%radius_CMB, sph%sph_params%radius_ICB,         &
     &    ipol, MHD_prop)
!
!*  ---------- Finite differnce coefficients  ---------------
      if(iflag_debug .gt. 0) write(*,*) 'const_second_fdm_coefs'
      if(iflag_debug .ge. iflag_full_msg)                               &
    &                    open(id_check, file='FDM.dat')
      call const_second_fdm_coefs(id_check, sph%sph_params, sph%sph_rj, &
     &                            r_2nd)
!
      if (iflag_debug.gt.0) write(*,*) 'const_first_fdm_ele_to_node'
      call const_first_fdm_ele_to_node(id_check, sph%sph_rj, r_e2n_1st)
      if (iflag_debug.gt.0) write(*,*) 'const_third_fdm_node_to_ele'
      call const_third_fdm_node_to_ele(id_check, sph%sph_rj, r_n2e_3rd)
      if(iflag_debug .ge. iflag_full_msg) close(id_check)
!
      end subroutine init_r_infos_sph_mhd_evo
!
!  -------------------------------------------------------------------
!
      subroutine init_bc_infos_sph_mhd_evo                              &
     &         (bc_IO, sph_grps, MHD_BC, ipol, sph, r_2nd,              &
     &          MHD_prop, radial_variation, sph_MHD_bc)
!
      use set_bc_sph_mhd
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_data), intent(in) :: radial_variation
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
      integer(kind = kint), parameter :: id_check = 50
!
!*  ---------- boundary conditions  ---------------
      if(iflag_debug.gt.0) write(*,*) 'set_fdm_matrices_sph_mhd'
      call set_fdm_matrices_sph_mhd                                     &
     &   (bc_IO, sph%sph_params, sph%sph_rj, sph_grps%radial_rj_grp,    &
     &    MHD_prop, radial_variation, MHD_BC, sph_MHD_bc)
!
      if(iflag_debug .ge. iflag_full_msg) then
        open(id_check, file='FDM.dat', position='APPEND')
        call check_bc_sph_mhd                                           &
     &     (id_check, sph%sph_rj, MHD_prop, sph_MHD_bc)
        close(id_check)
      end if
!
      end subroutine init_bc_infos_sph_mhd_evo
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_delta_r_4_sph_mhd(sph_params, sph_rj)
!
      use set_radius_func_noequi
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_shell_parameters), intent(in) :: sph_params
!
!   Choose radial grid mode
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_dr_for_nonequi'
      call allocate_dr_rj_noequi(sph_rj%nidx_rj(1))
      call set_dr_for_nonequi(sph_params%nlayer_CMB,                    &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
!*
      end subroutine set_delta_r_4_sph_mhd
!
!  -------------------------------------------------------------------
!
      subroutine init_reference_fields(sph, ipol, r_2nd,                &
     &          refs, rj_fld, MHD_prop, sph_MHD_bc)
!
      use calypso_mpi
      use calypso_mpi_int
      use sph_mhd_rst_IO_control
      use reference_sources_from_d_rj
      use init_reference_scalar
      use init_external_magne_sph
      use radial_reference_field_IO
      use m_base_field_labels
!
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
!
      type(radial_reference_field), intent(inout) :: refs
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara), parameter                                  &
     &            :: tmat_name = 'reference_Temperature'
      character(len=kchara), parameter                                  &
     &            :: cmat_name = 'reference_Composition'
      logical :: flag_write_ref
      integer :: irank_local
!
      real(kind = kreal) :: range_ICB(3)
      integer(kind = kint) :: kr_reduce_inner
      integer(kind = kint) :: kr_reduce_outer
      integer(kind = kint) :: k_reduce_old2new_in(3)
      integer(kind = kint) :: k_reduce_old2new_out(3)
      real(kind = kreal) :: coef_reduce_old2new_in(3)
!
      integer(kind = kint) :: kr
      real(kind = kreal) :: grad, ratio
!
!
      flag_write_ref = .FALSE.
      if((refs%iref_diffusivity%i_K_viscosity                           &
     &    * refs%iref_grad_diffusivity%i_K_viscosity) .gt. 0) then
        call copy_const_diffusivity_to_ref                              &
     &    (sph%sph_rj%nidx_rj(1), MHD_prop%fl_prop%coef_diffuse,        &
     &     refs%ref_field%d_fld(1,refs%iref_diffusivity%i_K_viscosity), &
     &     refs%ref_field%d_fld(1,                                      &
     &                       refs%iref_grad_diffusivity%i_K_viscosity))
        flag_write_ref = .TRUE.
      end if
!
      if((refs%iref_diffusivity%i_B_diffusivity                         &
     &    * refs%iref_grad_diffusivity%i_B_diffusivity) .gt. 0) then
        call copy_const_diffusivity_to_ref                              &
     &    (sph%sph_rj%nidx_rj(1), MHD_prop%cd_prop%coef_diffuse,        &
     &   refs%ref_field%d_fld(1,refs%iref_diffusivity%i_B_diffusivity), &
     &   refs%ref_field%d_fld(1,                                        &
     &                     refs%iref_grad_diffusivity%i_B_diffusivity))
        flag_write_ref = .TRUE.
      end if
!
      if((refs%iref_diffusivity%i_T_diffusivity                         &
     &    * refs%iref_grad_diffusivity%i_T_diffusivity) .gt. 0) then
        call copy_const_diffusivity_to_ref                              &
     &    (sph%sph_rj%nidx_rj(1), MHD_prop%ht_prop%coef_diffuse,        &
     &   refs%ref_field%d_fld(1,refs%iref_diffusivity%i_T_diffusivity), &
     &   refs%ref_field%d_fld(1,                                        &
     &                     refs%iref_grad_diffusivity%i_T_diffusivity))
!
        call r_diffusivity_w_ICB_reduction                              &
     &     (sph%sph_params, MHD_prop%ht_prop, refs%iref_radius,         &
     &      refs%iref_diffusivity%i_T_diffusivity,                      &
     &      refs%iref_grad_diffusivity%i_T_diffusivity, refs%ref_field)
        flag_write_ref = .TRUE.
      end if
!
      if((refs%iref_diffusivity%i_C_diffusivity                         &
     &    * refs%iref_grad_diffusivity%i_C_diffusivity) .gt. 0) then
        call copy_const_diffusivity_to_ref                              &
     &    (sph%sph_rj%nidx_rj(1), MHD_prop%cp_prop%coef_diffuse,        &
     &   refs%ref_field%d_fld(1,refs%iref_diffusivity%i_C_diffusivity), &
     &   refs%ref_field%d_fld(1,                                        &
     &                     refs%iref_grad_diffusivity%i_C_diffusivity))
!
        call r_diffusivity_w_ICB_reduction                              &
     &     (sph%sph_params, MHD_prop%cp_prop, refs%iref_radius,         &
     &      refs%iref_diffusivity%i_C_diffusivity,                      &
     &      refs%iref_grad_diffusivity%i_C_diffusivity, refs%ref_field)
        flag_write_ref = .TRUE.
      end if
!
!
      call cal_ref_sources_from_d_rj(sph, ipol, rj_fld, refs)
      call load_sph_reference_fields(refs)
      call overwrite_sources_by_reference(sph%sph_rj, refs%iref_base,   &
     &    ipol%base, refs%ref_field, rj_fld)
!
      irank_local = 0
      if(sph%sph_rj%idx_rj_degree_zero .gt. 0) irank_local = my_rank
      call calypso_mpi_allreduce_one_int                                &
     &   (irank_local, refs%irank_reference, MPI_SUM)

      refs%ref_field%iflag_update(1:refs%ref_field%ntot_phys) = 0
      call s_init_reference_scalar(refs%irank_reference,                &
     &    MHD_prop%takepito_T, sph%sph_params, sph%sph_rj,              &
     &    r_2nd, MHD_prop%ht_prop,                                      &
     &   refs%ref_field%d_fld(1,refs%iref_diffusivity%i_T_diffusivity), &
     &   refs%ref_field%d_fld(1,                                        &
     &                     refs%iref_grad_diffusivity%i_T_diffusivity), &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center,                  &
     &    tmat_name, MHD_prop%ref_param_T,                              &
     &    refs%iref_radius, temperature%name,                           &
     &    refs%iref_base%i_temp, refs%iref_grad%i_grad_temp,            &
     &    refs%iref_base%i_heat_source, refs%r_itp,                     &
     &    refs%ref_field, sph_MHD_bc%bcs_T, flag_write_ref)
!
      call s_init_reference_scalar(refs%irank_reference,                &
     &    MHD_prop%takepito_C, sph%sph_params, sph%sph_rj,              &
     &    r_2nd, MHD_prop%cp_prop,                                      &
     &   refs%ref_field%d_fld(1,refs%iref_diffusivity%i_C_diffusivity), &
     &   refs%ref_field%d_fld(1,                                        &
     &                     refs%iref_grad_diffusivity%i_C_diffusivity), &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center,                  &
     &    cmat_name, MHD_prop%ref_param_C,                              &
     &    refs%iref_radius, composition%name,                           &
     &    refs%iref_base%i_light, refs%iref_grad%i_grad_composit,       &
     &    refs%iref_base%i_light_source, refs%r_itp,                    &
     &    refs%ref_field, sph_MHD_bc%bcs_C, flag_write_ref)
!
      call init_sph_contant_ext_magne(MHD_prop%cd_prop, sph%sph_rj,     &
     &    refs%iref_cmp, ipol%base, refs%ref_field, rj_fld,             &
     &    flag_write_ref)
!
      call calypso_mpi_barrier
!
      if(flag_write_ref .eqv. .FALSE.) return
      call set_default_reference_file_name(refs)
      call output_reference_field(refs)
!
      end subroutine init_reference_fields
!
!  -------------------------------------------------------------------
!
      subroutine r_diffusivity_w_ICB_reduction(sph_params, scl_prop,    &
     &          iref_radius, iref_diffusivity, iref_grad_diffuse,       &
     &          ref_field)
!
      use radial_interpolation
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(scalar_property), intent(inout) :: scl_prop
      integer(kind = kint), intent(in) :: iref_radius
      integer(kind = kint), intent(in) :: iref_diffusivity
      integer(kind = kint), intent(in) :: iref_grad_diffuse
      type(phys_data), intent(inout) :: ref_field
!
      real(kind = kreal) :: range_ICB(3)
      integer(kind = kint) :: kr_reduce_inner
      integer(kind = kint) :: kr_reduce_outer
      integer(kind = kint) :: k_reduce_old2new_in(3)
      integer(kind = kint) :: k_reduce_old2new_out(3)
      real(kind = kreal) :: coef_reduce_old2new_in(3)
      real(kind = kreal) :: ratio
!
      integer(kind = kint) :: kr
!
!
      if(scl_prop%diffuse_reduction_radius_ICB .le. zero)               &
     &  scl_prop%diffuse_reduction_radius_ICB = sph_params%radius_ICB
!
        range_ICB(1) = scl_prop%diffuse_reduction_radius_ICB            &
     &                - scl_prop%diffuse_reduction_width_ICB
        range_ICB(2) = scl_prop%diffuse_reduction_radius_ICB
        range_ICB(3) = scl_prop%diffuse_reduction_radius_ICB            &
     &                + scl_prop%diffuse_reduction_width_ICB
      write(*,*) 'ref_field%n_point', ref_field%n_point
      write(*,*) 'ref_field%d_fld', size(ref_field%d_fld)
      write(*,*) 'iref_radius', iref_radius
!
      call cal_radial_interpolation_coef                                &
     &   (ref_field%n_point, ref_field%d_fld(1,iref_radius),            &
     &    ithree, range_ICB, kr_reduce_inner, kr_reduce_outer,          &
     &    k_reduce_old2new_in, k_reduce_old2new_out,                    &
     &    coef_reduce_old2new_in)
!
!        write(*,*) 'range_ICB',  range_ICB(1:3)
!        write(*,*) 'kr_reduce_inner',  kr_reduce_inner
!        write(*,*) 'kr_reduce_outer',  kr_reduce_outer
!        write(*,*) 'k_reduce_old2new_in',  k_reduce_old2new_in(1:3)
!        write(*,*) 'k_reduce_old2new_out',  k_reduce_old2new_out(1:3)
!        write(*,*) 'coef_reduce_old2new_in',coef_reduce_old2new_in(1:3)
!
!        do kr = 1, k_reduce_old2new_in(1)
!          ref_field%d_fld(kr,iref_diffusivity)                         &
!     &        = ref_field%d_fld(kr,iref_diffusivity)
!          ref_field%d_fld(kr,iref_grad_diffuse) =  zero
!        end do
        do kr = k_reduce_old2new_in(1)+1, k_reduce_old2new_in(2)-1
          ratio = one - scl_prop%grad_diffusibity_ICB                   &
     &           * (ref_field%d_fld(kr,iref_radius) - range_ICB(1))
          ref_field%d_fld(kr,iref_diffusivity)                          &
     &           = ratio * ref_field%d_fld(kr,iref_diffusivity)
          ref_field%d_fld(kr,iref_grad_diffuse)                         &
     &           = - scl_prop%grad_diffusibity_ICB
        end do
!
        kr = k_reduce_old2new_in(2)
        ref_field%d_fld(kr,iref_diffusivity)                            &
     &     = scl_prop%diffuse_reduction_ratio_ICB                       &
     &      * ref_field%d_fld(kr,iref_diffusivity)
        ref_field%d_fld(kr,iref_grad_diffuse) =  zero
!
        do kr = k_reduce_old2new_in(2)+1, k_reduce_old2new_in(3)
          ratio = one - scl_prop%grad_diffusibity_ICB                   &
     &           * (range_ICB(3) - ref_field%d_fld(kr,iref_radius))
          ref_field%d_fld(kr,iref_diffusivity)                          &
     &           = ratio * ref_field%d_fld(kr,iref_diffusivity)
          ref_field%d_fld(kr,iref_grad_diffuse)                         &
     &           =  scl_prop%grad_diffusibity_ICB
        end do
!        do kr = k_reduce_old2new_in(3)+ 1, ref_field%n_point
!          ref_field%d_fld(kr,iref_diffusivity)                         &
!     &        = ref_field%d_fld(kr,iref_diffusivity)
!          ref_field%d_fld(kr,iref_grad_diffuse) = zero
!        end do
!
!      do kr = 1, ref_field%n_point
!        write(*,*) kr, ref_field%d_fld(kr,iref_radius),                &
!     &                 ref_field%d_fld(kr,iref_diffusivity),           &
!     &                 ref_field%d_fld(kr,iref_grad_diffuse)
!      end do
!
      end subroutine r_diffusivity_w_ICB_reduction
!
!  -------------------------------------------------------------------
!
      end module init_radial_infos_sph_mhd
