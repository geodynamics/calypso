!>@file   adjust_reference_fields.f90
!!@brief  module adjust_reference_fields
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2015
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine adjust_press_by_average_on_CMB                       &
!!     &         (kr_in, kr_out, sph_rj, ipol, rj_fld)
!!      subroutine sync_temp_by_per_temp_sph(ref_temp, ref_comp,        &
!!     &          MHD_prop, sph_rj, ipol, rj_fld)
!!        d_rj(inod,ipol%base%i_temp):        T => \Theta = T - T0
!!        d_rj(inod,ipol%base%i_per_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%grad_fld%i_grad_temp):      T => d \Theta / dr
!!        d_rj(inod,ipol%grad_fld%i_grad_per_t): d \Theta / dr
!!      subroutine trans_per_temp_to_temp_sph                           &
!!     &         (SPH_model, sph_rj, ipol, rj_fld)
!!        d_rj(inod,ipol%base%i_temp):        \Theta = T - T0 => T
!!        d_rj(inod,ipol%base%i_per_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%grad_fld%i_grad_temp): d \Theta / dr => dT / dr
!!        d_rj(inod,ipol%grad_fld%i_grad_per_t): d \Theta / dr
!!
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!
!!      subroutine delete_sphere_average(is_scalar, sph_rj, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module adjust_reference_fields
!
      use m_precision
      use m_machine_parameter
!
      use t_SPH_MHD_model_data
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_reference_scalar_param
!
      implicit  none
!
      private :: sync_scalar_by_pert_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine adjust_press_by_average_on_CMB                         &
     &         (kr_in, kr_out, sph_rj, ipol, rj_fld)
!
      use set_reference_sph_mhd
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call adjust_by_ave_pressure_on_CMB(kr_in, kr_out,                 &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol%base%i_press, &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine adjust_press_by_average_on_CMB
!
! -----------------------------------------------------------------------
!
      subroutine sync_temp_by_per_temp_sph                              &
     &         (SPH_model, sph_rj, ipol, rj_fld)
!
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ids_grad_temp, ids_grad_pert_t
      integer(kind = kint) :: ids_grad_comp, ids_grad_pert_c
!
!
      ids_grad_temp =   ipol%grad_fld%i_grad_temp +  1
      ids_grad_pert_t = ipol%grad_fld%i_grad_per_t + 1
      call sync_scalar_by_pert_sph                                      &
     &   (sph_rj, SPH_model%ref_temp, SPH_model%MHD_prop%ref_param_T,   &
     &    ipol%base%i_temp, ipol%grad_fld%i_grad_temp,                  &
     &    ids_grad_temp, ipol%base%i_per_temp,                          &
     &    ipol%grad_fld%i_grad_per_t, ids_grad_pert_t, rj_fld)
!
      ids_grad_comp =   ipol%grad_fld%i_grad_composit +  1
      ids_grad_pert_c = ipol%grad_fld%i_grad_per_c + 1
      call sync_scalar_by_pert_sph                                      &
     &   (sph_rj, SPH_model%ref_comp, SPH_model%MHD_prop%ref_param_C,   &
     &    ipol%base%i_light, ipol%grad_fld%i_grad_composit,             &
     &    ids_grad_comp, ipol%base%i_per_light,                         &
     &    ipol%grad_fld%i_grad_per_c, ids_grad_pert_c, rj_fld)
!
      end subroutine sync_temp_by_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine trans_per_temp_to_temp_sph                             &
     &         (SPH_model, sph_rj, ipol, rj_fld)
!
      use set_reference_sph_mhd
!
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ids_grad_temp, ids_grad_pert_t
      integer(kind = kint) :: ids_grad_comp, ids_grad_pert_c
!
!
      ids_grad_temp =   ipol%grad_fld%i_grad_temp +  1
      ids_grad_pert_t = ipol%grad_fld%i_grad_per_t + 1
      call trans_pert_to_scalar_sph                                     &
     &   (sph_rj, SPH_model%ref_temp, SPH_model%MHD_prop%ref_param_T,   &
     &    ipol%base%i_temp, ipol%grad_fld%i_grad_temp,                  &
     &    ids_grad_temp, ipol%base%i_per_temp,                          &
     &    ipol%grad_fld%i_grad_per_t, ids_grad_pert_t, rj_fld)
!
      ids_grad_comp =   ipol%grad_fld%i_grad_composit +  1
      ids_grad_pert_c = ipol%grad_fld%i_grad_per_c + 1
      call trans_pert_to_scalar_sph                                     &
     &   (sph_rj, SPH_model%ref_comp, SPH_model%MHD_prop%ref_param_C,   &
     &    ipol%base%i_light, ipol%grad_fld%i_grad_composit,             &
     &    ids_grad_comp, ipol%base%i_per_light,                         &
     &    ipol%grad_fld%i_grad_per_c, ids_grad_pert_c, rj_fld)
!
      end subroutine trans_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sync_scalar_by_pert_sph(sph_rj, reference, ref_param,  &
     &          is_temp, is_grad_t, ids_grad_t,                         &
     &          is_par_temp, is_grad_part_t, ids_grad_part_t, rj_fld)
!
      use set_reference_sph_mhd
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      integer(kind = kint), intent(in) :: is_temp, is_par_temp
      integer(kind = kint), intent(in) :: is_grad_t, is_grad_part_t
      integer(kind = kint), intent(in) :: ids_grad_t, ids_grad_part_t
!
      type(reference_scalar_param), intent(in) :: ref_param
      type(reference_field), intent(in) :: reference
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!
      if     (ref_param%iflag_reference .ne. id_sphere_ref_temp         &
     &  .and. ref_param%iflag_reference .ne. id_takepiro_temp           &
     &  .and. ref_param%iflag_reference .ne. id_numerical_solution      &
     &   ) return
!
      call chenge_temp_to_per_temp_sph                                  &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,             &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, reference%t_rj,        &
     &    is_temp, is_grad_t, ids_grad_t,                               &
     &    is_par_temp, is_grad_part_t, ids_grad_part_t,                 &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine sync_scalar_by_pert_sph
!
! -----------------------------------------------------------------------
!
      subroutine trans_pert_to_scalar_sph(sph_rj, reference, ref_param, &
     &          is_temp, is_grad_t, ids_grad_t,                         &
     &          is_par_temp, is_grad_part_t, ids_grad_part_t, rj_fld)
!
      use set_reference_sph_mhd
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(reference_scalar_param), intent(in) :: ref_param
      type(reference_field), intent(in) :: reference
!
      integer(kind = kint), intent(in) :: is_temp, is_par_temp
      integer(kind = kint), intent(in) :: is_grad_t, is_grad_part_t
      integer(kind = kint), intent(in) :: ids_grad_t, ids_grad_part_t
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if     (ref_param%iflag_reference .ne. id_sphere_ref_temp         &
     &  .and. ref_param%iflag_reference .ne. id_takepiro_temp           &
     &  .and. ref_param%iflag_reference .ne. id_numerical_solution      &
     &   ) return
!
      call transfer_per_temp_to_temp_sph                                &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,             &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, reference%t_rj,        &
     &    is_temp, is_grad_t, ids_grad_t,                               &
     &    is_par_temp, is_grad_part_t, ids_grad_part_t,                 &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine trans_pert_to_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine delete_sphere_average(is_scalar, sph_rj, rj_fld)
!
      use copy_nodal_fields
      use set_reference_sph_mhd
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: is_scalar
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call delete_zero_degree_comp                                      &
     &   (is_scalar, sph_rj%idx_rj_degree_zero, rj_fld%n_point,         &
     &    sph_rj%nidx_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      if(sph_rj%inod_rj_center .gt. 0) then
        rj_fld%d_fld(sph_rj%inod_rj_center,is_scalar) = 0.0d0
      end if
!
      end subroutine delete_sphere_average
!
!-----------------------------------------------------------------------
!
      end module adjust_reference_fields
