!>@file   address_sph_trans_snap.f90
!!@brief  module address_sph_trans_snap
!!
!!@author H. Matsui (UC Berkeley) and T. Kera (Tohoku University)
!!@date Programmed in Jan., 2010
!>        Modified by T. Kera in Aug., 2021
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine bwd_trans_address_snap                               &
!!     &         (d_rj, ipol, iphys, b_trns, trns_back)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(phys_address), intent(inout) :: b_trns
!!        type(spherical_transform_data), intent(inout) :: trns_back
!!      subroutine fwd_trans_address_snap                               &
!!     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!
!!      subroutine bwd_trans_address_ene_flux                           &
!!     &         (d_rj, ipol, iphys, b_trns, trns_back)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!      subroutine fwd_trans_address_ene_flux                           &
!!     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!
!!      subroutine bwd_trans_address_diff_vect                          &
!!     &         (d_rj, ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(phys_address), intent(inout) :: b_trns
!!        type(spherical_transform_data), intent(inout) :: trns_back
!!      subroutine fwd_trans_address_diff_vect                          &
!!     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(phys_address), intent(inout) :: f_trns
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!@endverbatim
!
      module address_sph_trans_snap
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_data
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_snap                                 &
     &         (d_rj, ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
      use add_prod_field_4_sph_trns
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(phys_address), intent(inout) :: b_trns
      type(spherical_transform_data), intent(inout) :: trns_back
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!      Vectors
      call add_base_vector_sph_trns_snap                                &
     &   (d_rj, ipol%base, iphys%base, b_trns%base, trns_back)
!
      call add_base_vector_sph_trns_snap                                &
     &   (d_rj, ipol%sym_fld, iphys%sym_fld, b_trns%sym_fld,            &
     &    trns_back)
      call add_base_vector_sph_trns_snap                                &
     &   (d_rj, ipol%asym_fld, iphys%asym_fld, b_trns%asym_fld,         &
     &    trns_back)
!
      call add_vector_comps_sph_trns_snap                               &
     &   (d_rj, ipol%prod_fld, iphys%prod_fld, b_trns%prod_fld,         &
     &    trns_back)
      trns_back%num_vector = trns_back%nfield
!
!      Scalars
      call add_base_scalar_sph_trns_snap                                &
     &   (d_rj, ipol%base, iphys%base, b_trns%base, trns_back)
!
      call add_base_scalar_sph_trns_snap                                &
     &   (d_rj, ipol%sym_fld, iphys%sym_fld, b_trns%sym_fld,            &
     &    trns_back)
      call add_base_scalar_sph_trns_snap                                &
     &   (d_rj, ipol%asym_fld, iphys%asym_fld, b_trns%asym_fld,         &
     &    trns_back)
!
      call add_scalar_comps_sph_trns_snap                               &
     &   (d_rj, ipol%fld_cmp, iphys%fld_cmp, b_trns%fld_cmp,            &
     &    trns_back)
!
      call add_flux_prods_sph_trns_snap                                 &
     &   (d_rj, ipol%prod_fld, iphys%prod_fld, b_trns%prod_fld,         &
     &    trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_snap
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_snap                                 &
     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!
      use add_base_force_4_sph_trns
      use add_prod_field_4_sph_trns
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
!   forces
      call add_base_force_sph_trns_snap                                 &
     &   (d_rj, ipol%forces, iphys%forces, f_trns%forces, trns_fwd)
!
      call add_base_force_4_MHD_sph_trns                                &
     &   (d_rj, ipol%forces_by_sym_sym, iphys%forces_by_sym_sym,        &
     &    f_trns%forces_by_sym_sym, trns_fwd)
      call add_base_force_4_MHD_sph_trns                                &
     &   (d_rj, ipol%forces_by_asym_asym, iphys%forces_by_asym_asym,    &
     &    f_trns%forces_by_asym_asym, trns_fwd)
      call add_base_force_4_MHD_sph_trns                                &
     &   (d_rj, ipol%forces_by_sym_asym, iphys%forces_by_sym_asym,      &
     &    f_trns%forces_by_sym_asym, trns_fwd)
      call add_base_force_4_MHD_sph_trns                                &
     &   (d_rj, ipol%forces_by_asym_sym, iphys%forces_by_asym_sym,      &
     &    f_trns%forces_by_asym_sym, trns_fwd)

      call add_base_force_sph_trns_snap                                 &
     &   (d_rj, ipol%forces_by_sym_sym, iphys%forces_by_sym_sym,        &
     &    f_trns%forces_by_sym_sym, trns_fwd)
      call add_base_force_sph_trns_snap                                 &
     &   (d_rj, ipol%forces_by_asym_asym, iphys%forces_by_asym_asym,    &
     &    f_trns%forces_by_asym_asym, trns_fwd)
      call add_base_force_sph_trns_snap                                 &
     &   (d_rj, ipol%forces_by_sym_asym, iphys%forces_by_sym_asym,      &
     &    f_trns%forces_by_sym_asym, trns_fwd)
      call add_base_force_sph_trns_snap                                 &
     &   (d_rj, ipol%forces_by_asym_sym, iphys%forces_by_asym_sym,      &
     &    f_trns%forces_by_asym_sym, trns_fwd)
!
      call add_vector_prods_sph_trns_snap                               &
     &   (d_rj, ipol%prod_fld, iphys%prod_fld, f_trns%prod_fld,         &
     &    trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
      call add_field_comps_sph_trns_snap                                &
     &   (d_rj, ipol%fld_cmp, iphys%fld_cmp, f_trns%fld_cmp,            &
     &    trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_ene_flux                             &
     &         (d_rj, ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
      use add_base_force_4_sph_trns
      use add_diffusion_4_sph_trns
      use add_prod_field_4_sph_trns
      use add_diff_vect_to_sph_trans
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      call add_vector_diffusion_sph_trns                                &
     &   (d_rj, ipol%diffusion, iphys%diffusion, b_trns%diffusion,      &
     &    trns_back)
!
      call add_rot_force_4_sph_trns_snap                                &
     &   (d_rj, ipol%rot_forces, iphys%rot_forces, b_trns%rot_forces,   &
     &    trns_back)
      call add_linear_force_bwd_trns_snap                               &
     &   (d_rj, ipol%forces, iphys%forces, b_trns%forces, trns_back)
      call add_mag_induct_bwd_trns_snap                                 &
     &   (d_rj, ipol%forces, iphys%forces, b_trns%forces, trns_back)
!
      call add_grad_4_sph_trns_snap                                     &
     &   (d_rj, ipol%grad_fld, iphys%grad_fld, b_trns%grad_fld,         &
     &    trns_back)
      call add_subtracted_sph_trns_snap                                 &
     &   (d_rj, ipol%prod_fld, iphys%prod_fld, b_trns%prod_fld,         &
     &    trns_back)
!
      call add_linear_force_bwd_trns_snap                               &
     &   (d_rj, ipol%forces_by_sym_asym, iphys%forces_by_sym_asym,      &
     &    b_trns%forces_by_sym_asym, trns_back)
      call add_linear_force_bwd_trns_snap                               &
     &   (d_rj, ipol%forces_by_sym_sym, iphys%forces_by_sym_sym,        &
     &    b_trns%forces_by_sym_sym, trns_back)
!
      call add_mag_induct_bwd_trns_snap                                 &
     &   (d_rj, ipol%forces_by_sym_asym, iphys%forces_by_sym_asym,      &
     &    b_trns%forces_by_sym_asym, trns_back)
      call add_mag_induct_bwd_trns_snap                                 &
     &   (d_rj, ipol%forces_by_asym_sym, iphys%forces_by_asym_sym,      &
     &    b_trns%forces_by_asym_sym, trns_back)
      call add_mag_induct_bwd_trns_snap                                 &
     &   (d_rj, ipol%forces_by_asym_asym, iphys%forces_by_asym_asym,    &
     &    b_trns%forces_by_asym_asym, trns_back)
      call add_mag_induct_bwd_trns_snap                                 &
     &   (d_rj, ipol%forces_by_sym_sym, iphys%forces_by_sym_sym,        &
     &    b_trns%forces_by_sym_sym, trns_back)
      trns_back%num_vector = trns_back%nfield
!
!
      call add_scalar_diffusion_sph_trns                                &
     &   (d_rj, ipol%diffusion, iphys%diffusion, b_trns%diffusion,      &
     &    trns_back)
!
      call add_scalar_flux_bwd_trns_snap                                &
     &   (d_rj, ipol%forces, iphys%forces, b_trns%forces, trns_back)
!
      call add_div_force_4_sph_trns_snap                                &
     &   (d_rj, ipol%div_forces, iphys%div_forces, b_trns%div_forces,   &
     &    trns_back)
!
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_ene_flux
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_ene_flux                             &
     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!
      use add_base_force_4_sph_trns
      use add_prod_field_4_sph_trns
      use add_energy_flux_4_sph_trns
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
!      Vector
      call add_base_force_fwd_trns_snap                                 &
     &   (d_rj, ipol%forces, iphys%forces, f_trns%forces, trns_fwd)
      call add_prod_vector_sph_trns_snap                                &
     &   (d_rj, ipol%prod_fld, iphys%prod_fld, f_trns%prod_fld,         &
     &    trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
!      Scalar
      call add_prod_scalar_sph_trns_snap                                &
     &   (d_rj, ipol%prod_fld, iphys%prod_fld, f_trns%prod_fld,         &
     &    trns_fwd)
      call add_ene_flux_4_sph_trns_snap                                 &
     &   (d_rj, ipol%ene_flux, iphys%ene_flux, f_trns%ene_flux,         &
     &    trns_fwd)
!
      call add_ene_flux_4_sph_trns_snap                                 &
     &    (d_rj, ipol%eflux_to_sym_by_sym_asym,                         &
     &    iphys%eflux_to_sym_by_sym_asym,                               &
     &    f_trns%eflux_to_sym_by_sym_asym, trns_fwd)
      call add_ene_flux_4_sph_trns_snap                                 &
     &   (d_rj, ipol%eflux_to_sym_by_asym_sym,                          &
     &    iphys%eflux_to_sym_by_asym_sym,                               &
     &    f_trns%eflux_to_sym_by_asym_sym, trns_fwd)
      call add_ene_flux_4_sph_trns_snap                                 &
     &   (d_rj, ipol%eflux_to_asym_by_sym_sym,                          &
     &    iphys%eflux_to_asym_by_sym_sym,                               &
     &    f_trns%eflux_to_asym_by_sym_sym, trns_fwd)
      call add_ene_flux_4_sph_trns_snap                                 &
     &   (d_rj, ipol%eflux_to_asym_by_asym_asym,                        &
     &   iphys%eflux_to_asym_by_asym_asym,                              &
     &   f_trns%eflux_to_asym_by_asym_asym, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_ene_flux
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_diff_vect                            &
     &         (d_rj, ipol, iphys, b_trns, trns_back)
!
      use add_diff_vect_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(phys_address), intent(inout) :: b_trns
      type(spherical_transform_data), intent(inout) :: trns_back
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      call add_diff_vect_sph_trns_by_pol                                &
     &   (d_rj, ipol%diff_vector, iphys%diff_vector,                    &
     &    b_trns%diff_vector, trns_back)
      trns_back%num_vector = trns_back%nfield
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_diff_vect
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_diff_vect                            &
     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!
      use add_diff_vect_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(phys_address), intent(inout) :: f_trns
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
      call add_diff_vect_scalar_trns_bpol(d_rj,                         &
     &    ipol%diff_vector, iphys%diff_vector, f_trns%diff_vector,      &
     &    trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
!
      end subroutine fwd_trans_address_diff_vect
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_snap
