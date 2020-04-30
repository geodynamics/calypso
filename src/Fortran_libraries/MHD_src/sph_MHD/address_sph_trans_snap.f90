!>@file   address_sph_trans_snap.f90
!!@brief  module address_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine bwd_trans_address_snap                               &
!!     &         (ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!      subroutine fwd_trans_address_snap                               &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!
!!      subroutine add_vector_4_bwd_trns_snap                           &
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine add_scalar_4_bwd_trns_snap                           &
!!     &         (ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!
!!      subroutine add_vector_4_fwd_trns_snap                           &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!      subroutine add_scalar_4_fwd_trns_snap                           &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_sph_trans_snap
!
      use m_precision
      use m_machine_parameter
!
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
     &         (ipol, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      call add_vector_4_bwd_trns_snap(ipol, iphys, b_trns, trns_back)
      trns_back%num_vector = trns_back%nfield
!
!
      call add_scalar_4_bwd_trns_snap(ipol, iphys, b_trns, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_snap
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_snap                                 &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      call add_vector_4_fwd_trns_snap(ipol, iphys, f_trns, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
      call add_scalar_4_fwd_trns_snap(ipol, iphys, f_trns, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_vector_4_bwd_trns_snap                             &
     &         (ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
      use add_base_force_4_sph_trns
      use add_diffusion_4_sph_trns
      use add_prod_field_4_sph_trns
      use add_diff_vect_to_sph_trans
      use add_field_to_sph_trans_list
!
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      call add_base_vector_sph_trns_snap                                &
     &   (ipol%base, iphys%base, b_trns%base, trns_back)
!
      call add_vector_diffusion_sph_trns                                &
     &   (ipol%diffusion, iphys%diffusion, b_trns%diffusion, trns_back)
!
      call add_rot_force_4_sph_trns_snap                                &
     &   (ipol%rot_forces, iphys%rot_forces, b_trns%rot_forces,         &
     &    trns_back)
!
      call add_base_force_bwd_trns_snap                                 &
     &   (ipol%forces, iphys%forces, b_trns%forces, trns_back)
!
!   Gradient of vector field
      call add_diff_vect_sph_trns_by_pol                                &
     &   (ipol%diff_vector, iphys%diff_vector, b_trns%diff_vector,      &
     &    trns_back)
      call add_grad_4_sph_trns_snap                                     &
     &   (ipol%grad_fld, iphys%grad_fld, b_trns%grad_fld, trns_back)
!
      call add_subtracted_sph_trns_snap                                 &
     &   (ipol%prod_fld, iphys%prod_fld, b_trns%prod_fld, trns_back)
!
      end subroutine add_vector_4_bwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_scalar_4_bwd_trns_snap                             &
     &         (ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
      use add_base_force_4_sph_trns
      use add_diffusion_4_sph_trns
!
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      call add_base_scalar_sph_trns_snap                                &
     &   (ipol%base, iphys%base, b_trns%base, trns_back)
!
      call add_scalar_diffusion_sph_trns                                &
     &   (ipol%diffusion, iphys%diffusion, b_trns%diffusion, trns_back)
!
      call add_scalar_flux_bwd_trns_snap                                &
     &   (ipol%forces, iphys%forces, b_trns%forces, trns_back)
!
      call add_div_force_4_sph_trns_snap                                &
     &   (ipol%div_forces, iphys%div_forces, b_trns%div_forces,         &
     &    trns_back)
!
      end subroutine add_scalar_4_bwd_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_vector_4_fwd_trns_snap                             &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use add_base_force_4_sph_trns
      use add_prod_field_4_sph_trns
!
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      call add_base_force_fwd_trns_snap                                 &
     &   (ipol%forces, iphys%forces, f_trns%forces, trns_fwd)
      call add_prod_vector_sph_trns_snap                                &
     &   (ipol%prod_fld, iphys%prod_fld, f_trns%prod_fld, trns_fwd)
!
      end subroutine add_vector_4_fwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_scalar_4_fwd_trns_snap                             &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use add_energy_flux_4_sph_trns
      use add_prod_field_4_sph_trns
!
      type(phys_address), intent(in) :: ipol, iphys
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      call add_prod_scalar_sph_trns_snap                                &
     &   (ipol%prod_fld, iphys%prod_fld, f_trns%prod_fld, trns_fwd)
      call add_ene_flux_4_sph_trns_snap                                 &
     &   (ipol%ene_flux, iphys%ene_flux, f_trns%ene_flux, trns_fwd)
!
      end subroutine add_scalar_4_fwd_trns_snap
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_snap
