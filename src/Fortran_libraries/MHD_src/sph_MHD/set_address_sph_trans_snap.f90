!>@file   set_address_sph_trans_snap.f90
!!@brief  module set_address_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_snapshot_trans                         &
!!     &         (d_rj, ipol, iphys, trns_snap,                         &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!      subroutine set_addresses_ene_flux_trans                         &
!!     &         (d_rj, ipol, iphys, trns_eflux,                        &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_eflux
!!      subroutine set_addresses_diff_vect_trans                        &
!!     &         (d_rj, ipol, iphys, trns_difv,                         &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_difv
!!
!!      subroutine copy_field_from_transform                            &
!!     &         (sph_params, sph_rtp, backward, mesh, nod_fld)
!!      subroutine copy_force_from_transform                            &
!!     &         (sph_params, sph_rtp, forward, mesh, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(spherical_transform_data), intent(in) :: forward
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_address_sph_trans_snap
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_mesh_data
      use t_spheric_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_snapshot_trans                           &
     &         (d_rj, ipol, iphys, trns_snap,                           &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_sph_trans_snap
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)  'Spherical transform field table ',                 &
     &              'for snapshot (trns_snap)'
      end if
!
      call bwd_trans_address_snap                                       &
     &   (d_rj, ipol, iphys, trns_snap%b_trns, trns_snap%backward)
!
      call fwd_trans_address_snap                                       &
     &   (d_rj, ipol, iphys, trns_snap%f_trns, trns_snap%forward)
!
      call count_num_fields_each_trans(trns_snap%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_snap%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_snap%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_snap%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_snap%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_snap%forward%num_scalar
      end if
!
      end subroutine set_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_ene_flux_trans                           &
     &         (d_rj, ipol, iphys, trns_eflux,                          &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_sph_trans_snap
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)  'Spherical transform field table ',                 &
     &              'for energy flux (trns_eflux)'
      end if
!
      call bwd_trans_address_ene_flux                                   &
     &   (d_rj, ipol, iphys, trns_eflux%b_trns, trns_eflux%backward)
      call fwd_trans_address_ene_flux                                   &
     &   (d_rj, ipol, iphys, trns_eflux%f_trns, trns_eflux%forward)
!
      call count_num_fields_each_trans(trns_eflux%backward,             &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_eflux%forward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_eflux%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_eflux%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_eflux%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_eflux%forward%num_scalar
      end if
!
      end subroutine set_addresses_ene_flux_trans
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_diff_vect_trans                          &
     &         (d_rj, ipol, iphys, trns_difv,                           &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_sph_trans_snap
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_difv
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for intermediate of snapshot (trns_difv)'
      end if
!
      call bwd_trans_address_diff_vect                                  &
     &   (d_rj, ipol, iphys, trns_difv%b_trns, trns_difv%backward)
      call fwd_trans_address_diff_vect                                  &
     &   (d_rj, ipol, iphys, trns_difv%f_trns, trns_difv%forward)
!
!
      call count_num_fields_each_trans(trns_difv%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_difv%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_difv%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_difv%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_difv%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_difv%forward%num_scalar
      end if
!
      end subroutine set_addresses_diff_vect_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_field_from_transform                              &
     &         (sph_params, sph_rtp, backward, mesh, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(in) :: backward
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, backward%num_vector
        call copy_vector_from_snap_trans                                &
     &    (backward%ifld_trns(i), backward%ifld_rtp(i),                 &
     &     sph_params%m_folding, sph_rtp, backward, mesh%node, nod_fld)
      end do
!
      do inum = 1, backward%num_scalar
        i = inum + backward%num_vector
        call copy_scalar_from_snap_trans                                &
     &    (backward%ifld_trns(i), backward%ifld_rtp(i),                 &
     &     sph_params%m_folding, sph_rtp, backward, mesh%node, nod_fld)
      end do
!
      end subroutine copy_field_from_transform
!
!-----------------------------------------------------------------------
!
      subroutine copy_force_from_transform                              &
     &         (sph_params, sph_rtp, forward, mesh, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(in) :: forward
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, forward%num_vector
        call copy_vector_from_snap_force                                &
     &     (forward%ifld_trns(i), forward%ifld_rtp(i),                  &
     &      sph_params%m_folding, sph_rtp, forward, mesh%node, nod_fld)
      end do
!
      do inum = 1, forward%num_scalar
        i = inum + forward%num_vector
        call copy_scalar_from_snap_force                                &
     &     (forward%ifld_trns(i), forward%ifld_rtp(i),                  &
     &      sph_params%m_folding, sph_rtp, forward, mesh%node, nod_fld)
      end do
!
      end subroutine copy_force_from_transform
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_snap
