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
!!     &         (SPH_MHD, iphys, trns_snap,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!      subroutine set_addresses_temporal_trans                         &
!!     &         (SPH_MHD, iphys, trns_tmp,                             &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!
!!      subroutine copy_field_from_transform                            &
!!     &         (sph_params, sph_rtp, backward, mesh, nod_fld)
!!      subroutine copy_force_from_transform                            &
!!     &         (sph_params, sph_rtp, forward, mesh, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_each_sph_trans), intent(in) :: forward
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_address_sph_trans_snap
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_address
      use t_SPH_mesh_field_data
      use t_addresses_sph_transform
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
     &         (SPH_MHD, iphys, trns_snap,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_snap
      use address_fwd_sph_trans_snap
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)                                                      &
     &       'Spherical transform field table for snapshot (trns_snap)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      call b_trans_address_vector_snap(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_snap%b_trns, trns_snap%backward)
      call b_trans_address_scalar_snap(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_snap%b_trns, trns_snap%backward)
      trns_snap%backward%num_tensor = 0
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      call f_trans_address_vector_snap(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_snap%f_trns, trns_snap%forward)
      call f_trans_address_scalar_snap(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_snap%f_trns, trns_snap%forward)
       trns_snap%forward%num_tensor = 0
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
      subroutine set_addresses_temporal_trans                           &
     &         (SPH_MHD, iphys, trns_tmp,                               &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_stmp
      use address_fwd_sph_trans_stmp
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for intermediate of snapshot'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      call b_trans_address_vector_stmp(trns_tmp%backward)
      call b_trans_address_scalar_stmp(trns_tmp%backward)
      trns_tmp%backward%num_tensor = 0
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      call f_trans_address_vector_stmp(trns_tmp%forward)
      call f_trans_address_scalar_stmp(SPH_MHD%ipol, SPH_MHD%itor,      &
     &   iphys, trns_tmp%f_trns, trns_tmp%forward)
      trns_tmp%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_tmp%backward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_tmp%forward,                &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_tmp%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_tmp%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_tmp%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_tmp%forward%num_scalar
      end if
!
      end subroutine set_addresses_temporal_trans
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
      type(address_each_sph_trans), intent(in) :: backward
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
      type(address_each_sph_trans), intent(in) :: forward
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
