!>@file   copy_MHD_4_sph_trans.f90
!!@brief  module copy_MHD_4_sph_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_forces_to_snapshot_rtp                          &
!!     &          (m_folding, sph_rtp, trns_MHD, node, iphys, nod_fld)
!!      subroutine copy_filtered_forces_to_snap                         &
!!     &          (m_folding, sph_rtp, trns_MHD, node, iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_MHD_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_rtp_data
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_forces_to_snapshot_rtp                            &
     &          (m_folding, sph_rtp, trns_MHD, node, iphys, nod_fld)
!
      use copy_snap_4_sph_trans
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_m_advect, iphys%i_m_advect,                 &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   Coriolis flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_coriolis, iphys%i_coriolis,                 &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   Lorentz flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_lorentz, iphys%i_lorentz,                   &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   induction flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_vp_induct, iphys%i_vp_induct,               &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   divergence of heat flux flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_h_flux, iphys%i_h_flux,                     &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of composition flux flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_c_flux, iphys%i_c_flux,                     &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine copy_forces_to_snapshot_rtp
!
!-----------------------------------------------------------------------
!
      subroutine copy_filtered_forces_to_snap                           &
     &          (m_folding, sph_rtp, trns_MHD, node, iphys, nod_fld)
!
      use copy_snap_4_sph_trans
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   Lorentz flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   induction flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of heat flux flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of composition flux flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine copy_filtered_forces_to_snap
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
