!>@file   copy_fields_from_sph_trans.f90
!!@brief  module copy_fields_from_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_scalar_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, backward, node, nod_fld)
!!      subroutine copy_vector_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, backward, node, nod_fld)
!!      subroutine copy_tensor_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, backward, node, nod_fld)
!!      subroutine copy_scalar_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, forward, node, nod_fld)
!!      subroutine copy_vector_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, forward, node, nod_fld)
!!      subroutine copy_tensor_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, forward, node, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_each_sph_trans), intent(in) :: forward
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_fields_from_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
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
      subroutine copy_scalar_from_snap_trans(i_trns, i_field,           &
     &          m_folding, sph_rtp, backward, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_each_sph_trans), intent(in) :: backward
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_scl_from_trans_wpole(sph_rtp, m_folding,            &
     &    backward%ncomp, i_trns, backward%fld_rtp, backward%fld_pole,  &
     &    i_field, node, nod_fld)
!
      end subroutine copy_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_trans(i_trns, i_field,           &
     &         m_folding, sph_rtp, backward, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_each_sph_trans), intent(in) :: backward
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_vec_from_trans_wpole(sph_rtp, m_folding,            &
     &    backward%ncomp, i_trns, backward%fld_rtp, backward%fld_pole,  &
     &    i_field, node, nod_fld)
!
      end subroutine copy_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_snap_trans(i_trns, i_field,           &
     &         m_folding, sph_rtp, backward, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_each_sph_trans), intent(in) :: backward
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_tsr_from_trans_wpole(sph_rtp, m_folding,            &
     &    backward%ncomp, i_trns, backward%fld_rtp, backward%fld_pole,  &
     &    i_field, node, nod_fld)
!
      end subroutine copy_tensor_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, forward, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_each_sph_trans), intent(in) :: forward
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_scl_from_trans_wpole(sph_rtp, m_folding,            &
     &    forward%ncomp, i_trns, forward%fld_rtp, forward%fld_pole,     &
     &    i_field, node, nod_fld)
!
      end subroutine copy_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, forward, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_each_sph_trans), intent(in) :: forward
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_vec_from_trans_wpole(sph_rtp, m_folding,            &
     &    forward%ncomp, i_trns, forward%fld_rtp, forward%fld_pole,     &
     &    i_field, node, nod_fld)
!
      end subroutine copy_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, forward, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_each_sph_trans), intent(in) :: forward
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_tsr_from_trans_wpole(sph_rtp, m_folding,            &
     &    forward%ncomp, i_trns, forward%fld_rtp, forward%fld_pole,     &
     &    i_field, node, nod_fld)
!
      end subroutine copy_tensor_from_snap_force
!
!-----------------------------------------------------------------------
!
      end module copy_fields_from_sph_trans
