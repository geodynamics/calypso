!>@file   copy_1st_nodal_4_sph_trans.f90
!!@brief  module copy_1st_nodal_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical transform data to 1st FEM data
!!
!!@verbatim
!!      subroutine copy_nod_scl_from_trans_wpole                        &
!!     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field,          &
!!     &          node, nod_fld)
!!      subroutine copy_nod_vec_from_trans_wpole                        &
!!     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field,          &
!!     &          node, nod_fld)
!!      subroutine copy_nod_tsr_from_trans_wpole                        &
!!     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field,          &
!!     &          node, nod_fld)
!!
!!      subroutine copy_nod_scl_from_sph_trans(d_rtp,  i_field)
!!      subroutine copy_nod_vec_from_sph_trans                          &
!!     &         (d_rtp, i_field, node, nod_fld)
!!      subroutine copy_nod_tsr_from_sph_trans                          &
!!     &         (d_rtp, i_field, node, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(inout) :: nod_fld
!!
!!      subroutine copy_nod_scl_to_sph_trans                            &
!!     &         (node, nod_fld, i_field, d_rtp)
!!      subroutine copy_nod_vec_to_sph_trans                            &
!!     &         (node, nod_fld, i_field, d_rtp)
!!      subroutine copy_nod_tsr_to_sph_trans                            &
!!     &         (node, nod_fld, i_field, d_rtp)
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(in) :: nod_fld
!!@endverbatim
!
      module copy_1st_nodal_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
!
      implicit  none
!
      private :: copy_nod_scl_from_sph_trans
      private :: copy_nod_vec_from_sph_trans
      private :: copy_nod_tsr_from_sph_trans
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_scl_from_trans_wpole                          &
     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field,            &
     &          node, nod_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_pole_sph_trans
      use copy_pole_field_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_nod_scl_from_sph_trans                                  &
     &   (d_rtp(1,i_trns), i_field, node, nod_fld)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    v_pole(1,i_trns), nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_nod_scl_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vec_from_trans_wpole                          &
     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field,            &
     &          node, nod_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_pole_sph_trans
      use copy_pole_field_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_nod_vec_from_sph_trans                                  &
     &   (d_rtp(1,i_trns), i_field, node, nod_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    v_pole(1,i_trns), nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_nod_vec_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_tsr_from_trans_wpole                          &
     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field,            &
     &          node, nod_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_pole_sph_trans
      use copy_pole_field_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_nod_tsr_from_sph_trans                                  &
     &   (d_rtp(1,i_trns), i_field, node, nod_fld)
      call copy_pole_tsr_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    v_pole(1,i_trns), nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_nod_tsr_from_trans_wpole
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_scl_from_sph_trans                            &
     &         (d_rtp, i_field, node, nod_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_scalar_from_sph_trans                                   &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack, node%numnod,         &
     &    d_rtp, i_field, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_nod_scl_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vec_from_sph_trans                            &
     &         (d_rtp, i_field, node, nod_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,3)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_xyz_vec_from_sph_trans                                  &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack,                      &
     &    node%numnod, node%theta, node%phi, d_rtp,                     &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_nod_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_tsr_from_sph_trans                            &
     &         (d_rtp, i_field, node, nod_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,6)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_xyz_tsr_from_sph_trans                                  &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack,                      &
     &    node%numnod, node%xx, node%rr, node%ss, node%a_r, node%a_s,   &
     &    d_rtp, i_field, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_nod_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_scl_to_sph_trans                              &
     &         (node, nod_fld, i_field, d_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp)
!
!
      call copy_scalar_to_sph_trans                                     &
     &   (nnod_rtp, inod_rtp_smp_stack, node%numnod,                    &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_rtp)
!
      end subroutine copy_nod_scl_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vec_to_sph_trans                              &
     &         (node, nod_fld, i_field, d_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,3)
!
!
      call copy_xyz_vec_to_sph_trans                                    &
     &   (nnod_rtp, inod_rtp_smp_stack, node%numnod,                    &
     &    node%xx, node%rr, node%ss, node%a_r, node%a_s,                &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_rtp)
!
      end subroutine copy_nod_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_tsr_to_sph_trans                              &
     &         (node, nod_fld, i_field, d_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,6)
!
!
      call copy_xyz_tsr_to_sph_trans                                    &
     &   (nnod_rtp, inod_rtp_smp_stack, node%numnod,                    &
     &    node%xx, node%rr, node%ss, node%a_r, node%a_s,                &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_rtp)
!
      end subroutine copy_nod_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_1st_nodal_4_sph_trans
