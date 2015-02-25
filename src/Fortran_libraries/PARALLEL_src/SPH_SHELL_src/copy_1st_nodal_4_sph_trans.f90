!>@file   copy_1st_nodal_4_sph_trans.f90
!!@brief  module copy_1st_nodal_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical transform data to 1st FEM data
!!
!!@verbatim
!!      subroutine copy_1st_scl_from_trans_wpole                        &
!!     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field)
!!      subroutine copy_1st_vec_from_trans_wpole                        &
!!     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field)
!!      subroutine copy_1st_tsr_from_trans_wpole                        &
!!     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field)
!!
!!      subroutine copy_1st_scl_from_sph_trans(d_rtp,  i_field)
!!      subroutine copy_1st_vec_from_sph_trans(d_rtp, i_field)
!!      subroutine copy_1st_tsr_from_sph_trans(d_rtp, i_field)
!!
!!      subroutine copy_1st_scl_to_sph_trans(i_field, d_rtp)
!!      subroutine copy_1st_vec_to_sph_trans(i_field, d_rtp)
!!      subroutine copy_1st_tsr_to_sph_trans(i_field, d_rtp)
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
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_scl_from_trans_wpole                          &
     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field)
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
!
      call copy_1st_scl_from_sph_trans(d_rtp(1,i_trns), i_field)
      call copy_pole_scl_fld_from_trans                                 &
     &   (numnod, internal_node, xx, v_pole(1,i_trns),                  &
     &    num_tot_nod_phys, i_field, d_nod)
!
      end subroutine copy_1st_scl_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_vec_from_trans_wpole                          &
     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field)
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
!
      call copy_1st_vec_from_sph_trans(d_rtp(1,i_trns), i_field)
      call copy_pole_vec_fld_from_trans                                 &
     &   (numnod, internal_node, xx, v_pole(1,i_trns),                  &
     &    num_tot_nod_phys, i_field, d_nod)
!
      end subroutine copy_1st_vec_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_tsr_from_trans_wpole                          &
     &         (ncomp_trans, i_trns, d_rtp, v_pole, i_field)
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
!
      call copy_1st_tsr_from_sph_trans(d_rtp(1,i_trns), i_field)
      call copy_pole_tsr_fld_from_trans                                 &
     &   (numnod, internal_node, xx, v_pole(1,i_trns),                  &
     &    num_tot_nod_phys, i_field, d_nod)
!
      end subroutine copy_1st_tsr_from_trans_wpole
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_1st_scl_from_sph_trans(d_rtp,  i_field)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp)
!
!
      call copy_scalar_from_sph_trans(nnod_rtp, inod_rtp_smp_stack,     &
     &    numnod, d_rtp, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_scl_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_vec_from_sph_trans(d_rtp, i_field)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,3)
!
!
      call copy_xyz_vec_from_sph_trans(nnod_rtp, inod_rtp_smp_stack,    &
     &    numnod, colatitude, longitude, d_rtp,                         &
     &    i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_tsr_from_sph_trans(d_rtp, i_field)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,6)
!
!
      call copy_xyz_tsr_from_sph_trans(nnod_rtp, inod_rtp_smp_stack,    &
     &    numnod, xx, radius, s_cylinder, a_radius, a_s_cylinder,       &
     &    d_rtp, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_1st_scl_to_sph_trans(i_field, d_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp)
!
!
      call copy_scalar_to_sph_trans(nnod_rtp, inod_rtp_smp_stack,       &
     &    numnod, i_field, num_tot_nod_phys, d_nod, d_rtp)
!
      end subroutine copy_1st_scl_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_vec_to_sph_trans(i_field, d_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,3)
!
!
      call copy_xyz_vec_to_sph_trans                                    &
     &   (nnod_rtp, inod_rtp_smp_stack, numnod,                         &
     &    xx, radius, s_cylinder, a_radius, a_s_cylinder,               &
     &    i_field, num_tot_nod_phys, d_nod, d_rtp)
!
      end subroutine copy_1st_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_tsr_to_sph_trans(i_field, d_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,6)
!
!
      call copy_xyz_tsr_to_sph_trans                                    &
     &   (nnod_rtp, inod_rtp_smp_stack, numnod,                         &
     &    xx, radius, s_cylinder, a_radius, a_s_cylinder,               &
     &    i_field, num_tot_nod_phys, d_nod, d_rtp)
!
      end subroutine copy_1st_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_1st_nodal_4_sph_trans
