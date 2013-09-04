!cvt_nod_fld_and_sph_fld.f90
!     module cvt_nod_fld_and_sph_fld
!
!      Written by H. Matsui on Feb., 2008
!
!      subroutine cvt_vec_fld_to_sph_vec(i_rtp, i_field)
!      subroutine cvt_sph_vec_to_vec_fld(i_rtp, i_field)
!
!      subroutine cvt_tsr_fld_to_sph_tsr(i_rtp, i_field)
!      subroutine cvt_sph_tsr_to_tsr_fld(i_rtp, i_field)
!
!      subroutine copy_sph_scalar_2_scl_fld(i_rtp, i_field)
!      subroutine copy_sph_vector_2_vec_fld(i_rtp, i_field)
!      subroutine copy_sph_tensor_2_tsr_fld(i_rtp, i_field)
!
!      subroutine copy_scl_fld_2_sph_vector(i_rtp, i_field)
!      subroutine copy_vct_fld_2_sph_vector(i_rtp, i_field)
!      subroutine copy_tsr_fld_2_sph_tensor(i_rtp, i_field)
!
      module cvt_nod_fld_and_sph_fld
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine cvt_vec_fld_to_sph_vec(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_nod_vec_to_sph_vec(numnod, np_smp, inod_smp_stack,       &
     &    xx, radius, s_cylinder, a_radius, a_s_cylinder,               &
     &    i_field, num_tot_nod_phys, d_nod,                             &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp, d_nod_rtp)
!
      end subroutine cvt_vec_fld_to_sph_vec
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_vec_to_vec_fld(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_sph_vec_to_nod_vec                                       &
     &   (numnod, np_smp, inod_smp_stack, colatitude, longitude,        &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, num_tot_nod_phys, d_nod, d_nod_rtp)
!
      end subroutine cvt_sph_vec_to_vec_fld
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cvt_tsr_fld_to_sph_tsr(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_nod_tsr_to_sph_tsr(numnod, np_smp, inod_smp_stack,       &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          i_field, num_tot_nod_phys, d_nod,                       &
     &          i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp, d_nod_rtp)
!
      end subroutine cvt_tsr_fld_to_sph_tsr
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_tsr_to_tsr_fld(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_sph_tsr_to_nod_tsr(numnod, np_smp, inod_smp_stack,       &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                  &
     &          i_field, num_tot_nod_phys, d_nod, d_nod_rtp)
!
      end subroutine cvt_sph_tsr_to_tsr_fld
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_sph_scalar_2_scl_fld(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_scalar_2_scalar_fld                                     &
     &         (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                  &
     &          i_field, numnod, num_tot_nod_phys, d_nod)
!
      end subroutine copy_sph_scalar_2_scl_fld
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_vector_2_vec_fld(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_vector_2_vector_fld                                     &
     &         (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                  &
     &          i_field, numnod, num_tot_nod_phys, d_nod)
!
      end subroutine copy_sph_vector_2_vec_fld
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_tensor_2_tsr_fld(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_tensor_2_tensor_fld                                     &
     &         (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                  &
     &          i_field, numnod, num_tot_nod_phys, d_nod)
!
      end subroutine copy_sph_tensor_2_tsr_fld
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_scl_fld_2_sph_vector(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_scalar_2_scalar_fld                                     &
     &         (i_field, numnod, num_tot_nod_phys, d_nod,               &
     &          i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_scl_fld_2_sph_vector
!
! -------------------------------------------------------------------
!
      subroutine copy_vct_fld_2_sph_vector(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_vector_2_vector_fld                                     &
     &         (i_field, numnod, num_tot_nod_phys, d_nod,               &
     &          i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_vct_fld_2_sph_vector
!
! -------------------------------------------------------------------
!
      subroutine copy_tsr_fld_2_sph_tensor(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_tensor_2_tensor_fld                                     &
     &         (i_field, numnod, num_tot_nod_phys, d_nod,               &
     &          i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_tsr_fld_2_sph_tensor
!
! -------------------------------------------------------------------
!
      end module cvt_nod_fld_and_sph_fld
