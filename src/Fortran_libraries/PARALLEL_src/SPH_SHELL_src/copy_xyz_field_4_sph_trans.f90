!copy_xyz_field_4_sph_trans.f90
!      module copy_xyz_field_4_sph_trans
!
!      Written by H. Matsui on Nov., 2012
!
!      subroutine copy_scalar_from_trans_w_pole(numnod, internal_node,  &
!     &          xx, ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!      subroutine copy_xyz_vec_from_trans_w_pole(numnod, internal_node, &
!     &          inod_smp_stack, xx, colatitude, longitude,             &
!     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!      subroutine copy_xyz_tsr_from_trans_w_pole                        &
!     &         (numnod, internal_node, inod_smp_stack,                 &
!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,        &
!     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
!      subroutine copy_scalar_from_sph_trans(numnod,                    &
!     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!      subroutine copy_xyz_vec_from_sph_trans(numnod, inod_smp_stack,   &
!     &          colatitude, longitude, ncomp_trans, i_trns, i_field,   &
!     &          ntot_phys, d_nod)
!      subroutine copy_xyz_tsr_from_sph_trans(numnod, inod_smp_stack,   &
!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,        &
!     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
!      subroutine copy_scalar_to_sph_trans(numnod, ncomp_trans,         &
!     &          i_trns, i_field, ntot_phys, d_nod)
!      subroutine copy_xyz_vec_to_sph_trans(numnod, inod_smp_stack,     &
!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,        &
!     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!      subroutine copy_xyz_tsr_to_sph_trans(numnod, inod_smp_stack,     &
!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,        &
!     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      module copy_xyz_field_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_from_trans_w_pole(numnod, internal_node,   &
     &          xx, ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use copy_pole_field_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_scalar_from_trans(ncomp_trans, i_trns,                  &
     &    numnod, d_nod(1,i_field))
!$omp end parallel
!
      call copy_pole_scl_fld_from_trans(numnod, internal_node,          &
     &    xx, ntot_phys, ncomp_trans, i_field, i_trns, d_nod )
!
      end subroutine copy_scalar_from_trans_w_pole
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_vec_from_trans_w_pole(numnod, internal_node,  &
     &          inod_smp_stack, xx, colatitude, longitude,              &
     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use copy_pole_field_sph_trans
      use cvt_sph_vector_2_xyz_smp
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: colatitude(numnod)
      real(kind = kreal), intent(in) :: longitude(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_vector_from_trans(ncomp_trans, i_trns,                  &
     &    numnod, d_nod(1,i_field))
!$omp end parallel
!
      call copy_pole_vec_fld_from_trans(numnod, internal_node,          &
     &    xx, ntot_phys, ncomp_trans, i_field, i_trns, d_nod )
!
      call overwrite_sph_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack, &
     &    d_nod(1,i_field), colatitude, longitude)
!
      end subroutine copy_xyz_vec_from_trans_w_pole
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_tsr_from_trans_w_pole                         &
     &         (numnod, internal_node, inod_smp_stack,                  &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use copy_pole_field_sph_trans
      use cvt_sph_tensor_2_xyz_smp
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_tensor_from_trans(ncomp_trans, i_trns,                  &
     &    numnod, d_nod(1,i_field))
!$omp end parallel
!
      call copy_pole_tsr_fld_from_trans(numnod, internal_node,          &
     &    xx, ntot_phys, ncomp_trans, i_field, i_trns, d_nod)
!
      call overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,              &
     &    inod_smp_stack, d_nod(1,i_field), xx, radius, s_cylinder,     &
     &    a_radius, a_s_cylinder)
!
      end subroutine copy_xyz_tsr_from_trans_w_pole
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_scalar_from_sph_trans(numnod,                     &
     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_scalar_from_trans(ncomp_trans, i_trns,                  &
     &    numnod, d_nod(1,i_field))
!$omp end parallel
!
      end subroutine copy_scalar_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_vec_from_sph_trans(numnod, inod_smp_stack,    &
     &          colatitude, longitude, ncomp_trans, i_trns, i_field,    &
     &          ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use cvt_sph_vector_2_xyz_smp
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: colatitude(numnod)
      real(kind = kreal), intent(in) :: longitude(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_vector_from_trans(ncomp_trans, i_trns,                &
     &    numnod, d_nod(1,i_field))
!$omp end parallel
!
      call overwrite_sph_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack, &
     &    d_nod(1,i_field), colatitude, longitude)
!
      end subroutine copy_xyz_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_tsr_from_sph_trans(numnod, inod_smp_stack,    &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use cvt_sph_tensor_2_xyz_smp
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_tensor_from_trans(ncomp_trans, i_trns,                  &
     &    numnod, d_nod(1,i_field))
!$omp end parallel
!
      call overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,              &
     &    inod_smp_stack, d_nod(1,i_field), xx, radius, s_cylinder,     &
     &    a_radius, a_s_cylinder)
!
      end subroutine copy_xyz_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_scalar_to_sph_trans(numnod, ncomp_trans,          &
     &          i_trns, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_scalar_to_trans(ncomp_trans, i_trns, numnod,            &
     &    d_nod(1,i_field))
!$omp end parallel
!
      end subroutine copy_scalar_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_vec_to_sph_trans(numnod, inod_smp_stack,      &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use m_work_4_sph_trans
      use copy_field_4_sph_trans
      use cvt_xyz_vector_2_sph_smp
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
!
!
      call cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,         &
     &            d_nod(1,i_field), d_nod_rtp(1,1), xx, radius,         &
     &            s_cylinder, a_radius, a_s_cylinder)
!
!$omp parallel
      call copy_vector_tmp_to_trans(ncomp_trans, i_trns)
!$omp end parallel
!
      end subroutine copy_xyz_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_tsr_to_sph_trans(numnod, inod_smp_stack,      &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          ncomp_trans, i_trns, i_field, ntot_phys, d_nod)
!
      use m_work_4_sph_trans
      use copy_field_4_sph_trans
      use cvt_xyz_tensor_2_sph_smp
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
!
!
      call cal_sph_tensor_smp(np_smp, numnod, inod_smp_stack,           &
     &    d_nod(1,i_field), d_nod_rtp, xx, radius,                      &
     &    s_cylinder, a_radius, a_s_cylinder)
!
!$omp parallel
      call copy_tensor_tmp_to_trans(ncomp_trans, i_trns)
!$omp end parallel
!
      end subroutine copy_xyz_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_xyz_field_4_sph_trans
