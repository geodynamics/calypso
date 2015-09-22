!>@file   copy_xyz_field_4_sph_trans.f90
!!@brief  module copy_xyz_field_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical harmonics data to FEM data directly
!!
!!@verbatim
!!      subroutine copy_scalar_from_sph_trans                           &
!!     &       (nnod_rtp, m_folding, inod_rtp_smp_stack,numnod,         &
!!     &        d_rtp, i_field, ntot_phys, d_nod)
!!      subroutine copy_xyz_vec_from_sph_trans                          &
!!     &       (nnod_rtp, m_folding, inod_rtp_smp_stack, numnod,        &
!!     &        colatitude, longitude, d_rtp, i_field, ntot_phys, d_nod)
!!      subroutine copy_xyz_tsr_from_sph_trans                          &
!!     &         (nnod_rtp, m_folding, inod_rtp_smp_stack, numnod,      &
!!     &        xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
!!     &        d_rtp, i_field, ntot_phys, d_nod)
!!
!!      subroutine copy_scalar_to_sph_trans(nnod_rtp,inod_rtp_smp_stack,&
!!     &          numnod, i_field, ntot_phys, d_nod, d_rtp)
!!      subroutine copy_xyz_vec_to_sph_trans                            &
!!     &         (nnod_rtp, inod_rtp_smp_stack, numnod,                 &
!!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,       &
!!     &          i_field, ntot_phys, d_nod, d_rtp)
!!      subroutine copy_xyz_tsr_to_sph_trans                            &
!!     &         (nnod_rtp, inod_rtp_smp_stack, numnod,                 &
!!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,       &
!!     &          i_field, ntot_phys, d_nod, d_rtp)
!!@endverbatim
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
      subroutine copy_scalar_from_sph_trans                             &
     &       (nnod_rtp, m_folding, inod_rtp_smp_stack, numnod,          &
     &        d_rtp, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
!
      integer(kind = kint), intent(in) :: nnod_rtp, m_folding
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: numnod
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
!$omp parallel
      call copy_scalar_from_trans(nnod_rtp, m_folding,                  &
     &    inod_rtp_smp_stack, numnod, d_rtp, d_nod(1,i_field))
!$omp end parallel
!
      end subroutine copy_scalar_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_vec_from_sph_trans                            &
     &       (nnod_rtp, m_folding, inod_rtp_smp_stack, numnod,          &
     &        colatitude, longitude, d_rtp, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use cvt_sph_vector_2_xyz_smp
!
      integer(kind = kint), intent(in) :: i_field
!
      integer(kind = kint), intent(in) :: nnod_rtp, m_folding
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: colatitude(numnod)
      real(kind = kreal), intent(in) :: longitude(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
      call copy_vector_from_trans(nnod_rtp, m_folding,                  &
     &    inod_rtp_smp_stack, numnod, d_rtp, d_nod(1,i_field))
!
!$omp parallel
      call overwrite_sph_vect_2_xyz_smp(np_smp, numnod,                 &
     &    inod_rtp_smp_stack, d_nod(1,i_field), colatitude, longitude)
!$omp end parallel
!
      end subroutine copy_xyz_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_tsr_from_sph_trans                            &
     &         (nnod_rtp, m_folding, inod_rtp_smp_stack, numnod,        &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          d_rtp, i_field, ntot_phys, d_nod)
!
      use copy_field_4_sph_trans
      use cvt_sph_tensor_2_xyz_smp
!
      integer(kind = kint), intent(in) :: i_field
!
      integer(kind = kint), intent(in) :: nnod_rtp, m_folding
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp,6)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
      call copy_tensor_from_trans(nnod_rtp, m_folding,                  &
     &    inod_rtp_smp_stack, numnod, d_rtp, d_nod(1,i_field))
!
!$omp parallel
      call overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,              &
     &    inod_rtp_smp_stack, d_nod(1,i_field), xx(1,1), xx(1,2),       &
     &    xx(1,3), radius, s_cylinder, a_radius, a_s_cylinder)
!$omp end parallel
!
      end subroutine copy_xyz_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_scalar_to_sph_trans(nnod_rtp, inod_rtp_smp_stack, &
     &          numnod, i_field, ntot_phys, d_nod, d_rtp)
!
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: numnod
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp)
!
!
      call copy_scalar_to_trans(nnod_rtp, inod_rtp_smp_stack, numnod,   &
     &    d_nod(1,i_field), d_rtp)
!
      end subroutine copy_scalar_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_vec_to_sph_trans                              &
     &         (nnod_rtp, inod_rtp_smp_stack, numnod,                   &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          i_field, ntot_phys, d_nod, d_rtp)
!
      use copy_field_4_sph_trans
      use cvt_xyz_vector_2_sph_smp
!
      integer(kind = kint), intent(in) :: i_field
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,3)
!
!
      call copy_vector_to_trans(nnod_rtp, inod_rtp_smp_stack, numnod,   &
     &    d_nod(1,i_field), d_rtp)
!
!$omp parallel
      call overwrite_vector_2_sph_smp                                   &
     &   (np_smp, nnod_rtp, inod_rtp_smp_stack, d_rtp,                  &
     &    xx(1,1), xx(1,2), xx(1,3), radius(1), s_cylinder(1),          &
     &    a_radius(1), a_s_cylinder(1) )
!$omp end parallel
!
      end subroutine copy_xyz_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_xyz_tsr_to_sph_trans                              &
     &         (nnod_rtp, inod_rtp_smp_stack, numnod,                   &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          i_field, ntot_phys, d_nod, d_rtp)
!
      use copy_field_4_sph_trans
      use cvt_xyz_tensor_2_sph_smp
!
      integer(kind = kint), intent(in) :: i_field
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,6)
!
!
      call copy_tensor_to_trans(nnod_rtp, inod_rtp_smp_stack, numnod,   &
     &    d_nod(1,i_field), d_rtp)
!
!$omp parallel
      call overwrite_sph_tensor_smp                                     &
     &   (np_smp, nnod_rtp, inod_rtp_smp_stack, d_rtp,                  &
     &    xx(1,1), xx(1,2), xx(1,3), radius(1), s_cylinder(1),          &
     &    a_radius(1), a_s_cylinder(1) )
!$omp end parallel
!
      end subroutine copy_xyz_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_xyz_field_4_sph_trans
