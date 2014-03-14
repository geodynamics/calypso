!>@file   cvt_nodal_and_sph_field.f90
!!@brief  module cvt_nodal_and_sph_field
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2011
!
!>@brief Convert vector and tensor between spherical grid and FEM grid
!!
!!@verbatim
!!      subroutine cvt_nod_vec_to_sph_vec                               &
!!     &         (numnod, np_smp, inod_smp_stack,                       &
!!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,       &
!!     &          i_field, ntot_phys, d_nod,                            &
!!     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp, d_tmp)
!!      subroutine cvt_sph_vec_to_nod_vec(numnod, internal_node,        &
!!     &          np_smp, inod_smp_stack, colatitude, longitude,        &
!!     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp,                     &
!!     &          i_field, ntot_phys, d_nod, d_tmp)
!!
!!      subroutine cvt_nod_tsr_to_sph_tsr                               &
!!     &         (numnod, np_smp, inod_smp_stack,                       &
!!     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,       &
!!     &          i_field, ntot_phys, d_nod,                            &
!!     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp, d_tmp)
!!      subroutine cvt_sph_tsr_to_nod_tsr(numnod, internal_node,        &
!!     &          np_smp, inod_smp_stack, xx,                           &
!!     &          radius, s_cylinder, a_radius, a_s_cylinder,           &
!!     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp,                     &
!!     &          i_field, ntot_phys, d_nod, d_tmp)
!!@endverbatim
!!
!!@n @param  np_smp          Number of SMP processes
!!@n @param  numnod          Number of FEM nodes
!!@n @param  internal_node   Number of internal FEM nodes
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of FEM nodes for each SMP process
!!@n @param  xx(numnod,3) position of FEM node
!!@n @param  radius(numnod)     radius r of FEM node
!!@n @param  s_cylinder(numnod)   cyrindrical radius s of FEM node
!!@n @param  a_radius(numnod)     1/r of FEM node
!!@n @param  a_s_cylinder(numnod) 1/s of FEM node
!!@n @param  ntot_phys      total number of component
!!                          of fields on FEM mesh
!!@n @param  d_nod(numnod,ntot_phys)   fields on FEM mesh
!!@n @param  i_field        address of field on FEM mesh
!!
!!@n @param  nnod_rtp   Number of spherical coordinate grid
!!                     (No pole or center)
!!@n @param  ntot_rtp    total number of component
!!                          of fields on spherical grid
!!@n @param  d_rtp(numnod,ntot_phys)   fields on spherical grid
!!@n @param  i_rtp          address of field on spherical grid
!!
!!@n @param  d_tmp(numnod,6)   work array for data convert
!!@n @param  d_tmp(nnod_rtp,6) work array for data convert
!
      module cvt_nodal_and_sph_field
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine cvt_nod_vec_to_sph_vec                                 &
     &         (numnod, np_smp, inod_smp_stack,                         &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          i_field, ntot_phys, d_nod,                              &
     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp, d_tmp)
!
      use cvt_xyz_vector_2_sph_smp
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
      integer(kind = kint), intent(in) :: np_smp, numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod, 3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod, ntot_phys)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ntot_rtp
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp, ntot_rtp)
!
      real(kind = kreal), intent(inout) :: d_tmp(numnod,6)
!
!
      call cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,         &
     &            d_nod(1,i_field), d_tmp(1,1), xx, radius,             &
     &            s_cylinder, a_radius, a_s_cylinder)
!
      call copy_vector_2_vector_fld(ione, numnod, isix, d_tmp,          &
     &                              i_rtp, nnod_rtp, ntot_rtp, d_rtp)
!
      end subroutine cvt_nod_vec_to_sph_vec
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_vec_to_nod_vec(numnod, internal_node,          &
     &          np_smp, inod_smp_stack, colatitude, longitude,          &
     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp,                       &
     &          i_field, ntot_phys, d_nod, d_tmp)
!
      use cvt_sph_vector_2_xyz_smp
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
      integer(kind = kint), intent(in) :: np_smp, numnod, internal_node
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: colatitude(numnod)
      real(kind = kreal), intent(in) :: longitude(numnod)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ntot_rtp
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp, ntot_rtp)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
!
      real(kind = kreal), intent(inout) :: d_tmp(numnod,6)
!
!
      call copy_vector_2_vector_fld(i_rtp, nnod_rtp, ntot_rtp, d_rtp,   &
     &    ione, numnod, isix, d_tmp)
!
      call fill_rest_vector_field(i_field, nnod_rtp, ntot_phys, d_nod,  &
     &    ione, numnod, internal_node, isix, d_tmp)
!
      call cvt_sph_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack,       &
     &    d_nod(1,i_field), d_tmp(1,1), colatitude, longitude)
!
      end subroutine cvt_sph_vec_to_nod_vec
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cvt_nod_tsr_to_sph_tsr                                 &
     &         (numnod, np_smp, inod_smp_stack,                         &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder,         &
     &          i_field, ntot_phys, d_nod,                              &
     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp, d_tmp)
!
      use cvt_xyz_tensor_2_sph_smp
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
      integer(kind = kint), intent(in) :: np_smp, numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod, 3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: d_nod(numnod, ntot_phys)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ntot_rtp
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp, ntot_rtp)
      real(kind = kreal), intent(inout) :: d_tmp(numnod,6)
!
!
      call cal_sph_tensor_smp(np_smp, numnod, inod_smp_stack,           &
     &            d_nod(1,i_field), d_tmp(1,1), xx, radius,             &
     &            s_cylinder, a_radius, a_s_cylinder)
!
      call copy_tensor_2_tensor_fld(ione, numnod, isix, d_tmp,          &
     &                              i_rtp, nnod_rtp, ntot_rtp, d_rtp)
!
      end subroutine cvt_nod_tsr_to_sph_tsr
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_tsr_to_nod_tsr(numnod, internal_node,          &
     &          np_smp, inod_smp_stack, xx,                             &
     &          radius, s_cylinder, a_radius, a_s_cylinder,             &
     &          i_rtp, nnod_rtp, ntot_rtp, d_rtp,                       &
     &          i_field, ntot_phys, d_nod, d_tmp)
!
      use cvt_sph_tensor_2_xyz_smp
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
      integer(kind = kint), intent(in) :: np_smp, numnod, internal_node
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod, 3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ntot_rtp
      real(kind = kreal), intent(in) :: d_rtp(nnod_rtp, ntot_rtp)
!
      integer(kind = kint), intent(in) ::  ntot_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
      real(kind = kreal), intent(inout) :: d_tmp(numnod,6)
!
!
      call copy_tensor_2_tensor_fld(i_rtp, nnod_rtp, ntot_rtp, d_rtp,   &
     &    ione, numnod, isix, d_tmp)
!
      call fill_rest_tensor_field(i_field, nnod_rtp, ntot_phys, d_nod,  &
     &    ione, numnod, internal_node, isix, d_tmp)
!
      call cal_xyz_tensor_by_sph_smp(np_smp, numnod,                    &
     &          inod_smp_stack, d_tmp(1,1), d_nod(1,i_field),           &
     &          xx, radius, s_cylinder, a_radius, a_s_cylinder)
!
      end subroutine cvt_sph_tsr_to_nod_tsr
!
! -------------------------------------------------------------------
!
      end module cvt_nodal_and_sph_field
