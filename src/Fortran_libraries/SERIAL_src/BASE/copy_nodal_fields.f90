!>@file   copy_nodal_fields.f90
!!@brief  module copy_nodal_fields
!!
!!@author H. Matsui
!!@date Programmed in ??
!
!>@brief Copy fields in structure
!!
!!@verbatim
!!      subroutine clear_field_data(fld, numdir, i_target)
!!
!!      subroutine copy_scalar_component(fld, i_org, i_target)
!!      subroutine copy_vector_component(fld, i_org, i_target)
!!      subroutine copy_tensor_component(fld, i_org, i_target)
!!        integer (kind = kint), intent(in) :: i_target, i_org
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine add_2_nod_scalars(fld, i_v1, i_v2, i_r)
!!      subroutine add_2_nod_vectors(fld, i_v1, i_v2, i_r)
!!      subroutine add_2_nod_tensors(fld, i_v1, i_v2, i_r)
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) + d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!
!!      subroutine subtract_2_nod_scalars(fld, i_v1, i_v2, i_r)
!!      subroutine subtract_2_nod_vectors(fld, i_v1, i_v2, i_r)
!!      subroutine subtract_2_nod_tensors(fld, i_v1, i_v2, i_r)
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) - d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!@endverbatim
!
      module copy_nodal_fields
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine clear_field_data(fld, numdir, i_target)
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_target, numdir
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call delete_phys_data_smp(fld%n_point, ione, fld%n_point,         &
     &    fld%ntot_phys, numdir, i_target, fld%d_fld)
!$omp end parallel
!
      end subroutine clear_field_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_component(fld, i_org, i_target)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call copy_nod_scalar_smp(fld%n_point,                             &
     &    fld%d_fld(1,i_org), fld%d_fld(1,i_target))
!$omp end parallel
!
      end subroutine copy_scalar_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_component(fld, i_org, i_target)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call copy_nod_vector_smp(fld%n_point,                             &
     &    fld%d_fld(1,i_org), fld%d_fld(1,i_target))
!$omp end parallel
!
      end subroutine copy_vector_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_component(fld, i_org, i_target)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call copy_nod_sym_tensor_smp(fld%n_point,                         &
     &    fld%d_fld(1,i_org), fld%d_fld(1,i_target))
!$omp end parallel
!
      end subroutine copy_tensor_component
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_scalars(fld, i_v1, i_v2, i_r)
!
      use cal_add_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call add_scalars_smp(fld%n_point,                                 &
     &    fld%d_fld(1,i_v1), fld%d_fld(1,i_v2), fld%d_fld(1,i_r))
!$omp end parallel
!
       end subroutine add_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_vectors(fld, i_v1, i_v2, i_r)
!
      use cal_add_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call add_vectors_smp(fld%n_point,                                 &
     &    fld%d_fld(1,i_v1), fld%d_fld(1,i_v2), fld%d_fld(1,i_r))
!$omp end parallel
!
       end subroutine add_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_tensors(fld, i_v1, i_v2, i_r)
!
      use cal_add_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call add_tensors_smp(fld%n_point,                                 &
     &    fld%d_fld(1,i_v1), fld%d_fld(1,i_v2), fld%d_fld(1,i_r))
!$omp end parallel
!
      end subroutine add_2_nod_tensors
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_scalars(fld, i_v1, i_v2, i_r)
!
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call subtract_scalars_smp(fld%n_point,                            &
     &    fld%d_fld(1,i_v1), fld%d_fld(1,i_v2), fld%d_fld(1,i_r))
!$omp end parallel
!
       end subroutine subtract_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_vectors(fld, i_v1, i_v2, i_r)
!
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call subtract_vectors_smp(fld%n_point,                            &
     &    fld%d_fld(1,i_v1), fld%d_fld(1,i_v2), fld%d_fld(1,i_r))
!$omp end parallel
!
       end subroutine subtract_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_tensors(fld, i_v1, i_v2, i_r)
!
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call subtract_tensors_smp(fld%n_point,                            &
     &    fld%d_fld(1,i_v1), fld%d_fld(1,i_v2), fld%d_fld(1,i_r))
!$omp end parallel
!
      end subroutine subtract_2_nod_tensors
!
!-----------------------------------------------------------------------
!
      end module copy_nodal_fields
