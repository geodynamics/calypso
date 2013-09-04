!>@file   sph_transform_each_fld.f90
!!@brief  module sph_transform_each_fld
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for each field
!!
!!@verbatim
!!      subroutine sph_back_trans_4_vector(is_fld, irtp_fld)
!!      subroutine sph_back_trans_4_scalar(is_fld, irtp_fld)
!!
!!      subroutine sph_forward_trans_4_vector(irtp_fld, is_fld)
!!      subroutine sph_forward_trans_4_scalar(irtp_fld, is_fld)
!!@endverbatim
!!
!!@n @param is_fld    address of spectrum data (Poloidal component)
!!@n @param irtp_fld  address of field data
!
!
      module sph_transform_each_fld
!
      use m_precision
!
      use m_constants
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_vector(is_fld, irtp_fld)
!
      use copy_spectr_4_sph_trans
      use sph_trans_vector
      use copy_sph_field_4_sph_trans
!
      integer(kind = kint), intent(in)  :: is_fld, irtp_fld
!
!
      if( (is_fld*irtp_fld) .gt. 0) return
!
!$omp parallel
      call copy_vec_spec_to_trans(ione, is_fld, ione)
!$omp end parallel
!
      call sph_b_trans_vector(ione)
!
!$omp parallel
      call copy_vec_fld_from_trans(ione, irtp_fld, ione)
!$omp end parallel
!
      end subroutine sph_back_trans_4_vector
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_scalar(is_fld, irtp_fld)
!
      use copy_spectr_4_sph_trans
      use sph_trans_scalar
      use copy_sph_field_4_sph_trans
!
      integer(kind = kint), intent(in)  :: is_fld, irtp_fld
!
!
      if( (is_fld*irtp_fld) .gt. 0) return
!
!$omp parallel
      call copy_scalar_spec_to_trans(ione, is_fld, ione)
!$omp end parallel
!
      call sph_b_trans_scalar(ione)
!
!$omp parallel
      call copy_scalar_fld_from_trans(ione, irtp_fld, ione)
!$omp end parallel
!
      end subroutine sph_back_trans_4_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_vector(irtp_fld, is_fld)
!
      use copy_spectr_4_sph_trans
      use sph_trans_vector
      use copy_sph_field_4_sph_trans
!
      integer(kind = kint), intent(in)  :: is_fld, irtp_fld
!
!
      if( (is_fld*irtp_fld) .gt. 0) return
!
!$omp parallel
      call copy_vec_fld_to_trans(ione, irtp_fld, ione)
!$omp end parallel
!
      call sph_f_trans_vector(ione)
!
!$omp parallel
      call copy_vec_spec_from_trans(ione, is_fld, ione)
!$omp end parallel
!
      end subroutine sph_forward_trans_4_vector
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_scalar(irtp_fld, is_fld)
!
      use copy_spectr_4_sph_trans
      use sph_trans_scalar
      use copy_sph_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld
!
!
      if( (is_fld*irtp_fld) .gt. 0) return
!
!$omp parallel
      call copy_scalar_fld_to_trans(ione, irtp_fld, ione)
!$omp end parallel
!
      call sph_f_trans_scalar(ione)
!
!$omp parallel
      call copy_scalar_spec_from_trans(ione, is_fld, ione)
!$omp end parallel
!
      end subroutine sph_forward_trans_4_scalar
!
!-----------------------------------------------------------------------
!
      end module sph_transform_each_fld
