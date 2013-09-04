!>@file   products_sph_fields_smp.f90
!!@brief  module products_sph_fields_smp
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief subroutines to obatine products of two fields on spherical grid
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine clear_rj_field_smp(numdir, i_res)
!!
!!      subroutine ovwrt_sph_coef_prod_vect_smp(coef, i_r)
!!      subroutine cal_rtp_product_4_scalar(i_s1, i_s2, i_r)
!!                d_rtp(,i_r) = d_rtp(,i_v1) * d_rtp(,i_v2)
!!      subroutine cal_rtp_dot_product(i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = d_rtp(,i_v1) \cdot d_rtp(,i_v2)
!!      subroutine cal_rtp_cross_product(i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = d_rtp(,i_v1) \times d_rtp(,i_v2)
!!
!!      subroutine cal_rtp_dot_prod_w_coef(coef, i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = coef * d_rtp(,i_v1) \cdot d_rtp(,i_v2)
!!      subroutine cal_rtp_cross_prod_w_coef(coef, i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = coef * d_rtp(,i_v1) \times d_rtp(,i_v2)
!!      subroutine cal_rtp_tri_product(coef, i_v1, i_v2, i_v3, i_r)
!!
!!      subroutine cal_rtp_scalar_product_vector(i_v1, i_s1, i_r)
!!                d_rtp(,i_r  ) = d_rtp(,i_s1) * d_rtp(,i_v1  )
!!                d_rtp(,i_r+1) = d_rtp(,i_s1) * d_rtp(,i_v1+1)
!!                d_rtp(,i_r+2) = d_rtp(,i_s1) * d_rtp(,i_v1+2)
!!      subroutine cal_rtp_sym_matvec(i_t1, i_v2, i_r)
!!                d_rtp(,i_r  ) =  d_rtp(,i_t1  )*d_rtp(,i_v2  )      &
!!     &                         + d_rtp(,i_t1+1)*d_rtp(,i_v2+1)      &
!!     &                         + d_rtp(,i_t1+2)*d_rtp(,i_v2+2)
!!                d_rtp(,i_r+1) =  d_rtp(,i_t1+1)*d_rtp(,i_v2  )      &
!!     &                         + d_rtp(,i_t1+3)*d_rtp(,i_v2+1)      &
!!     &                         + d_rtp(,i_t1+4)*d_rtp(,i_v2+2)
!!                d_rtp(,i_r+2) =  d_rtp(,i_t1+2)*d_rtp(,i_v2  )      &
!!     &                         + d_rtp(,i_t1+4)*d_rtp(,i_v2+1)      &
!!     &                         + d_rtp(,i_t1+5)*d_rtp(,i_v2+2)
!!       subroutine prod_rtp_scalar_mag_vector(i_s1, i_v2, i_r)
!!
!!       subroutine rtp_vec_scalar_prod_w_coef(coef, i_v1,i_s1, i_r)
!!                d_rtp(,i_r  ) = coef * d_rtp(,i_s1) * d_rtp(,i_v1  )
!!                d_rtp(,i_r+1) = coef * d_rtp(,i_s1) * d_rtp(,i_v1+1)
!!                d_rtp(,i_r+2) = coef * d_rtp(,i_s1) * d_rtp(,i_v1+2)
!!
!!      subroutine rtp_vect_prod_cvec_w_coef_smp(coef, c_vec, i_v2, i_r)
!!      subroutine ovwrt_rtp_cvec_x_prod_w_coef(coef, c_vec, i_r)
!!             d_rtp(,:i_r:i_r+2) = coef * c_vec(1:3)
!!                                 \times d_rtp(,:i_v2:i_v2+2)
!!@endverbatim
!!
!!@n @param  i_s1     Scalar field address for d_rtp
!!@n @param  i_s2     Scalar field address for d_rtp
!!@n @param  i_v1     Vector field address for d_rtp
!!@n @param  i_v2     Vector field address for d_rtp
!!@n @param  i_v3     Vector field address for d_rtp
!!@n @param  i_t1     Symmetric tensor field address for d_rtp
!!@n @param  coef     Scalar coefficients
!!@n @param  c_vec(3) constant vector
!!
!!@n @param  i_r      Result field address for d_rtp
!
      module products_sph_fields_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine clear_rtp_field_smp(numdir, i_r)
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_r, numdir
!
!
      call delete_phys_data_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,   &
     &    ntot_phys_rtp, numdir, i_r, d_rtp)
!
      end subroutine clear_rtp_field_smp
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ovwrt_sph_coef_prod_vect_smp(coef, i_r)
!
      use overwrite_prod_const_smp
!
      real(kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
!
!
      call ovwrt_coef_prod_vect_smp(np_smp, nnod_rtp,                   &
     &   inod_rtp_smp_stack, coef, d_rtp(1,i_r) )
!
      end subroutine ovwrt_sph_coef_prod_vect_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_product_4_scalar(i_s1, i_s2, i_r)
!
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
!
!
      call cal_scalar_prod_no_coef_smp(np_smp, nnod_rtp,                &
     &    inod_rtp_smp_stack, d_rtp(1,i_s1), d_rtp(1,i_s2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_dot_product(i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_dot_prod_no_coef_smp(np_smp, nnod_rtp,                   &
     &    inod_rtp_smp_stack, d_rtp(1,i_v1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_cross_product(i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_cross_prod_no_coef_smp(np_smp, nnod_rtp,                 &
     &    inod_rtp_smp_stack, d_rtp(1,i_v1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_cross_product
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_dot_prod_w_coef(coef, i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_dot_prod_w_coef_smp(np_smp, nnod_rtp,                    &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_v2),       &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_dot_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_cross_prod_w_coef(coef, i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_cross_prod_w_coef_smp(np_smp, nnod_rtp,                  &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_v2),       &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_cross_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_tri_product(coef, i_v1, i_v2, i_v3, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
      integer (kind = kint), intent(in) :: i_v1, i_v2, i_v3
!
!
      call cal_tri_product_w_coef_smp(np_smp, nnod_rtp,                 &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_v2),       &
     &    d_rtp(1,i_v3), d_rtp(1,i_r) )
!
      end subroutine cal_rtp_tri_product
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_scalar_product_vector(i_v1, i_s1, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
!
!
      call cal_vec_scalar_prod_no_coef_smp(np_smp, nnod_rtp,            &
     &    inod_rtp_smp_stack, d_rtp(1,i_v1), d_rtp(1,i_s1),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_scalar_product_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_sym_matvec(i_t1, i_v2, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
!
!
      call cal_tensor_vec_prod_no_coef_smp(np_smp, nnod_rtp,            &
     &    inod_rtp_smp_stack, d_rtp(1,i_t1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_rtp_scalar_mag_vector(i_s1, i_v2, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
!
      call cal_scalar_mag_vector_prod_smp(np_smp, nnod_rtp,             &
     &    inod_rtp_smp_stack, d_rtp(1,i_s1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine prod_rtp_scalar_mag_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine rtp_vec_scalar_prod_w_coef(coef, i_v1, i_s1, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
!
!
      call cal_vec_scalar_prod_w_coef_smp(np_smp, nnod_rtp,             &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_s1),       &
     &    d_rtp(1,i_r) )
!
      end subroutine rtp_vec_scalar_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine rtp_vect_prod_cvec_w_coef_smp(coef, c_vec, i_v2, i_r)
!
      use cal_products_w_const_smp
!
      real (kind = kreal), intent(in) :: coef
      real (kind = kreal), intent(in) :: c_vec(3)
      integer(kind = kint), intent(in) :: i_r, i_v2
!
!
      call cal_vect_prod_cvec_w_coef_smp(np_smp, nnod_rtp,             &
     &    inod_rtp_smp_stack, coef, c_vec(1), d_rtp(1,i_v2),           &
     &    d_rtp(1,i_r) )
!
      end subroutine rtp_vect_prod_cvec_w_coef_smp
!
!-----------------------------------------------------------------------
!
      subroutine ovwrt_rtp_cvec_x_prod_w_coef(coef, c_vec, i_r)
!
      use overwrite_prod_const_smp
!
      real (kind = kreal), intent(in) :: coef
      real (kind = kreal), intent(in) :: c_vec(3)
      integer(kind = kint), intent(in) :: i_r
!
!
!
      call ovwrt_vect_prod_cvec_coef_smp(np_smp, nnod_rtp,              &
     &    inod_rtp_smp_stack, coef, c_vec, d_rtp(1,i_r))
!
      end subroutine ovwrt_rtp_cvec_x_prod_w_coef
!
! ----------------------------------------------------------------------
!
      end module products_sph_fields_smp
