!copy_sph_field_4_sph_trans.f90
!     module copy_sph_field_4_sph_trans
!
!      subroutine copy_scalar_fld_to_trans(nscalar_trans,               &
!     &          is_spec, i_trns)
!      subroutine copy_scalar_fld_from_trans(nscalar_trans,             &
!     &          is_spec, i_trns)
!      subroutine copy_vec_fld_from_trans(nvector_trans,                &
!     &          is_spec, i_trns)
!      subroutine copy_vec_fld_to_trans(nvector_trans, is_spec, i_trns)
!
!      subroutine copy_tensor_fld_from_trans(ntensor_trans,             &
!     &          is_spec, i_trns)
!      subroutine copy_tensor_fld_to_trans(ntensor_trans,               &
!     &          is_spec, i_trns)
!
!      Written by H. Matsui on Feb., 2008
!
      module copy_sph_field_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
      use copy_field_4_sph_trans
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_fld_to_trans(nscalar_trans,                &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_scalar_to_trans(nscalar_trans, i_trns, nnod_rtp,        &
     &    d_rtp(1,is_spec) )
!
      end subroutine copy_scalar_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_fld_from_trans(nscalar_trans,              &
     &           is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_scalar_from_trans(nscalar_trans, i_trns,                &
     &    nnod_rtp, d_rtp(1,is_spec) )
!
      end subroutine copy_scalar_fld_from_trans
!
!-----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_vec_fld_from_trans(nvector_trans,                 &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nvector_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_vector_from_trans(nvector_trans, i_trns,                &
     &    nnod_rtp, d_rtp(1,is_spec) )
!
      end subroutine copy_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vec_fld_to_trans(nvector_trans, is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nvector_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_vector_to_trans(nvector_trans, i_trns,                  &
     &    nnod_rtp, d_rtp(1,is_spec) )
!
      end subroutine copy_vec_fld_to_trans
!
!-----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_tensor_fld_from_trans(ntensor_trans,              &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ntensor_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_tensor_from_trans(ntensor_trans, i_trns,                &
     &    nnod_rtp, d_rtp(1,is_spec) )
!
      end subroutine copy_tensor_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_fld_to_trans(ntensor_trans,                &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ntensor_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_tensor_to_trans(ntensor_trans, i_trns,                  &
     &    nnod_rtp, d_rtp(1,is_spec) )
!
      end subroutine copy_tensor_fld_to_trans
!
!-----------------------------------------------------------------------
!
      end module copy_sph_field_4_sph_trans
