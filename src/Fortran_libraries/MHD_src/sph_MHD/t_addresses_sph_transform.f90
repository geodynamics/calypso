!>@file   t_addresses_sph_transform.f90
!!@brief  module t_addresses_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine alloc_bwd_trns_field_name(num_field, trns)
!!      subroutine alloc_fwd_trns_field_name(num_field, trns)
!!      subroutine alloc_nonlinear_data(nnod_rtp, trns)
!!      subroutine alloc_nonlinear_pole(nnod_pole, trns)
!!      subroutine dealloc_bwd_trns_field_name(trns)
!!      subroutine dealloc_fwd_trns_field_name(trns)
!!      subroutine dealloc_nonlinear_data(trns)
!!      subroutine dealloc_nonlinear_pole(trns)
!!        type(address_4_sph_trans), intent(inout) :: trns
!!
!!      subroutine add_scalar_trans_flag                                &
!!     &         (is_fld, nfield_vec, num_trans, itrans)
!!      subroutine add_vector_trans_flag(is_fld, num_trans, itrans)
!!
!!      subroutine add_scl_trans_flag_snap(is_fld, irtp_fld,            &
!!     &          nfield_vec, num_trans, itrans)
!!      subroutine add_vec_trans_flag_snap(is_fld, irtp_fld,            &
!!     &          num_trans, itrans)
!!@endverbatim
!
      module t_addresses_sph_transform
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      strucutre for spherical transform data addresses
      type address_4_sph_trans
!>        number of components for backward spherical harmonics transform
        integer(kind = kint) :: ncomp_rj_2_rtp = 0
!>        number of components
!!        for backward vector spherical harmonics transform
        integer(kind = kint) :: nvector_rj_2_rtp = 0
!>        number of scalars for backward spherical harmonics transform
        integer(kind = kint) :: nscalar_rj_2_rtp = 0
!>        number of tensors for backward spherical harmonics transform
        integer(kind = kint) :: ntensor_rj_2_rtp = 0
!
!>        number of components for forward spherical harmonics transform
        integer(kind = kint) :: ncomp_rtp_2_rj = 0
!>        number of vectors for forward spherical harmonics transform
        integer(kind = kint) :: nvector_rtp_2_rj = 0
!>        number of scalars for forward spherical harmonics transform
        integer(kind = kint) :: nscalar_rtp_2_rj = 0
!>        number of tensors for forward spherical harmonics transform
        integer(kind = kint) :: ntensor_rtp_2_rj = 0
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
!
!>        number of components for backward spherical harmonics transform
        integer(kind = kint) :: nfield_rj_2_rtp = 0
!>        Field name for backward transform
        character(len = kchara), allocatable :: b_trns_name(:)
!
!>        number of components for backward spherical harmonics transform
        integer(kind = kint) :: nfield_rtp_2_rj = 0
!>        Field name for forward transform
        character(len = kchara), allocatable :: f_trns_name(:)
!
!>        field data in grid space
        real(kind = kreal), allocatable :: fld_rtp(:,:)
!>        Nonliear terms data in grid space
        real(kind = kreal), allocatable :: frc_rtp(:,:)
!
!>        field data at pole
        real(kind = kreal), allocatable :: fld_pole(:,:)
!>        local field data at pole
        real(kind = kreal), allocatable :: flc_pole(:,:)
!
!>        Nonlinear terms data at pole
        real(kind = kreal), allocatable :: frc_pole(:,:)
      end type address_4_sph_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_bwd_trns_field_name(num_field, trns)
!
      integer(kind = kint), intent(in) :: num_field
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      trns%nfield_rj_2_rtp = num_field
      allocate(trns%b_trns_name(trns%nfield_rj_2_rtp))
!
      end subroutine alloc_bwd_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine alloc_fwd_trns_field_name(num_field, trns)
!
      integer(kind = kint), intent(in) :: num_field
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      trns%nfield_rtp_2_rj = num_field
      allocate(trns%f_trns_name(trns%nfield_rtp_2_rj))
!
      end subroutine alloc_fwd_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_data(nnod_rtp, trns)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      allocate(trns%fld_rtp(nnod_rtp,trns%ncomp_rj_2_rtp))
      allocate(trns%frc_rtp(nnod_rtp,trns%ncomp_rtp_2_rj))
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%fld_rtp = 0.0d0
      if(trns%ncomp_rtp_2_rj .gt. 0) trns%frc_rtp = 0.0d0
!
      end subroutine alloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_pole(nnod_pole, trns)
!
      integer(kind = kint), intent(in) :: nnod_pole
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      allocate(trns%fld_pole(nnod_pole,trns%ncomp_rj_2_rtp))
      allocate(trns%flc_pole(nnod_pole,trns%ncomp_rj_2_rtp))
!
      allocate(trns%frc_pole(nnod_pole,trns%ncomp_rtp_2_rj))
!
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%fld_pole = 0.0d0
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%flc_pole = 0.0d0
      if(trns%ncomp_rtp_2_rj .gt. 0) trns%frc_pole = 0.0d0
!
      end subroutine alloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_data(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
      deallocate(trns%fld_rtp, trns%frc_rtp)
!
      end subroutine dealloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_pole(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      deallocate(trns%fld_pole, trns%flc_pole, trns%frc_pole)
!
      end subroutine dealloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_bwd_trns_field_name(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      deallocate(trns%b_trns_name)
!
      end subroutine dealloc_bwd_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_fwd_trns_field_name(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      deallocate(trns%f_trns_name)
!
      end subroutine dealloc_fwd_trns_field_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_trans_flag                                  &
     &         (is_fld, nfield_vec, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, nfield_vec
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if(is_fld .gt. 0) then
        num_trans = num_trans + 1
        itrans = num_trans + 3*nfield_vec
      end if
!
      end subroutine add_scalar_trans_flag
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_trans_flag(is_fld, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if(is_fld .gt. 0) then
        num_trans = num_trans + 1
        itrans = 3*num_trans -  2
      end if
!
      end subroutine add_vector_trans_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scl_trans_flag_snap(is_fld, irtp_fld,              &
     &          nfield_vec, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld, nfield_vec
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
      integer(kind = kint) :: iflag
!
      iflag = is_fld*irtp_fld
      call add_scalar_trans_flag (iflag, nfield_vec, num_trans, itrans)
!
      end subroutine add_scl_trans_flag_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_vec_trans_flag_snap(is_fld, irtp_fld,              &
     &          num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
      integer(kind = kint) :: iflag
!
      iflag = is_fld*irtp_fld
      call add_vector_trans_flag(iflag, num_trans, itrans)
!
      end subroutine add_vec_trans_flag_snap
!
!-----------------------------------------------------------------------
!
      end module t_addresses_sph_transform
