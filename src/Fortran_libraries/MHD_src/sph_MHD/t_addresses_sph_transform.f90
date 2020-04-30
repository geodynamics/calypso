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
!!      subroutine alloc_sph_trns_field_name(each_trns)
!!      subroutine alloc_sph_trns_field_data(sph_rtp, each_trns)
!!      subroutine alloc_sph_trns_pole_data(sph_rtp, each_trns)
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!
!!      subroutine dealloc_sph_trns_field_name(each_trns)
!!      subroutine dealloc_sph_trns_field_dats(each_trns)
!!      subroutine dealloc_sph_trns_pole_data(each_trns)
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!
!!      subroutine count_num_fields_each_trans(each_trns,               &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!      subroutine copy_field_name_4_sph_trns                           &
!!     &         (num_copy, etrns_org, etrns_new)
!!        type(spherical_transform_data), intent(in) :: etrns_org
!!        type(spherical_transform_data), intent(inout) :: etrns_new
!!@endverbatim
!
      module t_addresses_sph_transform
!
      use m_precision
!
      use t_spheric_rtp_data
!
      implicit none
!
!>      strucutre of spherical transform data addresses
      type spherical_transform_data
!>        number of fields for spherical harmonics transform
        integer(kind = kint) :: nfield = 0
!>        number of components for spherical harmonics transform
        integer(kind = kint) :: ncomp =  0
!>        number of vector for spherical harmonics transform
        integer(kind = kint) :: num_vector = 0
!>        number of scalars for spherical harmonics transform
        integer(kind = kint) :: num_scalar = 0
!>        number of tensors for spherical harmonics transform
        integer(kind = kint) :: num_tensor = 0
!
!>        Field name for spherical transform
        character(len = kchara), allocatable :: field_name(:)
!>        address of spherical transform array
        integer(kind = kint), allocatable :: ifld_trns(:)
!>        address of backward transform for sprctr
        integer(kind = kint), allocatable :: ifld_rj(:)
!>        address of backward transform for nodal field
        integer(kind = kint), allocatable :: ifld_rtp(:)
!
!>        field data in grid space
        real(kind = kreal), allocatable :: fld_rtp(:,:)
!
!>        field data at pole
        real(kind = kreal), allocatable :: fld_pole(:,:)
!>        local field data at pole
        real(kind = kreal), allocatable :: flc_pole(:,:)
      end type spherical_transform_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_field_name(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      allocate(each_trns%field_name(each_trns%nfield))
      allocate(each_trns%ifld_trns(each_trns%nfield))
      allocate(each_trns%ifld_rj(each_trns%nfield))
      allocate(each_trns%ifld_rtp(each_trns%nfield))
!
      if(each_trns%nfield .le. 0) return
      each_trns%ifld_trns = 0
      each_trns%ifld_rj =   0
      each_trns%ifld_rtp =  0
!
      end subroutine alloc_sph_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_field_data(sph_rtp, each_trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      allocate(each_trns%fld_rtp(sph_rtp%nnod_rtp,each_trns%ncomp))
      if(each_trns%ncomp .gt. 0) each_trns%fld_rtp = 0.0d0
!
      end subroutine alloc_sph_trns_field_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_pole_data(sph_rtp, each_trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      allocate(each_trns%fld_pole(sph_rtp%nnod_pole,each_trns%ncomp))
      allocate(each_trns%flc_pole(sph_rtp%nnod_pole,each_trns%ncomp))
!
      if(each_trns%ncomp*sph_rtp%nnod_pole .gt. 0) then
        each_trns%fld_pole = 0.0d0
        each_trns%flc_pole = 0.0d0
      end if
!
      end subroutine alloc_sph_trns_pole_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trns_field_name(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      deallocate(each_trns%field_name, each_trns%ifld_trns)
      deallocate(each_trns%ifld_rj, each_trns%ifld_rtp)
!
      end subroutine dealloc_sph_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trns_field_dats(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      deallocate(each_trns%fld_rtp)
!
      end subroutine dealloc_sph_trns_field_dats
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trns_pole_data(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      deallocate(each_trns%fld_pole, each_trns%flc_pole)
!
      end subroutine dealloc_sph_trns_pole_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_fields_each_trans(each_trns,                 &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(spherical_transform_data), intent(inout) :: each_trns
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rj_2_rtp
!
!
      nscltsr_rj_2_rtp = each_trns%num_scalar + 6*each_trns%num_tensor
!
      ncomp_sph_trans =   max(ncomp_sph_trans, each_trns%ncomp)
      nvector_sph_trans = max(nvector_sph_trans, each_trns%num_vector)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
!
      end subroutine count_num_fields_each_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_field_name_4_sph_trns                             &
     &         (num_copy, etrns_org, etrns_new)
!
      integer(kind = kint), intent(in) :: num_copy
      type(spherical_transform_data), intent(in) :: etrns_org
      type(spherical_transform_data), intent(inout) :: etrns_new
!
!
      if(num_copy .le. 0) return
      etrns_new%field_name(1:num_copy)                                  &
     &            = etrns_org%field_name(1:num_copy) 
      etrns_new%ifld_trns(1:num_copy) = etrns_org%ifld_trns(1:num_copy)
      etrns_new%ifld_rj(1:num_copy) =   etrns_org%ifld_rj(1:num_copy)
      etrns_new%ifld_rtp(1:num_copy) =  etrns_org%ifld_rtp(1:num_copy)
!
      end subroutine copy_field_name_4_sph_trns
!
!-----------------------------------------------------------------------
!
      end module t_addresses_sph_transform
