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
!!        type(address_each_sph_trans), intent(inout) :: each_trns
!!      subroutine alloc_nonlinear_data(sph_rtp, trns)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine alloc_nonlinear_pole(sph_rtp, trns)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine dealloc_bwd_trns_field_name(trns)
!!      subroutine dealloc_fwd_trns_field_name(trns)
!!      subroutine dealloc_nonlinear_data(trns)
!!      subroutine dealloc_nonlinear_pole(trns)
!!      subroutine dealloc_nonlinear_zmean(trns)
!!        type(address_4_sph_trans), intent(inout) :: trns
!!
!!      subroutine count_num_fields_each_trans(each_trns,               &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!      subroutine add_field_name_4_sph_trns_snap                       &
!!     &         (field_name, num_component, i_pol, i_tor, irtp,        &
!!     &          i_trns, each_trns)
!!      subroutine add_field_name_4_sph_trns_nofld                      &
!!     &         (field_name, num_component, i_pol, i_tor, irtp,        &
!!     &          i_trns, each_trns)
!!        type(address_each_sph_trans), intent(inout) :: each_trns
!!      subroutine add_field_name_4_sph_trns                            &
!!     &         (iflag_add, field_name, num_component,                 &
!!     &          i_pol, i_tor, irtp, i_trns, each_trns)
!!        type(address_each_sph_trans), intent(inout) :: each_trns
!!@endverbatim
!
      module t_addresses_sph_transform
!
      use m_precision
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_sph_multi_FFTW
!
      implicit none
!
!>      strucutre of spherical transform data addresses
      type address_each_sph_trans
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
      end type address_each_sph_trans
!
!
!>      strucutre of spherical transform data addresses
      type address_4_sph_trans
!>        strucutre of backward spherical transform data addresses
        type(address_each_sph_trans) :: backward
!>        strucutre of forward spherical transform data addresses
        type(address_each_sph_trans) :: forward
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
!
!>        zonal mean of field data in grid space
        real(kind = kreal), allocatable :: fld_zm(:,:)
!
!>        Work area of Fourier transform for MHD
        type(work_for_sgl_FFTW) :: mul_FFTW
      end type address_4_sph_trans
!
      private :: copy_field_name_4_sph_trns
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_field_name(each_trns)
!
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      type(address_each_sph_trans), intent(inout) :: each_trns
!
!
      deallocate(each_trns%fld_pole, each_trns%flc_pole)
!
      end subroutine dealloc_sph_trns_pole_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_data(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call alloc_sph_trns_field_data(sph_rtp, trns%backward)
      call alloc_sph_trns_field_data(sph_rtp, trns%forward)
!
      allocate(trns%fld_zm(sph_rtp%nnod_med,6))
      trns%fld_zm = 0.0d0
!
      end subroutine alloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_pole(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call alloc_sph_trns_pole_data(sph_rtp, trns%backward)
      call alloc_sph_trns_pole_data(sph_rtp, trns%forward)
!
      end subroutine alloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_data(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_field_dats(trns%backward)
      call dealloc_sph_trns_field_dats(trns%forward)
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
      call dealloc_sph_trns_pole_data(trns%backward)
      call dealloc_sph_trns_pole_data(trns%forward)
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
      call dealloc_sph_trns_field_name(trns%backward)
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
      call dealloc_sph_trns_field_name(trns%forward)
!
      end subroutine dealloc_fwd_trns_field_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_fields_each_trans(each_trns,                 &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      subroutine add_field_name_4_sph_trns_snap                         &
     &         (field_name, num_component, i_pol, i_tor, irtp,          &
     &          i_trns, each_trns)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: num_component
      integer(kind = kint), intent(in) :: i_pol, i_tor, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(address_each_sph_trans), intent(inout) :: each_trns
!
      integer(kind = kint)  :: iflag_snap
!
!
      iflag_snap = i_pol * irtp
      call add_field_name_4_sph_trns(iflag_snap, field_name,            &
     &    num_component, i_pol, i_tor, irtp, i_trns, each_trns)
!
      end subroutine add_field_name_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_trns_nofld                        &
     &         (field_name, num_component, i_pol, i_tor, irtp,          &
     &          i_trns, each_trns)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: num_component
      integer(kind = kint), intent(in) :: i_pol, i_tor, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(address_each_sph_trans), intent(inout) :: each_trns
!
!
      call add_field_name_4_sph_trns(i_pol, field_name,                 &
     &    num_component, i_pol, i_tor, irtp, i_trns, each_trns)
!
      end subroutine add_field_name_4_sph_trns_nofld
!
!-----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_trns                              &
     &         (iflag_add, field_name, num_component,                   &
     &          i_pol, i_tor, irtp, i_trns, each_trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: iflag_add, num_component
      integer(kind = kint), intent(in) :: i_pol, i_tor, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(address_each_sph_trans), intent(inout) :: each_trns
!
      type(address_each_sph_trans) :: etrns_tmp
!
!
      if(iflag_add .eq. 0) return
!
      i_trns = each_trns%ncomp + 1
!
      etrns_tmp%nfield = each_trns%nfield
      call alloc_sph_trns_field_name(etrns_tmp)
      call copy_field_name_4_sph_trns                                   &
     &   (etrns_tmp%nfield, each_trns, etrns_tmp)
      call dealloc_sph_trns_field_name(each_trns)
!
      each_trns%ncomp =  each_trns%ncomp + num_component
      each_trns%nfield = each_trns%nfield + 1
      call alloc_sph_trns_field_name(each_trns)
      call copy_field_name_4_sph_trns                                   &
     &   (etrns_tmp%nfield, etrns_tmp, each_trns)
      call dealloc_sph_trns_field_name(etrns_tmp)
!
      each_trns%field_name(each_trns%nfield) = field_name
      each_trns%ifld_trns(each_trns%nfield) = i_trns
      each_trns%ifld_rj(each_trns%nfield) =   i_pol
      each_trns%ifld_rtp(each_trns%nfield) =  irtp
!
!
      if(iflag_debug .eq. 0) return
      write(*,'(i5,a2,a,a2,4i5)') each_trns%nfield, '. ',               &
     &    trim(each_trns%field_name(each_trns%nfield)), ': ',           &
     &    each_trns%ifld_trns(each_trns%nfield),                        &
     &    each_trns%ifld_rj(each_trns%nfield), i_tor,                   &
     &    each_trns%ifld_rtp(each_trns%nfield)
!
      end subroutine add_field_name_4_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_field_name_4_sph_trns                             &
     &         (num_copy, etrns_org, etrns_new)
!
      integer(kind = kint), intent(in) :: num_copy
      type(address_each_sph_trans), intent(in) :: etrns_org
      type(address_each_sph_trans), intent(inout) :: etrns_new
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
