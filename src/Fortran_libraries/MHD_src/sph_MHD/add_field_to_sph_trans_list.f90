!>@file   add_field_to_sph_trans_list.f90
!!@brief  module add_field_to_sph_trans_list
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_field_4_sph_trns_by_pol                          &
!!     &         (field, i_pol, irtp, i_trns, each_trns)
!!        type(field_def), intent(in) :: field
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!      subroutine add_field_name_4_sph_trns_snap                       &
!!     &         (field, i_pol, irtp, i_trns, each_trns)
!!        type(field_def), intent(in) :: field
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!      subroutine add_scalar_4_sph_trns_by_pol                         &
!!     &         (field, i_pol, irtp, i_trns, each_trns)
!!        type(field_def), intent(in) :: field
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!      subroutine add_scalar_4_sph_trns_snap                           &
!!     &         (field, i_pol, irtp, i_trns, each_trns)
!!        type(field_def), intent(in) :: field
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!
!!      subroutine add_field_name_4_sph_trns                            &
!!     &         (iflag_add, field_name, num_component,                 &
!!     &          i_pol, irtp, i_trns, each_trns)
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!@endverbatim
!
      module add_field_to_sph_trans_list
!
      use m_precision
!
      use t_field_labels
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_field_4_sph_trns_by_pol                            &
     &         (field, i_pol, irtp, i_trns, each_trns)
!
      type(field_def), intent(in) :: field
      integer(kind = kint), intent(in) :: i_pol, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      call add_field_name_4_sph_trns(i_pol, field%name, field%n_comp,   &
     &    i_pol, irtp, i_trns, each_trns)
!
      end subroutine add_field_4_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_trns_snap                         &
     &         (field, i_pol, irtp, i_trns, each_trns)
!
      type(field_def), intent(in) :: field
      integer(kind = kint), intent(in) :: i_pol, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(spherical_transform_data), intent(inout) :: each_trns
!
      integer(kind = kint)  :: iflag_snap
!
!
      iflag_snap = i_pol * irtp
      call add_field_name_4_sph_trns(iflag_snap,                        &
     &    field%name, field%n_comp, i_pol, irtp, i_trns, each_trns)
!
      end subroutine add_field_name_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_4_sph_trns_by_pol                           &
     &         (field, i_pol, irtp, i_trns, each_trns)
!
      type(field_def), intent(in) :: field
      integer(kind = kint), intent(in) :: i_pol, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      call add_field_name_4_sph_trns(i_pol, field%name, n_scalar,       &
     &    i_pol, irtp, i_trns, each_trns)
!
      end subroutine add_scalar_4_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_scalar_4_sph_trns_snap                             &
     &         (field, i_pol, irtp, i_trns, each_trns)
!
      type(field_def), intent(in) :: field
      integer(kind = kint), intent(in) :: i_pol, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(spherical_transform_data), intent(inout) :: each_trns
!
      integer(kind = kint)  :: iflag_snap
!
!
      iflag_snap = i_pol * irtp
      call add_field_name_4_sph_trns(iflag_snap, field%name,            &
     &    n_scalar, i_pol, irtp, i_trns, each_trns)
!
      end subroutine add_scalar_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_trns                              &
     &         (iflag_add, field_name, num_component,                   &
     &          i_pol, irtp, i_trns, each_trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: iflag_add, num_component
      integer(kind = kint), intent(in) :: i_pol, irtp
!
      integer(kind = kint), intent(inout) :: i_trns
      type(spherical_transform_data), intent(inout) :: each_trns
!
      type(spherical_transform_data) :: etrns_tmp
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
     &    each_trns%ifld_rj(each_trns%nfield),                          &
     &    each_trns%ifld_rtp(each_trns%nfield)
!
      end subroutine add_field_name_4_sph_trns
!
!-----------------------------------------------------------------------
!
      end module add_field_to_sph_trans_list
