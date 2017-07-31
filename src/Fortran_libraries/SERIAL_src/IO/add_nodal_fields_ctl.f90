!>@file   add_nodal_fields_ctl.f90
!!@brief  module add_nodal_fields_ctl
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Add require field name
!!
!!@verbatim
!!      subroutine add_phys_name_ctl(fld_name, field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_nodal_fields_ctl
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
      type(ctl_array_c3), private :: field_tmp_ctl
!
      private :: copy_field_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_phys_name_ctl(fld_name, field_ctl)
!
      use m_machine_parameter
!
      character(len=kchara), intent(in) :: fld_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i, iflag
!
!
      iflag = 0
      do i = 1, field_ctl%num
        if (field_ctl%c1_tbl(i) .eq. fld_name) then
          iflag = 1
          exit
        end if
      end do
!
      if (iflag .gt. 0) return
!
      field_tmp_ctl%num = field_ctl%num
      call alloc_control_array_c3(field_tmp_ctl)
      call copy_field_ctl                                               &
     &   (field_tmp_ctl%num, field_ctl, field_tmp_ctl)
      call dealloc_control_array_c3(field_ctl)
!
      field_ctl%num = field_ctl%num + 1
      call alloc_control_array_c3(field_ctl)
      call copy_field_ctl                                               &
     &   (field_tmp_ctl%num, field_tmp_ctl, field_ctl)
      call dealloc_control_array_c3(field_tmp_ctl)
!
      field_ctl%c1_tbl(field_ctl%num) = fld_name
      field_ctl%c2_tbl(field_ctl%num) = 'Viz_off'
      field_ctl%c3_tbl(field_ctl%num) = 'Monitor_off'
!
      if(iflag_debug .eq. iflag_full_msg) then
        write(*,*) trim(field_ctl%c1_tbl(field_ctl%num) ),              &
     &            ' is added at field ID ',   field_ctl%num
      end if
!
      end subroutine add_phys_name_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_field_ctl(num_copy, org_field_ctl, tgt_field_ctl)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c3), intent(in) ::    org_field_ctl
      type(ctl_array_c3), intent(inout) :: tgt_field_ctl
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_copy
        tgt_field_ctl%c1_tbl(i) = org_field_ctl%c1_tbl(i)
        tgt_field_ctl%c2_tbl(i) = org_field_ctl%c2_tbl(i)
        tgt_field_ctl%c3_tbl(i) = org_field_ctl%c3_tbl(i)
      end do
!
      end subroutine copy_field_ctl
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_ctl
