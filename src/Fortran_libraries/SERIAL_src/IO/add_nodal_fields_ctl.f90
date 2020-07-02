!>@file   add_nodal_fields_ctl.f90
!!@brief  module add_nodal_fields_ctl
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Add require field name
!!
!!@verbatim
!!      logical function check_field_list_ctl(field, field_ctl)
!!      subroutine add_phys_name_ctl(field, field_ctl)
!!        type(field_def), intent(in) :: field
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_nodal_fields_ctl
!
      use m_precision
      use m_machine_parameter
      use t_field_labels
      use t_control_array_character3
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      logical function check_field_list_ctl(field, field_ctl)
!
      type(field_def), intent(in) :: field
      type(ctl_array_c3), intent(in) :: field_ctl
!
      integer(kind = kint) :: i
!
!
      check_field_list_ctl = .TRUE.
      do i = 1, field_ctl%num
        if (field_ctl%c1_tbl(i) .eq. field%name) return
      end do
      check_field_list_ctl = .FALSE.
!
!      if(iflag_debug .gt. 0) then
!        write(*,*) trim(field%name), ' is missing in the field list.'
!      end if
!
      end function check_field_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine add_phys_name_ctl(field, field_ctl)
!
      type(field_def), intent(in) :: field
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      type(read_chara3_item) :: field_tmp_ctl
!
!
      if(check_field_list_ctl(field, field_ctl)) return
!
      field_tmp_ctl%iflag = 1
      field_tmp_ctl%charavalue(1) = trim(field%name)
      field_tmp_ctl%charavalue(2) = 'Viz_off'
      field_tmp_ctl%charavalue(3) = 'Monitor_off'
!
      call append_control_array_c3(field_tmp_ctl, field_ctl)
!
      if(iflag_debug .gt. 0) then
        write(*,*) trim(field_ctl%c1_tbl(field_ctl%num) ),              &
     &            ' is added at field ID ',   field_ctl%num
      end if
!
      end subroutine add_phys_name_ctl
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_ctl
