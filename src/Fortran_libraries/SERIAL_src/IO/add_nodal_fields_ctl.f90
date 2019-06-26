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
      use t_control_elements
      use t_control_array_character3
!
      implicit  none
!
      type(read_chara3_item), private :: field_tmp_ctl
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
      if(iflag .gt. 0) return
!
      field_tmp_ctl%iflag = 1
      field_tmp_ctl%charavalue(1) = fld_name
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
