!
!      module add_nodal_fields_ctl
!
!        programmed by H.Matsui on Sep., 2006
!
!!
!!      subroutine add_phys_name_tmp(fld_name)
!!
      module add_nodal_fields_ctl
!
      use m_precision
!
      implicit  none
!
!
      integer (kind = kint) :: num_nod_phys_tmp
      character(len=kchara), allocatable :: phys_nod_name_tmp(:)
      character(len=kchara), allocatable :: visualize_ctl_tmp(:)
      character(len=kchara), allocatable :: monitor_ctl_tmp(:)
!
      private :: visualize_ctl_tmp, monitor_ctl_tmp
      private :: num_nod_phys_tmp, phys_nod_name_tmp
!
      private :: copy_field_ctl_to_tmp, copy_field_ctl_from_tmp
      private :: allocate_work_4_field_ctl, deallocate_work_4_field_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_phys_name_tmp(fld_name)
!
      use m_machine_parameter
      use m_ctl_data_4_fields
!
      character(len=kchara), intent(in) :: fld_name
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
      num_nod_phys_tmp = field_ctl%num
      call allocate_work_4_field_ctl
      call copy_field_ctl_to_tmp
      call deallocate_phys_control
!
      field_ctl%num = field_ctl%num + 1
      call copy_field_ctl_from_tmp
      call deallocate_work_4_field_ctl
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
      end subroutine add_phys_name_tmp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_work_4_field_ctl
!
!
      allocate( phys_nod_name_tmp(num_nod_phys_tmp) )
      allocate( visualize_ctl_tmp(num_nod_phys_tmp) )
      allocate( monitor_ctl_tmp(num_nod_phys_tmp)   )
!
      end subroutine allocate_work_4_field_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_4_field_ctl
!
      deallocate( phys_nod_name_tmp )
      deallocate( visualize_ctl_tmp )
      deallocate( monitor_ctl_tmp   )
!
      end subroutine deallocate_work_4_field_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_field_ctl_to_tmp
!
      use m_ctl_data_4_fields
!
      integer(kind = kint) :: i
!
      do i = 1, num_nod_phys_tmp
        phys_nod_name_tmp(i) =  field_ctl%c1_tbl(i)
        visualize_ctl_tmp(i) =  field_ctl%c2_tbl(i)
        monitor_ctl_tmp(i) =    field_ctl%c3_tbl(i)
      end do
!
      end subroutine copy_field_ctl_to_tmp
!
! -----------------------------------------------------------------------
!
      subroutine copy_field_ctl_from_tmp
!
      use m_ctl_data_4_fields
!
      integer(kind = kint) :: i
!
!
      call alloc_control_array_c3(field_ctl)
!
      do i = 1, num_nod_phys_tmp
        field_ctl%c1_tbl(i) = phys_nod_name_tmp(i)
        field_ctl%c2_tbl(i) =  visualize_ctl_tmp(i)
        field_ctl%c3_tbl(i) =  monitor_ctl_tmp(i)
      end do
!
      end subroutine copy_field_ctl_from_tmp
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_ctl
