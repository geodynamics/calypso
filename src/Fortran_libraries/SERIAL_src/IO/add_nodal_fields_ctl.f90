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
      do i = 1, num_nod_phys_ctl
        if (phys_nod_name_ctl(i) .eq. fld_name) then
          iflag = 1
          exit
        end if
      end do
!
      if (iflag .gt. 0) return
!
      num_nod_phys_tmp = num_nod_phys_ctl
      call allocate_work_4_field_ctl
      call copy_field_ctl_to_tmp
      call deallocate_phys_control
!
      num_nod_phys_ctl = num_nod_phys_ctl + 1
      call allocate_phys_control
      call copy_field_ctl_from_tmp
      call deallocate_work_4_field_ctl
!
      phys_nod_name_ctl(num_nod_phys_ctl) = fld_name
      visualize_ctl(num_nod_phys_ctl) =    'Viz_off'
      monitor_ctl(num_nod_phys_ctl) =      'Monitor_off'
!
      if(iflag_debug .eq. iflag_full_msg) then
        write(*,*) trim(phys_nod_name_ctl(num_nod_phys_ctl) ),          &
     &            ' is added at field ID ',   num_nod_phys_ctl
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
        phys_nod_name_tmp(i) =  phys_nod_name_ctl(i)
        visualize_ctl_tmp(i) =  visualize_ctl(i)
        monitor_ctl_tmp(i) =    monitor_ctl(i)
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
      do i = 1, num_nod_phys_tmp
        phys_nod_name_ctl(i) = phys_nod_name_tmp(i)
        visualize_ctl(i) =     visualize_ctl_tmp(i)
        monitor_ctl(i) =       monitor_ctl_tmp(i)
      end do
!
      end subroutine copy_field_ctl_from_tmp
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_ctl
