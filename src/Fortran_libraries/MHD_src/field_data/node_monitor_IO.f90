!
!      module node_monitor_IO
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2007
!
!!      subroutine set_local_nod_4_monitor(mesh, group)
!!      subroutine output_monitor_control                               &
!!     &         (istep, point_step, time_d, mesh, nod_fld)
!!
!!      subroutine allocate_monitor_group
!!      subroutine allocate_monitor_local
!!      subroutine deallocate_monitor_local
!!
!!      subroutine open_node_monitor_file(my_rank, nod_fld)
!!      subroutine set_local_node_id_4_monitor(node, nod_grp)
!!        type(IO_step_param), intent(in) :: point_step
!!        type(time_data), intent(in) :: time_d
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine skip_monitor_data(i_step_init)
!
      module node_monitor_IO
!
      use m_precision
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_IO_step_parameter
!
      implicit none
!
      integer(kind=kint), parameter :: id_monitor_file = 47
      character(len=kchara), parameter :: node_monitor_head = 'node'
!
      integer (kind=kint) :: num_monitor
      character (len=kchara), allocatable :: monitor_grp(:)
!
      integer (kind=kint) :: num_monitor_local
      integer (kind=kint), allocatable :: monitor_local(:)
!
      integer (kind = kint) :: num_field_monitor
      integer (kind = kint) :: ntot_comp_monitor
      integer (kind = kint), allocatable :: num_comp_phys_monitor(:)
      character (len = kchara), allocatable :: phys_name_monitor(:)
!
      private :: id_monitor_file, node_monitor_head
      private :: num_monitor_local, monitor_local
      private :: num_comp_phys_monitor, phys_name_monitor
      private :: allocate_monitor_local
      private :: open_node_monitor_file, output_nodal_monitor_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_nod_4_monitor(mesh, group)
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
!
!
      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
      end subroutine set_local_nod_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine output_monitor_control                                 &
     &         (istep, point_step, time_d, mesh, nod_fld)
!
      use calypso_mpi
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: istep
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: point_step
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
!
!
      if(output_IO_flag(istep,point_step) .gt. 0) return
      if(num_monitor .eq. 0 .or. num_monitor_local .eq. 0) return
      if(iflag_debug.eq.1) write(*,*) 'output_nodal_monitor_data'
!
      call output_nodal_monitor_data(time_d, mesh%node, nod_fld)
!
      end subroutine output_monitor_control
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_monitor_group
!
!
      allocate( monitor_grp(num_monitor) )
!
      end subroutine allocate_monitor_group
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_monitor_local
!
!
      allocate( monitor_local(num_monitor_local) )
      if(num_monitor_local .gt. 0) monitor_local = 0
!
      end subroutine allocate_monitor_local
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_monitor_local
!
!
      deallocate(monitor_grp, monitor_local)
!
      end subroutine deallocate_monitor_local
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine open_node_monitor_file(my_rank, nod_fld)
!
      use set_parallel_file_name
!
      integer (kind=kint), intent(in) :: my_rank
      type(phys_data), intent(in) :: nod_fld
!
      character(len=kchara) :: fname_tmp, file_name
      integer (kind=kint) :: i, j
!
!
      call add_int_suffix(my_rank, node_monitor_head, fname_tmp)
      call add_dat_extension(fname_tmp, file_name)
      open (id_monitor_file,file=file_name, status='old',               &
     &    position='append', err = 99)
      return
!
  99  continue
      open(id_monitor_file, file=file_name, status='replace')
!
      allocate(num_comp_phys_monitor(num_field_monitor))
      allocate(phys_name_monitor(num_field_monitor))
!
      j = 0
      do i = 1, nod_fld%num_phys
        if (nod_fld%iflag_monitor(i) .eq. 1 ) then
          j = j + 1
          num_comp_phys_monitor(j) = nod_fld%num_component(i)
          phys_name_monitor(j) =     nod_fld%phys_name(i)
        end if
      end do
!
        write(id_monitor_file,'(a)') num_monitor_local
        write(id_monitor_file,'(a)') 'ID step time x y z '
        write(id_monitor_file,1001)  num_field_monitor
        write(id_monitor_file,1002)                                     &
     &        nod_fld%num_component(1:num_field_monitor)
 1001   format('number_of_fields: ',i16)
 1002   format('number_of_components: ',200i3)
!
        do i = 1, num_field_monitor
          write(id_monitor_file,*) trim(phys_name_monitor(i))
        end do
!
      deallocate(num_comp_phys_monitor)
      deallocate(phys_name_monitor)
!
      end subroutine open_node_monitor_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_local_node_id_4_monitor(node, nod_grp)
!
      use calypso_mpi
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: i, k, inum
!
!
!  count number of local nodes for monitor
!
      num_monitor_local = 0
!
      if ( num_monitor .gt. 0 ) then
        do i = 1, nod_grp%num_grp
!
          do inum = 1, num_monitor
            if (nod_grp%grp_name(i) .eq. monitor_grp(inum)) then
               num_monitor_local = num_monitor_local                    &
     &                            + nod_grp%istack_grp(i)               &
     &                            - nod_grp%istack_grp(i-1)
               exit
            end if
          end do
        end do
      end if
!
!   allocate local node ID
      call allocate_monitor_local
!
      if (num_monitor_local .eq. 0) return
!
      num_monitor_local = 0
      do i=1, nod_grp%num_grp
        do inum = 1, num_monitor
          if (nod_grp%grp_name(i) .eq. monitor_grp(inum)) then
            do k= nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
              if( nod_grp%item_grp(k) .le. node%internal_node ) then 
                num_monitor_local = num_monitor_local + 1
                monitor_local(num_monitor_local) = nod_grp%item_grp(k)
              end if
            end do
            exit
          end if
        end do
      end do
!
      end subroutine set_local_node_id_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_nodal_monitor_data(time_d, node, nod_fld)
!
      use calypso_mpi
      use m_machine_parameter
!
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
      integer (kind = kint) :: i, inod, i_fld, ist, ied
!
!
      call open_node_monitor_file(my_rank, nod_fld)
!
      do i = 1, num_monitor_local
        inod = monitor_local(i)
        write(id_monitor_file,'(2i16,1pe25.15e3)',                      &
     &             advance='NO') time_d%i_time_step, inod, time_d%time
        write(id_monitor_file,'(1p3e25.15e3)',                          &
     &             advance='NO') node%xx(inod,1:3)
        do i_fld = 1, nod_fld%num_phys
          if(nod_fld%iflag_monitor(i_fld) .gt. 0) then
            ist = nod_fld%istack_component(i_fld-1) + 1
            ied = nod_fld%istack_component(i_fld)
            write(id_monitor_file,'(1p6E25.15e3)',                      &
     &             advance='NO')  nod_fld%d_fld(inod,ist:ied)
          end if
        end do
        write(id_monitor_file,'(a)') ''
      end do
!
      close(id_monitor_file)
!
      end subroutine output_nodal_monitor_data
!
!  ---------------------------------------------------------------------
!
      subroutine skip_monitor_data(i_step_init)
!
      integer(kind=kint), intent(in) :: i_step_init
!
      integer (kind = kint) :: i, k, itmp
      integer (kind = kint) :: i_read_step
      real(kind = kreal) :: rtmp
!
!
      if (num_monitor .ne. 0 .and. num_monitor_local .ne. 0) then
        do
          do i = 1, num_monitor_local
           read(id_monitor_file,*) i_read_step, itmp, rtmp,       &
     &         (rtmp,k=1,3), (rtmp,k=1,ntot_comp_monitor)
          end do
          if (i_read_step.ge.i_step_init) exit
        end do
      end if
!
      end subroutine skip_monitor_data
!
!  ---------------------------------------------------------------------
!
      end module node_monitor_IO
