!>@file   t_node_monitor_IO.f90
!!@brief  module t_node_monitor_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Aug., 2007
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_node_grp_monitor(nmtr_ctl, nod_mntr)
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!      subroutine count_field_4_monitor(fld, nod_mntr)
!!        type(phys_data), intent(in) :: fld
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!      subroutine set_local_nod_4_monitor(mesh, group, nod_mntr)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!      subroutine output_monitor_control(istep, point_step, time_d,    &
!!     &                                  mesh, nod_fld, nod_mntr)
!!        integer(kind = kint), intent(in) :: istep
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(in) :: point_step
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(node_monitor_IO), intent(in) :: nod_mntr
!!
!!      subroutine alloc_monitor_group(nod_mntr)
!!      subroutine dealloc_monitor_local(nod_mntr)
!!
!!      subroutine skip_monitor_data(i_step_init, nod_mntr)
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!@endverbatim
!
      module t_node_monitor_IO
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
!
      type node_monitor_IO
        character(len=kchara) :: node_monitor_head = 'node'
!
        integer (kind=kint) :: num_monitor
        character (len=kchara), allocatable :: monitor_grp(:)
!
        integer (kind=kint) :: num_monitor_local
        integer (kind=kint), allocatable :: monitor_local(:)
!
        integer (kind = kint) :: num_field_monitor
        integer (kind = kint) :: ntot_comp_monitor
      end type node_monitor_IO
!
      private :: id_monitor_file
      private :: alloc_monitor_local
      private :: open_node_monitor_file, output_nodal_monitor_data
      private :: set_local_node_id_4_monitor
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_node_grp_monitor(nmtr_ctl, nod_mntr)
!
      use t_ctl_data_node_monitor
!
      type(node_monitor_control), intent(in) :: nmtr_ctl
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      integer(kind = kint) :: i
!
      nod_mntr%num_monitor = 0
      if (nmtr_ctl%group_4_monitor_ctl%icou .gt. 0) then
        nod_mntr%num_monitor = nmtr_ctl%group_4_monitor_ctl%num
      end if
!
      call alloc_monitor_group(nod_mntr)
      if(nod_mntr%num_monitor .le. 0) return
!
      nod_mntr%monitor_grp(1:nod_mntr%num_monitor)                      &
     &     = nmtr_ctl%group_4_monitor_ctl%c_tbl(1:nod_mntr%num_monitor)
!
      if(iflag_debug .eq. 0) return
      do i = 1, nod_mntr%num_monitor
        write(*,*) 'monitor_grp',i, nod_mntr%monitor_grp(i)
      end do
!
      end subroutine set_control_node_grp_monitor
!
!  ---------------------------------------------------------------------
!
      subroutine count_field_4_monitor(fld, nod_mntr)
!
      type(phys_data), intent(in) :: fld
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      integer(kind = kint) :: i
!
!    count number of components for monitoring
!
      nod_mntr%num_field_monitor = 0
      nod_mntr%ntot_comp_monitor = 0
      do i = 1, fld%num_phys
        if(fld%flag_monitor(i)) then
          nod_mntr%num_field_monitor = nod_mntr%num_field_monitor + 1
          nod_mntr%ntot_comp_monitor = nod_mntr%ntot_comp_monitor       &
     &                                + fld%num_component(i)
        end if
      end do
!
      end subroutine count_field_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine set_local_nod_4_monitor(mesh, group, nod_mntr)
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
!
      call set_local_node_id_4_monitor(mesh%node, group%nod_grp,        &
     &                                 nod_mntr)
!
      end subroutine set_local_nod_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine output_monitor_control(istep, point_step, time_d,      &
     &                                  mesh, nod_fld, nod_mntr)
!
      use calypso_mpi
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: istep
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: point_step
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(node_monitor_IO), intent(in) :: nod_mntr
!
!
!
      if(output_IO_flag(istep,point_step) .eqv. .FALSE.) return
      if(nod_mntr%num_monitor .eq. 0                                    &
     &      .or. nod_mntr%num_monitor_local .eq. 0) return
      if(iflag_debug.eq.1) write(*,*) 'output_nodal_monitor_data'
!
      call output_nodal_monitor_data(time_d, mesh%node,                 &
     &                               nod_fld, nod_mntr)
!
      end subroutine output_monitor_control
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_monitor_group(nod_mntr)
!
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      allocate(nod_mntr%monitor_grp(nod_mntr%num_monitor))
!
      end subroutine alloc_monitor_group
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_monitor_local(nod_mntr)
!
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      allocate( nod_mntr%monitor_local(nod_mntr%num_monitor_local) )
      if(nod_mntr%num_monitor_local .gt. 0) nod_mntr%monitor_local = 0
!
      end subroutine alloc_monitor_local
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_monitor_local(nod_mntr)
!
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      deallocate(nod_mntr%monitor_grp, nod_mntr%monitor_local)
!
      end subroutine dealloc_monitor_local
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine open_node_monitor_file(id_rank, nod_fld, nod_mntr)
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      type(phys_data), intent(in) :: nod_fld
      type(node_monitor_IO), intent(in) :: nod_mntr
!
      integer (kind = kint), allocatable :: num_comp_phys_monitor(:)
      character (len = kchara), allocatable :: phys_name_monitor(:)
!
      character(len=kchara) :: fname_tmp, file_name
      integer (kind=kint) :: i, j
!
!
      fname_tmp = add_process_id(id_rank, nod_mntr%node_monitor_head)
      file_name = add_dat_extension(fname_tmp)
      open (id_monitor_file,file=file_name, status='old',               &
     &    position='append', err = 99)
      return
!
  99  continue
      open(id_monitor_file, file=file_name, status='replace')
!
      allocate(num_comp_phys_monitor(nod_mntr%num_field_monitor))
      allocate(phys_name_monitor(nod_mntr%num_field_monitor))
!
      j = 0
      do i = 1, nod_fld%num_phys
        if(nod_fld%flag_monitor(i)) then
          j = j + 1
          num_comp_phys_monitor(j) = nod_fld%num_component(i)
          phys_name_monitor(j) =     nod_fld%phys_name(i)
        end if
      end do
!
        write(id_monitor_file,'(a)') nod_mntr%num_monitor_local
        write(id_monitor_file,'(a)') 'ID step time x y z '
        write(id_monitor_file,1001)  nod_mntr%num_field_monitor
        write(id_monitor_file,1002)                                     &
     &        nod_fld%num_component(1:nod_mntr%num_field_monitor)
 1001   format('number_of_fields: ',i16)
 1002   format('number_of_components: ',200i3)
!
        do i = 1, nod_mntr%num_field_monitor
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
      subroutine set_local_node_id_4_monitor(node, nod_grp, nod_mntr)
!
      use calypso_mpi
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      integer (kind = kint) :: i, k, inum
!
!
!  count number of local nodes for monitor
!
      nod_mntr%num_monitor_local = 0
!
      if ( nod_mntr%num_monitor .gt. 0 ) then
        do i = 1, nod_grp%num_grp
!
          do inum = 1, nod_mntr%num_monitor
            if(nod_grp%grp_name(i)                                      &
     &            .eq. nod_mntr%monitor_grp(inum)) then
               nod_mntr%num_monitor_local = nod_mntr%num_monitor_local  &
     &                                     + nod_grp%istack_grp(i)      &
     &                                      - nod_grp%istack_grp(i-1)
               exit
            end if
          end do
        end do
      end if
!
!   allocate local node ID
      call alloc_monitor_local(nod_mntr)
!
      if (nod_mntr%num_monitor_local .eq. 0) return
!
      nod_mntr%num_monitor_local = 0
      do i=1, nod_grp%num_grp
        do inum = 1, nod_mntr%num_monitor
          if (nod_grp%grp_name(i) .eq. nod_mntr%monitor_grp(inum)) then
            do k= nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
              if( nod_grp%item_grp(k) .le. node%internal_node ) then 
                nod_mntr%num_monitor_local                              &
     &                            = nod_mntr%num_monitor_local + 1
                nod_mntr%monitor_local(nod_mntr%num_monitor_local)      &
     &                            = nod_grp%item_grp(k)
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
      subroutine output_nodal_monitor_data(time_d, node,                &
     &                                     nod_fld, nod_mntr)
!
      use calypso_mpi
      use m_machine_parameter
!
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(node_monitor_IO), intent(in) :: nod_mntr
!
      integer (kind = kint) :: i, inod, i_fld, ist, ied
!
!
      call open_node_monitor_file(my_rank, nod_fld, nod_mntr)
!
      do i = 1, nod_mntr%num_monitor_local
        inod = nod_mntr%monitor_local(i)
        write(id_monitor_file,'(2i16,1pe25.15e3)',                      &
     &             advance='NO') time_d%i_time_step, inod, time_d%time
        write(id_monitor_file,'(1p3e25.15e3)',                          &
     &             advance='NO') node%xx(inod,1:3)
        do i_fld = 1, nod_fld%num_phys
          if(nod_fld%flag_monitor(i_fld)) then
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
      subroutine skip_monitor_data(i_step_init, nod_mntr)
!
      integer(kind=kint), intent(in) :: i_step_init
      type(node_monitor_IO), intent(in) :: nod_mntr
!
      integer (kind = kint) :: i, k, itmp
      integer (kind = kint) :: i_read_step
      real(kind = kreal) :: rtmp
!
!
      if (nod_mntr%num_monitor .ne. 0                                   &
     &       .and. nod_mntr%num_monitor_local .ne. 0) then
        do
          do i = 1, nod_mntr%num_monitor_local
           read(id_monitor_file,*) i_read_step, itmp, rtmp,             &
     &         (rtmp,k=1,3), (rtmp,k=1,nod_mntr%ntot_comp_monitor)
          end do
          if (i_read_step.ge.i_step_init) exit
        end do
      end if
!
      end subroutine skip_monitor_data
!
!  ---------------------------------------------------------------------
!
      end module t_node_monitor_IO
