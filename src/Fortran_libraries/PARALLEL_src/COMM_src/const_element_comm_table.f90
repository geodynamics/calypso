!>@file   const_element_comm_table.f90
!!@brief  module const_element_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine const_comm_table_by_connenct                         &
!!     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,    &
!!     &          node, nod_comm, neib_e, host, e_comm)
!!        type(node_data), intent(in) :: node
!!        type(element_around_node), intent(in) :: host
!!        type(element_around_node), intent(in) :: neib_e
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: e_comm
!!@endverbatim
!!
      module const_element_comm_table
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_solver_SR
!
      use t_geometry_data
      use t_comm_table
      use t_belonged_element_4_node
!
      implicit none
!
      type work_4_ele_comm_table
!>        global node ID for element import table
        integer(kind = kint_gl), allocatable :: inod_import_e(:)
!>        global node ID for element export table
        integer(kind = kint_gl), allocatable :: inod_export_e(:)
!>        local node ID for element export table
        integer(kind = kint), allocatable :: inod_import_l(:)
!>        local node ID for element export table
        integer(kind = kint), allocatable :: inod_export_l(:)
!
!>        local node ID for import table
        integer(kind = kint), allocatable :: item_local(:)
!>        local node ID for import table
        integer(kind = kint), allocatable :: inod_local(:)
!
!>        element position for element import table
        real(kind = kreal), allocatable :: xe_import(:)
!>        element position for element import table
        real(kind = kreal), allocatable :: xe_export(:)
      end type work_4_ele_comm_table
!
!>      small number
      real(kind = kreal) :: tiny = 1.0d-11
!
      private :: alloc_element_rev_imports
      private :: alloc_element_rev_exports
      private :: dealloc_element_rev_imports
      private :: dealloc_element_rev_exports
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct                           &
     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,      &
     &          node, nod_comm, neib_e, host, e_comm)
!
      use find_element_comm_table
      use const_global_element_ids
      use make_element_comm_table_SR
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: host
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: e_comm
!
      type(work_4_ele_comm_table) :: wk_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call allocate_type_neib_id(e_comm)
      call allocate_type_import_num(e_comm)
!
!      write(*,*) 'count_element_import_num', my_rank
      call count_element_import_num(node%numnod, host%istack_4_node,    &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!      call calypso_mpi_barrier
!
      call alloc_element_rev_imports(node%numnod,                       &
     &    nod_comm%ntot_export, e_comm%ntot_import, wk_comm)
      call allocate_type_import_item(e_comm)
!
!      write(*,*) 'local_node_id_reverse_SR', my_rank
      call local_node_id_reverse_SR                                     &
     &   (node%numnod, nod_comm%num_neib, nod_comm%id_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    wk_comm%item_local, wk_comm%inod_local)
!      call calypso_mpi_barrier
!
!      write(*,*) 'set_element_import_item', my_rank
      call set_element_import_item(node%numnod, node%internal_node,     &
     &    numele, nnod_4_ele, ie, node%inod_global, x_ele,              &
     &    host%istack_4_node, host%iele_4_node, wk_comm%inod_local,     &
     &    nod_comm%num_neib, nod_comm%istack_import,                    &
     &    nod_comm%item_import, e_comm%num_neib, e_comm%istack_import,  &
     &    e_comm%item_import, wk_comm%inod_import_e,                    &
     &    wk_comm%inod_import_l, wk_comm%xe_import)
!      call calypso_mpi_barrier
!
      call allocate_type_export_num(e_comm)
!
!      write(*,*) 'element_num_reverse_SR', my_rank
      call element_num_reverse_SR(e_comm%num_neib, e_comm%id_neib,      &
     &    e_comm%num_import, e_comm%num_export, e_comm%istack_export,   &
     &    e_comm%ntot_export)
!      call calypso_mpi_barrier
!
      call alloc_element_rev_exports(e_comm%ntot_export, wk_comm)
      call allocate_type_export_item(e_comm)
!
!      write(*,*) 'element_position_reverse_SR', my_rank
      call element_position_reverse_SR(e_comm%num_neib, e_comm%id_neib, &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import, wk_comm%inod_export_e,                     &
     &    wk_comm%inod_export_l, wk_comm%xe_export)
!      call calypso_mpi_barrier
!
!      write(*,*) 'set_element_export_item', my_rank
      call set_element_export_item(txt, node%numnod, numele,            &
     &    node%inod_global, internal_flag, x_ele, neib_e%istack_4_node, &
     &    neib_e%iele_4_node, nod_comm%num_neib,                        &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_e, wk_comm%inod_export_l,                 &
     &    wk_comm%xe_export, e_comm%item_export)
!      call calypso_mpi_barrier
!
      call dealloc_element_rev_exports(wk_comm)
      call dealloc_element_rev_imports(wk_comm)
!
!      write(*,*) 'check_element_position', my_rank
      call check_element_position(txt, numele, x_ele, e_comm)
!      call calypso_mpi_barrier
!
      end subroutine const_comm_table_by_connenct
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_element_rev_imports                              &
     &         (numnod, ntot_export, ntot_import_e, wk_comm)
!
      integer(kind = kint), intent(in) :: numnod, ntot_export
      integer(kind = kint), intent(in) :: ntot_import_e
      type(work_4_ele_comm_table), intent(inout) :: wk_comm
!
!
      allocate(wk_comm%inod_local(numnod))
      allocate(wk_comm%item_local(ntot_export))
!
      allocate(wk_comm%inod_import_e(ntot_import_e))
      allocate(wk_comm%inod_import_l(ntot_import_e))
      allocate(wk_comm%xe_import(3*ntot_import_e))
!
      if(numnod .gt. 0) wk_comm%inod_local = 0
      if(ntot_export .gt. 0) wk_comm%item_local = 0
      if(ntot_import_e .gt. 0) wk_comm%inod_import_e = 0
      if(ntot_import_e .gt. 0) wk_comm%xe_import = 0.0d0
!
      end subroutine alloc_element_rev_imports
!
!------------------------------------------------------------------
!
      subroutine alloc_element_rev_exports(ntot_export_e, wk_comm)
!
      integer(kind = kint), intent(in) :: ntot_export_e
      type(work_4_ele_comm_table), intent(inout) :: wk_comm
!
!
      allocate(wk_comm%inod_export_e(ntot_export_e))
      allocate(wk_comm%inod_export_l(ntot_export_e))
      allocate(wk_comm%xe_export(3*ntot_export_e))
      if(ntot_export_e .gt. 0) wk_comm%inod_export_e = 0
      if(ntot_export_e .gt. 0) wk_comm%inod_export_l = 0
      if(ntot_export_e .gt. 0) wk_comm%xe_export = 0.0d0
!
      end subroutine alloc_element_rev_exports
!
!------------------------------------------------------------------
!
      subroutine dealloc_element_rev_imports(wk_comm)
!
      type(work_4_ele_comm_table), intent(inout) :: wk_comm
!
      deallocate(wk_comm%inod_import_e, wk_comm%inod_import_l)
      deallocate(wk_comm%xe_import)
      deallocate(wk_comm%item_local, wk_comm%inod_local)
!
      end subroutine dealloc_element_rev_imports
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_element_rev_exports(wk_comm)
!
      type(work_4_ele_comm_table), intent(inout) :: wk_comm
!
      deallocate(wk_comm%inod_export_e, wk_comm%inod_export_l)
      deallocate(wk_comm%xe_export)
!
      end subroutine dealloc_element_rev_exports
!
!-----------------------------------------------------------------------
!
      end module const_element_comm_table
      