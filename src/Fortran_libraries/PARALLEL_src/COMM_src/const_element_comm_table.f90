!>@file   const_element_comm_table.f90
!!@brief  module const_element_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine elapsed_label_4_ele_comm_tbl
!!
!!      subroutine const_comm_table_by_connenct                         &
!!     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,    &
!!     &          node, nod_comm, neib_e, x_ref_ele, host, e_comm)
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
      use m_work_time
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
!>        global node ID for element import connectivity
!        integer(kind = kint_gl), allocatable :: ie_global_import(:,:)
!>        global node ID for element export connectivity
!        integer(kind = kint_gl), allocatable :: ie_global_export(:,:)
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
      logical, save :: iflag_ecomm_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed
      integer(kind = kint), save :: ied_elapsed
!
      private :: ist_elapsed, ied_elapsed, iflag_ecomm_time
!
!      private :: alloc_element_rev_imports
!      private :: alloc_element_rev_exports
!      private :: dealloc_element_rev_imports
!      private :: dealloc_element_rev_exports
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_ele_comm_tbl
!
      integer(kind = kint), parameter :: num_append = 8
!
!
      call append_elapsed_times(num_append, ist_elapsed, ied_elapsed)
!
      elps1%labels(ist_elapsed+1) = 'count_element_import_num'
      elps1%labels(ist_elapsed+2) = 'local_node_id_reverse_SR'
      elps1%labels(ist_elapsed+3) = 'set_element_import_item'
      elps1%labels(ist_elapsed+4) = 'element_num_reverse_SR'
      elps1%labels(ist_elapsed+5) = 'element_data_reverse_SR'
      elps1%labels(ist_elapsed+6) = 's_set_element_export_item'
      elps1%labels(ist_elapsed+7) = 'element_export_item_in_ext'
      elps1%labels(ist_elapsed+8) = 'check_element_position'
!
      iflag_ecomm_time = .TRUE.
!
      end subroutine elapsed_label_4_ele_comm_tbl
!
!-----------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct                           &
     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,      &
     &          node, nod_comm, neib_e, x_ref_ele, host, e_comm)
!
      use find_element_comm_table
      use const_global_element_ids
      use set_element_export_item
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
      real(kind = kreal), intent(in)                                    &
     &           :: x_ref_ele(neib_e%istack_4_node(node%numnod))
!
      type(communication_table), intent(inout) :: e_comm
!
      type(work_4_ele_comm_table) :: wk_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(e_comm)
      call alloc_import_num(e_comm)
!
!      write(*,*) 'count_element_import_num', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+1)
      call count_element_import_num(node%numnod, host%istack_4_node,    &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+1)
!
      call alloc_element_rev_imports(node%numnod,                       &
     &    nod_comm%ntot_export, e_comm%ntot_import, wk_comm)
      call alloc_import_item(e_comm)
!
!      write(*,*) 'local_node_id_reverse_SR', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+2)
      call local_node_id_reverse_SR                                     &
     &   (node%numnod, nod_comm%num_neib, nod_comm%id_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    wk_comm%item_local, wk_comm%inod_local)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+2)
!
!      write(*,*) 'set_element_import_item', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+3)
      call set_element_import_item(node%numnod, node%internal_node,     &
     &    numele, nnod_4_ele, ie, node%inod_global, x_ele,              &
     &    host%istack_4_node, host%iele_4_node, wk_comm%inod_local,     &
     &    nod_comm%num_neib, nod_comm%istack_import,                    &
     &    nod_comm%item_import, e_comm%num_neib,                        &
     &    e_comm%istack_import, e_comm%item_import,                     &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+3)
!
      call alloc_export_num(e_comm)
!
!      write(*,*) 'element_num_reverse_SR', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+4)
      call element_num_reverse_SR(e_comm%num_neib, e_comm%id_neib,      &
     &    e_comm%num_import, e_comm%num_export, e_comm%istack_export,   &
     &    e_comm%ntot_export)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+4)
!
      call alloc_element_rev_exports(e_comm%ntot_export, wk_comm)
      call alloc_export_item(e_comm)
!
!      write(*,*) 'element_data_reverse_SR', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+5)
      call element_data_reverse_SR(e_comm%num_neib, e_comm%id_neib,     &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import, wk_comm%inod_export_e,                     &
     &    wk_comm%inod_export_l, wk_comm%xe_export)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+5)
!
!      write(*,*) 'set_element_export_item', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+6)
      call s_set_element_export_item(txt, node%numnod, numele,          &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_l, wk_comm%xe_export, e_comm%item_export)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+6)
!
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+7)
      call element_export_item_in_ext                                   &
     &   (txt, node%numnod, numele, node%inod_global,                   &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_e, wk_comm%xe_export, e_comm%item_export)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+7)
!
!
      call dealloc_element_rev_exports(wk_comm)
      call dealloc_element_rev_imports(wk_comm)
!
!      write(*,*) 'check_element_position', my_rank
      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+8)
      call check_element_position(txt, numele, x_ele, e_comm)
      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+8)
!
      end subroutine const_comm_table_by_connenct
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_element_rev_imports                              &
     &        (numnod, ntot_export, ntot_import_e, wk_comm)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ntot_export
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
      subroutine alloc_element_rev_exports                              &
     &         (ntot_export_e, wk_comm)
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
