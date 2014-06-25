!t_comm_table.f90
!      module t_comm_table
!> @brief Structure for communication table
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_type_comm_tbl_num(comm_tbls)
!      subroutine allocate_type_comm_tbl_item(comm_tbls)
!      subroutine deallocate_type_comm_tbl(comm_tbls)
!
!      subroutine allocate_type_neib_id(comm_tbls)
!      subroutine allocate_type_import_num(comm_tbls)
!      subroutine allocate_type_export_num(comm_tbls)
!      subroutine allocate_type_import_item(comm_tbls)
!      subroutine allocate_type_export_item(comm_tbls)
!
!      subroutine deallocate_type_neib_id(comm_tbls)
!      subroutine deallocate_type_import(comm_tbls)
!      subroutine deallocate_type_export(comm_tbls)
!      subroutine deallocate_type_import_num(comm_tbls)
!      subroutine deallocate_type_import_item(comm_tbls)
!      subroutine deallocate_type_export_item(comm_tbls)
!
!      subroutine link_comm_tbl_types(comm_org, comm_tbls)
!        type(communication_table), intent(in) :: comm_org
!        type(communication_table), intent(inout) :: comm_tbls
!
!      subroutine unlink_dest_comm_tbl_type(comm_tbls)
!        type(interpolate_table), intent(inout) :: comm_tbls
!
      module t_comm_table
!
      use m_precision
!
      implicit  none
!
!> data structure for communication table
      type communication_table
!>     number of neighboring domain
        integer(kind = kint) :: num_neib
!>     neighboring pe id
        integer(kind = kint), pointer :: id_neib(:)
!>    total number of import data 
        integer(kind = kint) :: ntot_import
!>     import data count for each neighbor pe (i-th pe)
        integer(kind = kint), pointer :: num_import(:)
!>     import data end point for each neighbor pe (i-th pe)
        integer(kind = kint), pointer :: istack_import(:)
!>      local id for import data                     (i-th)
        integer(kind = kint), pointer :: item_import(:)
!
!>     total number of export data 
        integer(kind = kint) :: ntot_export
!>     export data count for each neighbor pe (i-th pe)
        integer(kind = kint), pointer :: num_export(:)
!>     export data end point for each neighbor pe (i-th pe)
        integer(kind = kint), pointer :: istack_export(:)
!>     local id for export data                     (i-th)
        integer(kind = kint), pointer :: item_export(:)
      end type communication_table
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_type_comm_tbl_num(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
!
      call allocate_type_neib_id(comm_tbls)
      call allocate_type_import_num(comm_tbls)
      call allocate_type_export_num(comm_tbls)
!
      end subroutine allocate_type_comm_tbl_num
!
!------------------------------------------------------------------
!
      subroutine allocate_type_comm_tbl_item(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
!
      call allocate_type_import_item(comm_tbls)
      call allocate_type_export_item(comm_tbls)
!
      end subroutine allocate_type_comm_tbl_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_comm_tbl(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
!
      call deallocate_type_neib_id(comm_tbls)
      call deallocate_type_import(comm_tbls)
      call deallocate_type_export(comm_tbls)
!
      end subroutine deallocate_type_comm_tbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_type_neib_id(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      allocate(comm_tbls%id_neib(comm_tbls%num_neib))
      if (comm_tbls%num_neib .gt. 0) comm_tbls%id_neib =   -1
!
      end subroutine allocate_type_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_type_import_num(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      allocate(comm_tbls%num_import(comm_tbls%num_neib))
      allocate(comm_tbls%istack_import(0:comm_tbls%num_neib))
!
      if (comm_tbls%num_neib .gt. 0) comm_tbls%num_import = 0
      comm_tbls%istack_import = 0
!
      end subroutine allocate_type_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_type_export_num(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      allocate(comm_tbls%num_export(comm_tbls%num_neib))
      allocate(comm_tbls%istack_export(0:comm_tbls%num_neib))
!
      if (comm_tbls%num_neib .gt. 0) comm_tbls%num_export = 0
      comm_tbls%istack_export = 0
!
      end subroutine allocate_type_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_type_import_item(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      comm_tbls%ntot_import                                             &
     &      = comm_tbls%istack_import(comm_tbls%num_neib)
      allocate(comm_tbls%item_import(comm_tbls%ntot_import))
!
      if (comm_tbls%ntot_import .gt. 0) comm_tbls%item_import = 0
!
      end subroutine allocate_type_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_type_export_item(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      comm_tbls%ntot_export                                             &
     &      = comm_tbls%istack_export(comm_tbls%num_neib)
      allocate(comm_tbls%item_export(comm_tbls%ntot_export))
!
      if (comm_tbls%ntot_export .gt. 0) comm_tbls%item_export = 0
!
      end subroutine allocate_type_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_type_neib_id(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      deallocate(comm_tbls%id_neib)
!
      end subroutine deallocate_type_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_type_import(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      call deallocate_type_import_num(comm_tbls)
      call deallocate_type_import_item(comm_tbls)
!
      end subroutine deallocate_type_import
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_export(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      call deallocate_type_export_num(comm_tbls)
      call deallocate_type_export_item(comm_tbls)
!
      end subroutine deallocate_type_export
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_import_num(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      deallocate(comm_tbls%num_import)
      deallocate(comm_tbls%istack_import)
!
      end subroutine deallocate_type_import_num
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_export_num(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      deallocate(comm_tbls%num_export)
      deallocate(comm_tbls%istack_export)
!
      end subroutine deallocate_type_export_num
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_import_item(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      deallocate(comm_tbls%item_import)
!
      end subroutine deallocate_type_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_export_item(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
      deallocate(comm_tbls%item_export)
!
      end subroutine deallocate_type_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine link_comm_tbl_types(comm_org, comm_tbls)
!
      type(communication_table), intent(in) :: comm_org
      type(communication_table), intent(inout) :: comm_tbls
!
!
      comm_tbls%num_neib =    comm_org%num_neib
      comm_tbls%ntot_import = comm_org%ntot_import
      comm_tbls%ntot_export = comm_org%ntot_export
!
      comm_tbls%id_neib =>       comm_org%id_neib
      comm_tbls%num_import =>    comm_org%num_import
      comm_tbls%istack_import => comm_org%istack_import
      comm_tbls%item_import =>   comm_org%item_import
      comm_tbls%num_export =>    comm_org%num_export
      comm_tbls%istack_export => comm_org%istack_export
      comm_tbls%item_export =>   comm_org%item_export
!
      end subroutine link_comm_tbl_types
!
!------------------------------------------------------------------
!
      subroutine unlink_dest_comm_tbl_type(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
!
      comm_tbls%num_neib =    0
      comm_tbls%ntot_import = 0
      comm_tbls%ntot_export = 0
!
      nullify( comm_tbls%id_neib       )
      nullify( comm_tbls%num_import    )
      nullify( comm_tbls%istack_import )
      nullify( comm_tbls%item_import   )
      nullify( comm_tbls%num_export    )
      nullify( comm_tbls%istack_export )
      nullify( comm_tbls%item_export   )
!
      end subroutine unlink_dest_comm_tbl_type
!
!------------------------------------------------------------------
!
      end module t_comm_table
