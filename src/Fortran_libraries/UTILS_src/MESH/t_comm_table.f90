!>@file   t_comm_table.f90
!!@brief  module t_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure for communication table
!!
!!@verbatim
!!      subroutine allocate_type_comm_tbl_num(comm_tbl)
!!      subroutine allocate_type_comm_tbl_item(comm_tbl)
!!      subroutine deallocate_type_comm_tbl(comm_tbl)
!!
!!      subroutine allocate_type_neib_id(comm_tbl)
!!      subroutine allocate_type_import_num(comm_tbl)
!!      subroutine allocate_type_export_num(comm_tbl)
!!      subroutine allocate_type_import_item(comm_tbl)
!!      subroutine allocate_type_export_item(comm_tbl)
!!
!!      subroutine deallocate_type_neib_id(comm_tbl)
!!      subroutine deallocate_type_import(comm_tbl)
!!      subroutine deallocate_type_export(comm_tbl)
!!      subroutine deallocate_type_import_num(comm_tbl)
!!      subroutine deallocate_type_import_item(comm_tbl)
!!      subroutine deallocate_type_export_item(comm_tbl)
!!
!!      subroutine link_comm_tbl_types(comm_org, comm_tbl)
!!        type(communication_table), intent(in) :: comm_org
!!        type(communication_table), intent(inout) :: comm_tbl
!!
!!      subroutine unlink_dest_comm_tbl_type(comm_tbl)
!!        type(interpolate_table), intent(inout) :: comm_tbl
!!
!!      subroutine compare_comm_table_stacks                            &
!!     &         (my_rank, org_comm, new_comm)
!!      type(communication_table), intent(in) :: org_comm
!!      type(communication_table), intent(in) :: new_comm
!!@endverbatim
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
      subroutine allocate_type_comm_tbl_num(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      call allocate_type_neib_id(comm_tbl)
      call allocate_type_import_num(comm_tbl)
      call allocate_type_export_num(comm_tbl)
!
      end subroutine allocate_type_comm_tbl_num
!
!------------------------------------------------------------------
!
      subroutine allocate_type_comm_tbl_item(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      call allocate_type_import_item(comm_tbl)
      call allocate_type_export_item(comm_tbl)
!
      end subroutine allocate_type_comm_tbl_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_comm_tbl(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      call deallocate_type_neib_id(comm_tbl)
      call deallocate_type_import(comm_tbl)
      call deallocate_type_export(comm_tbl)
!
      end subroutine deallocate_type_comm_tbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_type_neib_id(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      allocate(comm_tbl%id_neib(comm_tbl%num_neib))
      if (comm_tbl%num_neib .gt. 0) comm_tbl%id_neib =   -1
!
      end subroutine allocate_type_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_type_import_num(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      allocate(comm_tbl%num_import(comm_tbl%num_neib))
      allocate(comm_tbl%istack_import(0:comm_tbl%num_neib))
!
      if (comm_tbl%num_neib .gt. 0) comm_tbl%num_import = 0
      comm_tbl%istack_import = 0
!
      end subroutine allocate_type_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_type_export_num(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      allocate(comm_tbl%num_export(comm_tbl%num_neib))
      allocate(comm_tbl%istack_export(0:comm_tbl%num_neib))
!
      if (comm_tbl%num_neib .gt. 0) comm_tbl%num_export = 0
      comm_tbl%istack_export = 0
!
      end subroutine allocate_type_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_type_import_item(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      allocate(comm_tbl%item_import(comm_tbl%ntot_import))
      if (comm_tbl%ntot_import .gt. 0) comm_tbl%item_import = 0
!
      end subroutine allocate_type_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_type_export_item(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      allocate(comm_tbl%item_export(comm_tbl%ntot_export))
      if (comm_tbl%ntot_export .gt. 0) comm_tbl%item_export = 0
!
      end subroutine allocate_type_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_type_neib_id(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      deallocate(comm_tbl%id_neib)
!
      end subroutine deallocate_type_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_type_import(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      call deallocate_type_import_num(comm_tbl)
      call deallocate_type_import_item(comm_tbl)
!
      end subroutine deallocate_type_import
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_export(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      call deallocate_type_export_num(comm_tbl)
      call deallocate_type_export_item(comm_tbl)
!
      end subroutine deallocate_type_export
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_import_num(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      deallocate(comm_tbl%num_import)
      deallocate(comm_tbl%istack_import)
!
      end subroutine deallocate_type_import_num
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_export_num(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      deallocate(comm_tbl%num_export)
      deallocate(comm_tbl%istack_export)
!
      end subroutine deallocate_type_export_num
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_import_item(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      deallocate(comm_tbl%item_import)
!
      end subroutine deallocate_type_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_type_export_item(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      deallocate(comm_tbl%item_export)
!
      end subroutine deallocate_type_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine link_comm_tbl_types(comm_org, comm_tbl)
!
      type(communication_table), intent(in) :: comm_org
      type(communication_table), intent(inout) :: comm_tbl
!
!
      comm_tbl%num_neib =    comm_org%num_neib
      comm_tbl%ntot_import = comm_org%ntot_import
      comm_tbl%ntot_export = comm_org%ntot_export
!
      comm_tbl%id_neib =>       comm_org%id_neib
      comm_tbl%num_import =>    comm_org%num_import
      comm_tbl%istack_import => comm_org%istack_import
      comm_tbl%item_import =>   comm_org%item_import
      comm_tbl%num_export =>    comm_org%num_export
      comm_tbl%istack_export => comm_org%istack_export
      comm_tbl%item_export =>   comm_org%item_export
!
      end subroutine link_comm_tbl_types
!
!------------------------------------------------------------------
!
      subroutine unlink_dest_comm_tbl_type(comm_tbl)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      comm_tbl%num_neib =    0
      comm_tbl%ntot_import = 0
      comm_tbl%ntot_export = 0
!
      nullify( comm_tbl%id_neib       )
      nullify( comm_tbl%num_import    )
      nullify( comm_tbl%istack_import )
      nullify( comm_tbl%item_import   )
      nullify( comm_tbl%num_export    )
      nullify( comm_tbl%istack_export )
      nullify( comm_tbl%item_export   )
!
      end subroutine unlink_dest_comm_tbl_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine compare_comm_table_stacks                              &
     &         (my_rank, org_comm, new_comm)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: org_comm
      type(communication_table), intent(in) :: new_comm
!
!
        write(*,*)'new_comm%num_neib',                                  &
     &       my_rank, org_comm%num_neib, new_comm%num_neib 
        write(*,*)'new_comm%id_neib', my_rank, new_comm%id_neib
        write(*,*)'new_comm%ntot_import',                               &
     &       my_rank, org_comm%ntot_import, new_comm%ntot_import
        write(*,*)'new_comm%ntot_export',                               &
     &       my_rank, org_comm%ntot_export, new_comm%ntot_export
        write(*,*)'id_neib',  my_rank, org_comm%id_neib
        write(*,*)'istack_import',  my_rank, org_comm%istack_import
        write(*,*)'new_comm%istack_import',                             &
     &       my_rank, new_comm%istack_import
        write(*,*)'istack_export',  my_rank, org_comm%istack_export
        write(*,*)'new_comm%istack_export',                             &
     &       my_rank, new_comm%istack_export
!
      end subroutine compare_comm_table_stacks
!
!------------------------------------------------------------------
!
      end module t_comm_table
