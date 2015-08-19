!>@file   m_comm_data_IO.f90
!!@brief  module m_comm_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Array for communication table IO
!!
!!@verbatim
!!      subroutine allocate_neib_comm_stack_IO
!!      subroutine allocate_comm_item_IO
!!      subroutine deallocate_comm_item_IO
!!
!!      subroutine allocate_neib_domain_IO
!!      subroutine allocate_import_stack_IO
!!      subroutine allocate_export_stack_IO
!!      subroutine allocate_import_item_IO
!!      subroutine allocate_export_item_IO
!!      subroutine allocate_import_work_IO
!!      subroutine allocate_export_work_IO
!!
!!      subroutine deallocate_neib_domain_IO
!!      subroutine deallocate_import_item_IO
!!      subroutine deallocate_export_item_IO
!!      subroutine deallocate_import_work_IO
!!      subroutine deallocate_export_work_IO
!!@verbatim
!
      module m_comm_data_IO
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: my_rank_IO
!
      integer(kind = kint) :: num_neib_domain_IO
      integer(kind = kint), allocatable :: id_neib_domain_IO(:)
      integer(kind = kint), allocatable :: istack_import_IO(:)
      integer(kind = kint), allocatable :: istack_export_IO(:)
!
      integer(kind = kint) :: nwork_import_IO
      integer(kind = kint) :: ntot_import_IO
      integer(kind = kint), allocatable :: item_import_IO(:)
      integer(kind = kint), allocatable :: iwork_import_IO(:,:)
!
      integer(kind = kint) :: nwork_export_IO
      integer(kind = kint) :: ntot_export_IO
      integer(kind = kint), allocatable :: item_export_IO(:)
      integer(kind = kint), allocatable :: iwork_export_IO(:,:)
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_neib_comm_stack_IO
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_export_stack_IO
!
      end subroutine allocate_neib_comm_stack_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_comm_item_IO
!
      call allocate_import_item_IO
      call allocate_export_item_IO
!
      end subroutine allocate_comm_item_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_comm_item_IO
!
      call deallocate_import_item_IO
      call deallocate_export_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine deallocate_comm_item_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_neib_domain_IO
!
      allocate(id_neib_domain_IO(num_neib_domain_IO))
!
      if (num_neib_domain_IO .gt. 0) id_neib_domain_IO = 0
!
      end subroutine allocate_neib_domain_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_import_stack_IO
!
      allocate(istack_import_IO(0:num_neib_domain_IO))
      istack_import_IO = 0
!
      end subroutine allocate_import_stack_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_export_stack_IO
!
      allocate(istack_export_IO(0:num_neib_domain_IO))
      istack_export_IO = 0
!
      end subroutine allocate_export_stack_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_import_item_IO
!
      allocate(item_import_IO(ntot_import_IO))
      if (ntot_import_IO.gt.0) item_import_IO = 0
!
      end subroutine allocate_import_item_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_export_item_IO
!
      allocate(item_export_IO(ntot_export_IO))
      if (ntot_export_IO.gt.0)  item_export_IO = 0
!
      end subroutine allocate_export_item_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_import_work_IO
!
      allocate(iwork_import_IO(ntot_import_IO,nwork_import_IO))
      if (ntot_import_IO.gt.0) iwork_import_IO = 0
!
      end subroutine allocate_import_work_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_export_work_IO
!
      allocate(iwork_export_IO(ntot_export_IO,nwork_export_IO))
      if (ntot_export_IO.gt.0) iwork_export_IO = 0
!
      end subroutine allocate_export_work_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_neib_domain_IO
!
      deallocate(id_neib_domain_IO)
!
      end subroutine deallocate_neib_domain_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_import_item_IO
!
      deallocate(item_import_IO)
      deallocate(istack_import_IO)
!
      end subroutine deallocate_import_item_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_export_item_IO
!
      deallocate(item_export_IO)
      deallocate(istack_export_IO)
!
      end subroutine deallocate_export_item_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_import_work_IO
!
      deallocate(iwork_import_IO)
!
      end subroutine deallocate_import_work_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_export_work_IO
!
      deallocate(iwork_export_IO)
!
      end subroutine deallocate_export_work_IO
!
!------------------------------------------------------------------
!
      end module m_comm_data_IO
