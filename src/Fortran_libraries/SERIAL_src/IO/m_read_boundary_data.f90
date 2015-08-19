!> @file  m_read_boundary_data.f90
!!      module m_read_boundary_data
!!
!! @author  H. Matsui and H. Okuda
!! @date Written in 2001
!
!> @brief Array for group data IO
!!
!!@verbatim
!!      subroutine allocate_bc_stack_dummy
!!      subroutine allocate_bc_item_dummy
!!      subroutine allocate_bc_ele_stack_dummy
!!      subroutine allocate_bc_ele_item_dummy
!!      subroutine allocate_bc_sf_stack_dummy
!!      subroutine allocate_bc_sf_item_dummy
!!
!!      subroutine deallocate_bc_item_dummy
!!      subroutine deallocate_bc_ele_item_dummy
!!      subroutine deallocate_bc_sf_item_dummy
!!
!!      subroutine deallocate_boundary_arrays
!!@endverbatim
!
      module m_read_boundary_data
!
      use m_precision
!
      implicit  none
!
!   node group
!
      integer(kind=kint) :: num_bc_dummy
      integer(kind=kint) :: num_nod_bc_dummy
! 
      integer(kind=kint), allocatable :: bc_istack_dummy(:)
      integer(kind=kint), allocatable :: bc_item_dummy(:)
      character(len=kchara), allocatable :: bc_name_dummy(:)
!
!   element group
!
      integer(kind=kint) :: num_mat_dummy
      integer(kind=kint) :: num_mat_bc_dummy
      integer(kind=kint), allocatable :: mat_istack_dummy(:)
      integer(kind=kint), allocatable :: mat_item_dummy(:)
      character(len=kchara), allocatable :: mat_name_dummy(:)
! 
!   surface group
!
      integer (kind=kint) :: num_surf_dummy
      integer (kind=kint) :: num_surf_bc_dummy
      integer (kind=kint), allocatable :: surf_istack_dummy(:)
      integer (kind=kint), allocatable :: surf_item_dummy(:,:)
      character (len=kchara), allocatable :: surf_name_dummy(:)
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_bc_stack_dummy
!
        allocate(bc_istack_dummy(0:num_bc_dummy))
        allocate(bc_name_dummy(num_bc_dummy))
        bc_istack_dummy= 0
!
      end subroutine allocate_bc_stack_dummy
!
! ----------------------------------------------------------------------
!
      subroutine allocate_bc_item_dummy
!
        num_nod_bc_dummy=bc_istack_dummy(num_bc_dummy)
!
        allocate(bc_item_dummy(num_nod_bc_dummy))
        if(num_nod_bc_dummy .gt. 0) bc_item_dummy=0
!
      end subroutine allocate_bc_item_dummy
!
! ----------------------------------------------------------------------
!
      subroutine allocate_bc_ele_stack_dummy
!
        allocate(mat_istack_dummy(0:num_mat_dummy))
        allocate(mat_name_dummy(num_mat_dummy))
        mat_istack_dummy=0
!
      end subroutine allocate_bc_ele_stack_dummy
!
! ----------------------------------------------------------------------
!
      subroutine allocate_bc_ele_item_dummy
!
        num_mat_bc_dummy=mat_istack_dummy(num_mat_dummy)
!
        allocate(mat_item_dummy(num_mat_bc_dummy))
        if(num_mat_bc_dummy .gt. 0) mat_item_dummy=0
!
      end subroutine allocate_bc_ele_item_dummy
!
! ----------------------------------------------------------------------
!
      subroutine allocate_bc_sf_stack_dummy
!
        allocate(surf_istack_dummy(0:num_surf_dummy))
        allocate(surf_name_dummy(num_surf_dummy))
        surf_istack_dummy = 0
!
      end subroutine allocate_bc_sf_stack_dummy
!
! ----------------------------------------------------------------------
!
      subroutine allocate_bc_sf_item_dummy
!
        num_surf_bc_dummy = surf_istack_dummy(num_surf_dummy)
!
        allocate(surf_item_dummy(num_surf_bc_dummy,2))
        if(num_surf_bc_dummy .gt. 0) surf_item_dummy=0
!
      end subroutine allocate_bc_sf_item_dummy
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_bc_item_dummy
!
      deallocate(bc_istack_dummy)
      deallocate(bc_name_dummy)
      deallocate(bc_item_dummy)
!
      end subroutine deallocate_bc_item_dummy
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_bc_ele_item_dummy
!
!
      deallocate(mat_istack_dummy)
      deallocate(mat_name_dummy)
      deallocate(mat_item_dummy)
!
      end subroutine deallocate_bc_ele_item_dummy
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_bc_sf_item_dummy
!
!
      deallocate(surf_istack_dummy)
      deallocate(surf_name_dummy)
      deallocate(surf_item_dummy) 
!
      end subroutine deallocate_bc_sf_item_dummy
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_boundary_arrays
!
!
      call deallocate_bc_item_dummy
      call deallocate_bc_ele_item_dummy
      call deallocate_bc_sf_item_dummy
!
      end subroutine deallocate_boundary_arrays
!
! ----------------------------------------------------------------------
!
      end module m_read_boundary_data
