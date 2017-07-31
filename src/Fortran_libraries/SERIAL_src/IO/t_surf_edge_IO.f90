!>@file   t_surf_edge_IO.f90
!!@brief  module t_surf_edge_IO
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!      subroutine alloc_surface_connect_IO                             &
!!     &         (numele, num_of_surf, sfed_IO)
!!      subroutine alloc_edge_connect_IO(numele, num_of_edge, sfed_IO)
!!
!!      subroutine dealloc_surface_connect_IO(sfed_IO)
!!      subroutine dealloc_edge_connect_IO(sfed_IO)
!!
!!      subroutine alloc_ele_vector_IO(nod_IO, sfed_IO)
!!      subroutine dealloc_ele_vector_IO(sfed_IO)
!!
!!      subroutine alloc_ele_scalar_IO(nod_IO, sfed_IO)
!!      subroutine dealloc_ele_scalar_IO(sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module t_surf_edge_IO
!
      use m_precision
      use t_geometry_data
!
      implicit  none
!
!>
      type surf_edge_IO_data
        real(kind=kreal),   allocatable :: ele_vector(:,:)
        real(kind=kreal),   allocatable :: ele_scalar(:)
!
        integer(kind = kint) :: nsf_4_ele
        integer(kind = kint) :: nsurf_in_ele
        integer(kind = kint), allocatable  :: isf_for_ele(:,:)
!
        integer(kind = kint) :: ned_4_ele
        integer(kind = kint) :: nedge_in_ele
        integer(kind = kint), allocatable  :: iedge_for_ele(:,:)
      end type surf_edge_IO_data
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine alloc_surface_connect_IO                               &
     &         (numele, num_of_surf, sfed_IO)
!
      integer(kind = kint), intent(in) :: numele, num_of_surf
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      sfed_IO%nsf_4_ele = numele
      sfed_IO%nsurf_in_ele = num_of_surf
      allocate( sfed_IO%isf_for_ele(numele,num_of_surf) )
      sfed_IO%isf_for_ele = 0
!
      end subroutine alloc_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine alloc_edge_connect_IO(numele, num_of_edge, sfed_IO)
!
      integer(kind = kint), intent(in) :: numele, num_of_edge
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      sfed_IO%ned_4_ele = numele
      sfed_IO%nedge_in_ele = num_of_edge
      allocate(sfed_IO%iedge_for_ele(numele,num_of_edge) )
      sfed_IO%iedge_for_ele = 0
!
      end subroutine alloc_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_surface_connect_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%isf_for_ele )
!
      end subroutine dealloc_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_connect_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%iedge_for_ele )
!
      end subroutine dealloc_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      allocate( sfed_IO%ele_vector(nod_IO%numnod,3) )
      sfed_IO%ele_vector = 0.0d0
!
      end subroutine alloc_ele_vector_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_ele_vector_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%ele_vector )
!
      end subroutine dealloc_ele_vector_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      allocate( sfed_IO%ele_scalar(nod_IO%numnod) )
      sfed_IO%ele_scalar = 0.0d0
!
      end subroutine alloc_ele_scalar_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_ele_scalar_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%ele_scalar )
!
      end subroutine dealloc_ele_scalar_IO
!
!------------------------------------------------------------------
!
      end module t_surf_edge_IO
