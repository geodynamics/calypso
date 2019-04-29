!>@file   node_geometry_IO_b.f90
!!@brief  module node_geometry_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for Binary mesh data IO
!!
!!@verbatim
!!      subroutine write_geometry_info_b(nod_IO, bflag)
!!      subroutine write_scalar_in_element_b(nod_IO, sfed_IO, bflag)
!!      subroutine write_vector_in_element_b(nod_IO, sfed_IO, bflag)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(binary_IO_flags), intent(inout) :: bflag
!!
!!      subroutine read_number_of_node_b(bflag, nod_IO)
!!      subroutine read_geometry_info_b(bflag, nod_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(node_data), intent(inout) :: nod_IO
!!
!!      subroutine read_scalar_in_element_b(bflag, nod_IO, sfed_IO)
!!      subroutine read_vector_in_element_b(bflag, nod_IO, sfed_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module node_geometry_IO_b
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_info_b(nod_IO, bflag)
!
      use binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(nod_IO%numnod, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_one_integer_b(nod_IO%internal_node, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = nod_IO%numnod
      call write_mul_int8_b(num64, nod_IO%inod_global, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_2d_vector_b(num64, n_vector, nod_IO%xx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine write_scalar_in_element_b(nod_IO, sfed_IO, bflag)
!
      use binary_IO
      use transfer_to_long_integers
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_one_integer_b(nod_IO%numnod, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_one_integer_b(nod_IO%internal_node, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call write_1d_vector_b                                            &
     &   (cast_long(nod_IO%numnod), sfed_IO%ele_scalar, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine write_vector_in_element_b(nod_IO, sfed_IO, bflag)
!
      use binary_IO
      use transfer_to_long_integers
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_one_integer_b(nod_IO%numnod, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_one_integer_b(nod_IO%internal_node, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call write_2d_vector_b(cast_long(nod_IO%numnod), n_vector,        &
     &   sfed_IO%ele_vector, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_vector_in_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node_b(bflag, nod_IO)
!
      use binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
!
!
      call read_one_integer_b(bflag, nod_IO%numnod)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_one_integer_b(bflag, nod_IO%internal_node)
!
      end subroutine read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info_b(bflag, nod_IO)
!
      use binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_node_geometry_base(nod_IO)
!
      num64 = nod_IO%numnod
      call read_mul_int8_b(bflag, num64, nod_IO%inod_global)
      if(bflag%ierr_IO .ne. 0) return
      call read_2d_vector_b(bflag, num64, n_vector, nod_IO%xx)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_geometry_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_scalar_in_element_b(bflag, nod_IO, sfed_IO)
!
      use binary_IO
      use transfer_to_long_integers
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_one_integer_b(bflag, nod_IO%numnod)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_one_integer_b(bflag, nod_IO%internal_node)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call read_1d_vector_b                                             &
     &   (bflag, cast_long(nod_IO%numnod), sfed_IO%ele_scalar)
     if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine read_vector_in_element_b(bflag, nod_IO, sfed_IO)
!
      use binary_IO
      use transfer_to_long_integers
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_one_integer_b(bflag, nod_IO%numnod)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_one_integer_b(bflag, nod_IO%internal_node)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call read_2d_vector_b(bflag, cast_long(nod_IO%numnod), n_vector,  &
     &    sfed_IO%ele_vector)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_vector_in_element_b
!
!------------------------------------------------------------------
!
      end module node_geometry_IO_b
