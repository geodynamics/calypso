!> @file  gz_node_geometry_IO_b.f90
!!      module gz_node_geometry_IO_b
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief Node data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_geometry_info_b(nod_IO)
!!      subroutine gz_write_scalar_in_element_b(nod_IO, sfed_IO)
!!      subroutine gz_write_vector_in_element_b(nod_IO, sfed_IO, bflag)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine gz_read_number_of_node_b(bflag, nod_IO)
!!      subroutine gz_read_geometry_info_b(bflag, nod_IO)
!!      subroutine gz_read_scalar_in_element_b(bflag, nod_IO, sfed_IO)
!!      subroutine gz_read_vector_in_element_b                          &
!!     &         (bflag, nod_IO, sfed_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_node_geometry_IO_b
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_surf_edge_IO
      use binary_IO
      use skip_gz_comment
      use transfer_to_long_integers
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_info_b(nod_IO, bflag)
!
      use gz_binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_one_integer_b(nod_IO%numnod, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_integer_b(nod_IO%internal_node, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_mul_int8_b                                          &
     &   (cast_long(nod_IO%numnod), nod_IO%inod_global, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_2d_vector_b                                         &
     &   (cast_long(nod_IO%numnod), ithree, nod_IO%xx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_scalar_in_element_b(nod_IO, sfed_IO, bflag)
!
      use gz_binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_one_integer_b(nod_IO%numnod, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_integer_b(nod_IO%internal_node, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_1d_vector_b                                         &
     &   (cast_long(nod_IO%numnod), sfed_IO%ele_scalar, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_vector_in_element_b(nod_IO, sfed_IO, bflag)
!
      use gz_binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_one_integer_b(nod_IO%numnod, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_one_integer_b(nod_IO%internal_node, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_2d_vector_b(cast_long(nod_IO%numnod), n_vector,     &
     &    sfed_IO%ele_scalar, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_vector_in_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_node_b(bflag, nod_IO)
!
      use gz_binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_read_one_integer_b(bflag, nod_IO%numnod)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_one_integer_b(bflag, nod_IO%internal_node)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_info_b(bflag, nod_IO)
!
      use gz_binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call gz_read_mul_int8_b                                           &
     &   (bflag, cast_long(nod_IO%numnod), nod_IO%inod_global)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_2d_vector_b                                          &
     &   (bflag, cast_long(nod_IO%numnod), ithree, nod_IO%xx)
!
      end subroutine gz_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_scalar_in_element_b(bflag, nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_read_number_of_node_b(bflag, nod_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call gz_read_1d_vector_b                                          &
     &   (bflag, cast_long(nod_IO%numnod), sfed_IO%ele_scalar)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_vector_in_element_b                            &
     &         (bflag, nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_read_number_of_node_b(bflag, nod_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call gz_read_2d_vector_b(bflag, cast_long(nod_IO%numnod),         &
     &    n_vector, sfed_IO%ele_scalar)
!
      end subroutine gz_read_vector_in_element_b
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO_b
