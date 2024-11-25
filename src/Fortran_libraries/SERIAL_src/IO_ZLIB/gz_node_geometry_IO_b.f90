!> @file  gz_node_geometry_IO_b.f90
!!      module gz_node_geometry_IO_b
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief Node data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_geometry_info_b(FPz_f, nod_IO, zbuf)
!!      subroutine gz_write_scalar_in_element_b                         &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!      subroutine gz_write_vector_in_element_b                         &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_number_of_node_b(FPz_f, zbuf, nod_IO)
!!      subroutine gz_read_geometry_info_b(FPz_f, zbuf, nod_IO)
!!      subroutine gz_read_scalar_in_element_b                          &
!!     &         (FPz_f, zbuf, nod_IO, sfed_IO)
!!      subroutine gz_read_vector_in_element_b                          &
!!     &         (FPz_f, zbuf, nod_IO, sfed_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
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
      use t_buffer_4_gzip
      use binary_IO
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
      subroutine gz_write_geometry_info_b(FPz_f, nod_IO, zbuf)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_one_integer_b(FPz_f, nod_IO%numnod, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, nod_IO%internal_node, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_mul_int8_b                                          &
     &   (FPz_f, cast_long(nod_IO%numnod), nod_IO%inod_global, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_2d_vector_b                                         &
     &   (FPz_f, cast_long(nod_IO%numnod), ithree, nod_IO%xx, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_scalar_in_element_b                           &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_one_integer_b(FPz_f, nod_IO%numnod, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, nod_IO%internal_node, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_1d_vector_b                                         &
     &   (FPz_f, cast_long(nod_IO%numnod), sfed_IO%ele_scalar, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_vector_in_element_b                           &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_one_integer_b(FPz_f, nod_IO%numnod, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, nod_IO%internal_node, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_2d_vector_b                                         &
     &   (FPz_f, cast_long(nod_IO%numnod), n_vector,                    &
     &    sfed_IO%ele_scalar, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_vector_in_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_node_b(FPz_f, zbuf, nod_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, nod_IO%numnod)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_integer_b(FPz_f, zbuf, nod_IO%internal_node)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_info_b(FPz_f, zbuf, nod_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_read_number_of_node_b(FPz_f, zbuf, nod_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_node_geometry_base(nod_IO)
!
      call gz_read_mul_int8_b                                           &
     &   (FPz_f, zbuf, cast_long(nod_IO%numnod), nod_IO%inod_global)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_2d_vector_b                                          &
     &   (FPz_f, zbuf, cast_long(nod_IO%numnod), ithree, nod_IO%xx)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_scalar_in_element_b                            &
     &         (FPz_f, zbuf, nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_read_number_of_node_b(FPz_f, zbuf, nod_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call gz_read_1d_vector_b                                          &
     &   (FPz_f, zbuf, cast_long(nod_IO%numnod), sfed_IO%ele_scalar)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_vector_in_element_b                            &
     &         (FPz_f, zbuf, nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_read_number_of_node_b(FPz_f, zbuf, nod_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call gz_read_2d_vector_b(FPz_f, zbuf, cast_long(nod_IO%numnod),   &
     &    n_vector, sfed_IO%ele_scalar)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_vector_in_element_b
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO_b
