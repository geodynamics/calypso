!>@file  gz_element_file_IO.f90
!!      module gz_element_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine gz_input_element_file                                &
!!     &         (id_rank, file_name, ele_mesh_IO, ierr)
!!      subroutine gz_input_surface_file                                &
!!     &         (id_rank, file_name, surf_mesh_IO, ierr)
!!      subroutine gz_input_edge_file                                   &
!!     &         (id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine gz_output_element_file                               &
!!     &         (id_rank, file_name, ele_mesh_IO)
!!      subroutine gz_output_surface_file                               &
!!     &         (id_rank, file_name, surf_mesh_IO)
!!      subroutine gz_output_edge_file                                  &
!!     &         (id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_e
      character, pointer, private, save :: FPz_ele
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_input_element_file                                  &
     &         (id_rank, file_name, ele_mesh_IO, ierr)
!
      use skip_gz_comment
      use gz_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped ascii element comm file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_ele, file_name, zbuf_e)
      call gz_read_element_comm_table                                   &
     &   (FPz_ele, id_rank, ele_mesh_IO%comm, zbuf_e, ierr)
!      call gz_read_element_geometry                                    &
!     &   (FPz_ele, ele_mesh_IO%node, ele_mesh_IO%sfed, zbuf_e)
      call close_gzfile_a(FPz_ele, zbuf_e)
!
      end subroutine gz_input_element_file
!
!------------------------------------------------------------------
!
      subroutine gz_input_surface_file                                  &
     &         (id_rank, file_name, surf_mesh_IO, ierr)
!
      use skip_gz_comment
      use gz_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped ascii surface mesh file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_ele, file_name, zbuf_e)
      call gz_read_surface_connection                                   &
     &   (FPz_ele, id_rank, surf_mesh_IO%comm,                          &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed, zbuf_e, ierr)
!      call gz_read_surface_geometry                                    &
!     &   (FPz_ele, surf_mesh_IO%node, surf_mesh_IO%sfed, zbuf_e)
      call close_gzfile_a(FPz_ele, zbuf_e)
!
      end subroutine gz_input_surface_file
!
!------------------------------------------------------------------
!
      subroutine gz_input_edge_file                                     &
     &         (id_rank, file_name, edge_mesh_IO, ierr)
!
      use skip_gz_comment
      use gz_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped ascii edge mesh file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_ele, file_name, zbuf_e)
      call gz_read_edge_connection(FPz_ele, id_rank, edge_mesh_IO%comm, &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed, zbuf_e, ierr)
!      call gz_read_edge_geometry                                       &
!     &   (FPz_ele, edge_mesh_IO%node, edge_mesh_IO%sfed, zbuf_e)
      call close_gzfile_a(FPz_ele, zbuf_e)
!
      end subroutine gz_input_edge_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_output_element_file                                 &
     &         (id_rank, file_name, ele_mesh_IO)
!
      use skip_gz_comment
      use gz_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped ascii element comm file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_ele, file_name, zbuf_e)
      call gz_write_element_comm_table                                  &
     &   (FPz_ele, id_rank, ele_mesh_IO%comm, zbuf_e)
!      call gz_write_element_geometry                                   &
!     &   (FPz_ele, ele_mesh_IO%node, ele_mesh_IO%sfed, zbuf_e)
      call close_gzfile_a(FPz_ele, zbuf_e)
!
      end subroutine gz_output_element_file
!
!------------------------------------------------------------------
!
      subroutine gz_output_surface_file                                 &
     &         (id_rank, file_name, surf_mesh_IO)
!
      use skip_gz_comment
      use gz_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped ascii surface mesh file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_ele, file_name, zbuf_e)
      call gz_write_surface_connection                                  &
     &   (FPz_ele, id_rank, surf_mesh_IO%comm,                          &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed, zbuf_e)
!      call gz_write_surface_geometry                                   &
!     &   (FPz_ele, surf_mesh_IO%node, surf_mesh_IO%sfed, zbuf_e)
      call close_gzfile_a(FPz_ele, zbuf_e)
!
      end subroutine gz_output_surface_file
!
!------------------------------------------------------------------
!
      subroutine gz_output_edge_file                                    &
     &         (id_rank, file_name, edge_mesh_IO)
!
      use skip_gz_comment
      use gz_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped ascii edge mesh file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_ele, file_name, zbuf_e)
      call gz_write_edge_connection(FPz_ele, id_rank,                   &
     &    edge_mesh_IO%comm, edge_mesh_IO%ele, edge_mesh_IO%sfed,       &
     &    zbuf_e)
!      call gz_write_edge_geometry                                      &
!     &   (FPz_ele, edge_mesh_IO%node, edge_mesh_IO%sfed, zbuf_e)
      call close_gzfile_a(FPz_ele, zbuf_e)
!
      end subroutine gz_output_edge_file
!
!------------------------------------------------------------------
!
      end module gz_element_file_IO
