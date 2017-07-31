!>@file   gz_MPI_mesh_data_IO.f90
!!@brief  module gz_MPI_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_geometry_data(IO_param, mesh_IO)
!!      subroutine gz_mpi_write_mesh_groups(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine gz_mpi_read_geometry_data(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_mesh_groups(IO_param, mesh_group_IO)
!!      subroutine gz_mpi_read_num_node_ele(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_num_node(IO_param, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module gz_MPI_mesh_data_IO
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_calypso_mpi_IO_param
      use m_fem_mesh_labels
!
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_sph_gl_1d_idx_IO
      use gz_MPI_spherical_model_IO
!
      implicit  none
!
!      private :: gz_mpi_write_geometry_info
!      private :: gz_mpi_write_element_info
!      private :: gz_mpi_read_number_of_node
!      private :: gz_mpi_read_element_info
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_node()), hd_fem_node())
      call gz_mpi_write_geometry_info(IO_param, mesh_IO%node)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_elem()), hd_fem_elem())
      call gz_mpi_write_element_info(IO_param, mesh_IO%ele)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_import()), hd_fem_import())
      call gz_mpi_write_import_data(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_export()), hd_fem_export())
      call gz_mpi_write_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine gz_mpi_write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_mesh_groups(IO_param, mesh_group_IO)
!
      use gz_MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!   write node group
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call gz_mpi_write_grp_data                                        &
     &   (IO_param, mesh_group_IO%nod_grp)
!  write element group
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call gz_mpi_write_grp_data                                        &
     &   (IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call gz_mpi_write_surf_grp_data                                   &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine gz_mpi_write_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_num_node_ele(IO_param, mesh_IO)
!
      call gz_mpi_read_element_info(IO_param, mesh_IO%ele)
!
! ----  import & export
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_import()))
      call gz_mpi_read_import_data(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_export()))
      call gz_mpi_read_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine gz_mpi_read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_groups(IO_param, mesh_group_IO)
!
      use gz_MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call gz_mpi_skip_header(IO_param, len(hd_fem_nodgrp()))
      call gz_mpi_read_group_data                                       &
     &   (IO_param, mesh_group_IO%nod_grp)
!  read element group
      call gz_mpi_skip_header(IO_param, len(hd_fem_elegrp()))
      call gz_mpi_read_group_data                                       &
     &   (IO_param, mesh_group_IO%ele_grp)
!  read surface group
      call gz_mpi_skip_header(IO_param, len(hd_fem_sfgrp()))
      call gz_mpi_read_surf_grp_data                                    &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine gz_mpi_read_mesh_groups
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_node_ele(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_num_node(IO_param, mesh_IO)
!
      call alloc_node_geometry_base(mesh_IO%node)
      call gz_mpi_read_node_position(IO_param,                          &
     &    mesh_IO%node%numnod, ithree, mesh_IO%node%inod_global,        &
     &    mesh_IO%node%xx)
!
!  ----  read element data -------
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_elem()))
      call gz_mpi_read_num_of_data(IO_param, mesh_IO%ele%numele)
!
      end subroutine gz_mpi_read_num_node_ele
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_node(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_node()))
      call gz_mpi_read_num_of_data                                      &
     &   (IO_param, mesh_IO%node%internal_node)
      call gz_mpi_read_num_of_data(IO_param, mesh_IO%node%numnod)
!
      end subroutine gz_mpi_read_num_node
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_info(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_mpi_write_num_of_data(IO_param, nod_IO%internal_node)
      call gz_mpi_write_node_position(IO_param,                         &
     &   nod_IO%numnod, ithree, nod_IO%inod_global, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine gz_mpi_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_info(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
!
      call gz_mpi_write_element_type                                    &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
!
      call gz_mpi_write_ele_connect                                     &
     &   (IO_param, ele_IO%numele, ele_IO%nnod_4_ele,                   &
     &    ele_IO%iele_global, ele_IO%ie)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine gz_mpi_write_element_info
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_element_info(IO_param, ele_IO)
!
      use set_nnod_4_ele_by_type
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i, num_tmp
!
!
      call alloc_element_types(ele_IO)
      call gz_mpi_read_element_type                                     &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_ele_connectivity(ele_IO)
!
      call gz_mpi_read_ele_connect                                      &
     &   (IO_param, ele_IO%numele, ele_IO%nnod_4_ele,                   &
     &    ele_IO%iele_global, ele_IO%ie)
!
      end subroutine gz_mpi_read_element_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_element_type                               &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      allocate(textbuf(len_multi_6digit_line(ncolumn)))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(num .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(num .le. ncolumn) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), len_multi_6digit_line(num),            &
     &     textbuf(1), ilen_gzipped)
        call read_mul_6digit_int_line(textbuf(1), num, int_dat(1))
      else if(num .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), len_multi_6digit_line(ncolumn),         &
     &    textbuf(1), ilen_gzipped)
        call read_mul_6digit_int_line(textbuf(1), ncolumn, int_dat(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, len_multi_6digit_line(ncolumn),                  &
     &        textbuf(1), ilen_gzipped)
          call read_mul_6digit_int_line                                 &
     &       (textbuf(1), ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_infleat_last                                          &
     &     (ilen_gz, len_multi_6digit_line(nrest),                      &
     &      textbuf(1), ilen_gzipped)
        call read_mul_6digit_int_line                                   &
     &     (textbuf(1), nrest, int_dat(num-nrest+1))
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_type                              &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call gz_mpi_write_num_of_data(IO_param, num)
!
      ilen_gz = int(real(num*len_6digit_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .le. ncolumn) then
        call gzip_defleat_once(len_multi_6digit_line(num),              &
     &      mul_6digit_int_line(num, int_dat(1)),                       &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .gt. 0) then
        call gzip_defleat_begin(len_multi_6digit_line(ncolumn),         &
     &      mul_6digit_int_line(ncolumn, int_dat(1)),                   &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_defleat_cont(len_multi_6digit_line(ncolumn),        &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)),       &
     &        ilen_gz, ilen_gzipped)
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_defleat_last(len_multi_6digit_line(nrest),            &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)),           &
     &      ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_element_type
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_mesh_data_IO
