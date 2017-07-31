!>@file   MPI_mesh_data_IO.f90
!!@brief  module MPI_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine mpi_write_geometry_data(IO_param, mesh_IO)
!!      subroutine mpi_write_mesh_groups(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine mpi_read_geometry_data(IO_param, mesh_IO)
!!      subroutine mpi_read_mesh_groups(IO_param, mesh_group_IO)
!!      subroutine mpi_read_num_node_ele(IO_param, mesh_IO)
!!      subroutine mpi_read_num_node(IO_param, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module MPI_mesh_data_IO
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
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use MPI_sph_gl_1d_idx_IO
      use MPI_spherical_model_IO
!
      implicit  none
!
      private :: mpi_write_geometry_info
      private :: mpi_write_element_info, mpi_read_element_info
      private :: mpi_read_element_type, mpi_write_element_type
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_domain_info(IO_param, mesh_IO%nod_comm)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_node()), hd_fem_node())
      call mpi_write_geometry_info(IO_param, mesh_IO%node)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elem()), hd_fem_elem())
      call mpi_write_element_info(IO_param, mesh_IO%ele)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_import()), hd_fem_import())
      call mpi_write_import_data(IO_param, mesh_IO%nod_comm)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_export()), hd_fem_export())
      call mpi_write_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_groups(IO_param, mesh_group_IO)
!
      use MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!   write node group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call mpi_write_grp_data                                           &
     &   (IO_param, mesh_group_IO%nod_grp)
!  write element group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call mpi_write_grp_data                                           &
     &   (IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call mpi_write_surf_grp_data                                      &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_write_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_num_node_ele(IO_param, mesh_IO)
!
      call mpi_read_element_info(IO_param, mesh_IO%ele)
!
! ----  import & export
!
      call mpi_skip_read(IO_param, len(hd_fem_import()))
      call mpi_read_import_data(IO_param, mesh_IO%nod_comm)
!
      call mpi_skip_read(IO_param, len(hd_fem_export()))
      call mpi_read_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine mpi_read_mesh_groups(IO_param, mesh_group_IO)
!
      use MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call mpi_skip_read(IO_param, len(hd_fem_nodgrp()))
      call mpi_read_group_data                                          &
     &   (IO_param, mesh_group_IO%nod_grp)
!  read element group
      call mpi_skip_read(IO_param, len(hd_fem_elegrp()))
      call mpi_read_group_data                                          &
     &   (IO_param, mesh_group_IO%ele_grp)
!  read surface group
      call mpi_skip_read(IO_param, len(hd_fem_sfgrp()))
      call mpi_read_surf_grp_data                                       &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_read_mesh_groups
!
!------------------------------------------------------------------
!
      subroutine mpi_read_num_node_ele(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_num_node(IO_param, mesh_IO)
!
      call alloc_node_geometry_base(mesh_IO%node)
      call mpi_read_node_position(IO_param,                             &
     &    mesh_IO%node%numnod, ithree, mesh_IO%node%inod_global,        &
     &    mesh_IO%node%xx)
!
!  ----  read element data -------
!
      call mpi_skip_read(IO_param, len(hd_fem_elem()))
      call mpi_read_num_of_data(IO_param, mesh_IO%ele%numele)
!
      end subroutine mpi_read_num_node_ele
!
!------------------------------------------------------------------
!
      subroutine mpi_read_num_node(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, mesh_IO%nod_comm)
!
      call mpi_skip_read(IO_param, len(hd_fem_node()))
      call mpi_read_num_of_data(IO_param, mesh_IO%node%internal_node)
      call mpi_read_num_of_data(IO_param, mesh_IO%node%numnod)
!
      end subroutine mpi_read_num_node
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_info(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_write_num_of_data(IO_param, nod_IO%internal_node)
!
      call mpi_write_node_position(IO_param,                            &
     &   nod_IO%numnod, ithree, nod_IO%inod_global, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine mpi_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_info(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
!
      call mpi_write_element_type                                       &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
!
      call mpi_write_ele_connect                                        &
     &   (IO_param, ele_IO%numele, ele_IO%nnod_4_ele,                   &
     &    ele_IO%iele_global, ele_IO%ie)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine mpi_write_element_info
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_element_info(IO_param, ele_IO)
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
      call mpi_read_element_type                                        &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
      call calypso_mpi_barrier
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_ele_connectivity(ele_IO)
!
      call mpi_read_ele_connect                                         &
     &   (IO_param, ele_IO%numele, ele_IO%nnod_4_ele,                   &
     &    ele_IO%iele_global, ele_IO%ie)
      call calypso_mpi_barrier
!
      end subroutine mpi_read_element_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_element_type                                  &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, n_item, ilength, led, loop
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          led = ione
        else if(n_item .le. ncolumn) then
          led = len_multi_6digit_line(n_item)
        else if(n_item .gt. 0) then
          nrest = mod((n_item-1),ncolumn) + 1
          loop = (n_item-1)/ncolumn
          led = len_multi_6digit_line(nrest)                            &
     &         + len_multi_6digit_line(ncolumn) * loop
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1) + led
      end do
      led = int(IO_param%istack_merged(IO_param%id_rank+1)              &
     &         -  IO_param%istack_merged(IO_param%id_rank))
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
        do i = 0, (num-1)/ncolumn - 1
          ilength = len_multi_6digit_line(ncolumn)
          call read_mul_6digit_int_line                                 &
     &       (calypso_mpi_seek_read_chara(IO_param%id_file,             &
     &                                    ioffset, ilength),            &
     &        ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        ilength = len_multi_6digit_line(nrest)
        call read_mul_6digit_int_line                                   &
     &     (calypso_mpi_seek_read_chara(IO_param%id_file,               &
     &                                  ioffset, ilength),              &
     &      nrest, int_dat(num-nrest+1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_element_type                                 &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_write_num_of_data(IO_param, num)
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_6digit_line(ncolumn) * loop                     &
     &       + len_multi_6digit_line(nrest)
      end if
!
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_6digit_line(ncolumn),                           &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_6digit_line(nrest),                               &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_element_type
!
! -----------------------------------------------------------------------
!
      end module MPI_mesh_data_IO
