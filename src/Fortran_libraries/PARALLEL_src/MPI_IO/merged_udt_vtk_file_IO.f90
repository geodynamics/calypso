!>@file  merged_udt_vtk_file_IO.f90
!!       module merged_udt_vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in July, 2006
!!@n      Modified in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine init_merged_ucd                                      &
!!     &         (iflag_format, node, ele, nod_comm, ucd, m_ucd)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: nod_comm
!!        type(ucd_data), intent(inout) :: ucd
!!        type(merged_ucd_data), intent(inout) :: m_ucd
!!      subroutine finalize_merged_ucd(iflag_format, m_ucd)
!!        type(merged_ucd_data), intent(inout) :: m_ucd
!!
!!      subroutine write_ucd_file_mpi(file_name, ucd, m_ucd)
!!      subroutine write_ucd_phys_mpi(file_name, ucd, m_ucd)
!!      subroutine write_ucd_grid_mpi(file_name, ucd, m_ucd)
!!
!!      subroutine write_vtk_file_mpi(file_name, ucd, m_ucd)
!!      subroutine write_vtk_phys_mpi(file_name, ucd, m_ucd)
!!      subroutine write_vtk_grid_mpi(file_name, ucd, m_ucd)
!!        type(ucd_data), intent(in) :: ucd
!!        type(merged_ucd_data), intent(in) :: m_ucd
!!@endverbatim
!
      module merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_field_file_format
!
      use t_ucd_data
!
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_merged_ucd                                        &
     &         (iflag_format, node, ele, nod_comm, ucd, m_ucd)
!
      use t_geometry_data
      use t_comm_table
      use m_merged_ucd_data
      use hdf5_file_IO
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: iflag_format
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      call link_nnod_stacks_2_ucd(nprocs, node, ele, m_ucd)
!
      call allocate_merged_ucd_data(node%numnod)
      call set_node_double_address                                      &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export)
!
      call update_ele_by_double_address                                 &
     &   (node%istack_internod, m_ucd, ucd)
!
      if(iflag_format .eq. iflag_sgl_hdf5) then
        call parallel_init_hdf5(ucd, m_ucd)
      end if
!
      end subroutine init_merged_ucd
!
!  ---------------------------------------------------------------------
!
      subroutine finalize_merged_ucd(iflag_format, m_ucd)
!
      use m_merged_ucd_data
      use hdf5_file_IO
!
      integer(kind = kint), intent(in) :: iflag_format
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if(iflag_format .eq. iflag_sgl_hdf5) then
        call parallel_finalize_hdf5(m_ucd)
      end if
      call deallocate_merged_ucd_data(m_ucd)
!
      end subroutine finalize_merged_ucd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_file_mpi(file_name, ucd, m_ucd)
!
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
      if(my_rank .eq. 0) then
        write(*,*) 'UCD data by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_ucd_node_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%ntot_comp, ucd%xx,                              &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
      call write_ucd_connect_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nele, ucd%nnod_4_ele, ucd%ie, m_ucd%istack_merged_ele)
!
      call write_ucd_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_ucd_file_mpi
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_phys_mpi(file_name, ucd, m_ucd)
!
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
     if(my_rank .eq. 0) then
        write(*,*) 'UCD field by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_ucd_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_ucd_phys_mpi
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_grid_mpi(file_name, ucd, m_ucd)
!
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
      if(my_rank .eq. 0) then
        write(*,*) 'UCD grid by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_ucd_node_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%ntot_comp, ucd%xx,                              &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
      call write_ucd_connect_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nele, ucd%nnod_4_ele, ucd%ie, m_ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_ucd_grid_mpi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_vtk_file_mpi(file_name, ucd, m_ucd)
!
      use vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
     if(my_rank .eq. 0) then
        write(*,*) 'VTK by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys_mpi(file_name, ucd, m_ucd)
!
      use vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
     if(my_rank .eq. 0) then
        write(*,*) 'VTK field by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_phys_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid_mpi(file_name, ucd, m_ucd)
!
      use vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
     if(my_rank .eq. 0) then
        write(*,*) 'VTK grid by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_grid_mpi
!
! -----------------------------------------------------------------------
!
      end module merged_udt_vtk_file_IO
