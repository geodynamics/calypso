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
!!      subroutine init_merged_ucd(node, ele, nod_comm, ucd, m_ucd)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: nod_comm
!!        type(ucd_data), intent(inout) :: ucd
!!        type(merged_ucd_data), intent(inout) :: m_ucd
!!      subroutine finalize_merged_ucd(ucd, m_ucd)
!!
!!      subroutine write_merged_ucd_file(istep, ucd, m_ucd)
!!      subroutine write_merged_udt_file(istep, ucd, m_ucd)
!!      subroutine write_merged_grd_file(ucd, m_ucd)
!!
!!      subroutine write_merged_vtk_file(istep, ucdv)
!!      subroutine write_merged_vtk_phys(istep, ucd, m_ucd)
!!      subroutine write_merged_vtk_grid(ucd, m_ucd)
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
      subroutine init_merged_ucd(node, ele, nod_comm, ucd, m_ucd)
!
      use t_geometry_data
      use t_comm_table
      use m_merged_ucd_data
      use hdf5_file_IO
      use set_ucd_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      write(*,*) 'init_merged_ucd', my_rank
      call link_numnod_stacks_2_output(nprocs, node%istack_numnod,      &
     &    node%istack_internod, ele%istack_interele, m_ucd)
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
!
      if (ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_init_hdf5(ucd, m_ucd)
      end if
!
      end subroutine init_merged_ucd
!
!  ---------------------------------------------------------------------
!
      subroutine finalize_merged_ucd(ucd, m_ucd)
!
      use m_merged_ucd_data
      use hdf5_file_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if (ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_finalize_hdf5(m_ucd)
      end if
      call deallocate_merged_ucd_data(m_ucd)
!
      end subroutine finalize_merged_ucd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_merged_ucd_file(istep, ucd, m_ucd)
!
      use ucd_file_MPI_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_ucd,         &
     &      istep, file_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'UCD data by MPI-IO: ', trim(file_name)
      end if
      call calypso_mpi_barrier
!
      call write_ucd_file_mpi(file_name, ucd, m_ucd)
!
      end subroutine write_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_udt_file(istep, ucd, m_ucd)
!
      use ucd_file_MPI_IO
!
      integer(kind = kint), intent(in) ::  istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_udt,         &
     &      istep, file_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'UCD field by MPI-IO: ', trim(file_name)
      end if
      call calypso_mpi_barrier
!
      call write_ucd_phys_mpi(file_name, ucd, m_ucd)
!
      end subroutine write_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_grd_file(ucd, m_ucd)
!
      use ucd_file_MPI_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_grd_file_name(ucd%file_prefix, iflag_udt,         &
     &    file_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'UCD grid by MPI-IO: ', trim(file_name)
      end if
      call calypso_mpi_barrier
!
      call write_ucd_grid_mpi(file_name, ucd, m_ucd)
!
      end subroutine write_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_merged_vtk_file(istep, ucd, m_ucd)
!
      use vtk_file_MPI_IO
!
      integer(kind = kint), intent(in) ::  istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_vtk,         &
     &      istep, file_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'VTK by MPI-IO: ', trim(file_name)
      end if
      call calypso_mpi_barrier
!
      call write_vtk_file_mpi(file_name, ucd, m_ucd)
!
      end subroutine write_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_phys(istep, ucd, m_ucd)
!
      use vtk_file_MPI_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_vtd,         &
     &      istep, file_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'VTK field by MPI-IO: ', trim(file_name)
      end if
      call calypso_mpi_barrier
!
      call write_vtk_phys_mpi(file_name, ucd, m_ucd)
!
      end subroutine write_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_grid(ucd, m_ucd)
!
      use vtk_file_MPI_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_grd_file_name(ucd%file_prefix, iflag_vtd,         &
     &    file_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'VTK grid by MPI-IO: ', trim(file_name)
      end if
      call calypso_mpi_barrier
!
      call write_vtk_grid_mpi(file_name, ucd, m_ucd)
!
      end subroutine write_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module merged_udt_vtk_file_IO
