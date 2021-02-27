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
!!      subroutine init_merged_ucd_element                              &
!!     &         (iflag_format, node, ele, nod_comm, ucd)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: nod_comm
!!        type(ucd_data), intent(inout) :: ucd
!!      subroutine finalize_merged_ucd(iflag_format, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine write_ucd_file_mpi(file_name, ucd)
!!      subroutine write_ucd_phys_mpi(file_name, ucd)
!!      subroutine write_ucd_grid_mpi(file_name, ucd)
!!
!!      subroutine write_vtk_file_mpi(file_name, ucd)
!!      subroutine write_vtk_phys_mpi(file_name, ucd)
!!      subroutine write_vtk_grid_mpi(file_name, ucd)
!!        type(ucd_data), intent(in) :: ucd
!!@endverbatim
!
      module merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_field_file_format
!
      use t_time_data
      use t_ucd_data
      use t_para_double_numbering
      use t_calypso_mpi_IO_param
!
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
!>      Structure of double numbering
      type(node_ele_double_number), private, save :: dbl_id1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_merged_ucd_element                                &
     &         (iflag_format, node, ele, nod_comm, ucd)
!
      use t_geometry_data
      use t_comm_table
      use hdf5_file_IO
      use set_ucd_data_to_type
      use pickup_internal_element
      use const_global_element_ids
!
      integer(kind = kint), intent(in) :: iflag_format
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(ucd_data), intent(inout) :: ucd
!
!
      call alloc_merged_ucd_nod_stack(nprocs, ucd)
      call count_number_of_node_stack(node%numnod,                      &
     &                                ucd%istack_merged_nod)
      call count_number_of_node_stack(node%internal_node,               &
     &                                ucd%istack_merged_intnod)
!
      ucd%nele = num_internal_element_4_IO                              &
     &         (node%internal_node, ele%numele, ele%nnod_4_ele, ele%ie)
      call alloc_merged_ucd_ele_stack(nprocs, ucd)
      call count_number_of_node_stack                                   &
     &   (int(ucd%nele), ucd%istack_merged_ele)
!
!
      call alloc_double_numbering(node%numnod, dbl_id1)
      call set_node_double_numbering(node, nod_comm, dbl_id1)
!
      ucd%nnod_4_ele = ele%nnod_4_ele
      call allocate_ucd_ele(ucd)
!
      call set_internal_element_4_IO                                    &
     &   (nprocs, ucd%istack_merged_intnod, node%numnod,                &
     &    node%internal_node,  ele%numele, ele%nnod_4_ele, ele%ie,      &
     &    dbl_id1%index, dbl_id1%irank,                                 &
     &    ucd%nele, ucd%nnod_4_ele, ucd%ie)
      call dealloc_double_numbering(dbl_id1)
!
      if(iflag_format .eq. iflag_sgl_hdf5) call parallel_init_hdf5
!
      end subroutine init_merged_ucd_element
!
!  ---------------------------------------------------------------------
!
      subroutine finalize_merged_ucd(iflag_format, ucd)
!
      use hdf5_file_IO
!
      integer(kind = kint), intent(in) :: iflag_format
      type(ucd_data), intent(inout) :: ucd
!
!
      if(iflag_format .eq. iflag_sgl_hdf5) call parallel_finalize_hdf5
!
      call dealloc_merged_ucd_nod_stack(ucd)
      call dealloc_merged_ucd_ele_stack(ucd)
!
      end subroutine finalize_merged_ucd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_file_mpi(file_name, ucd)
!
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'UCD data by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_ucd_node_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%ntot_comp, ucd%xx,                              &
     &    ucd%istack_merged_intnod, ucd%istack_merged_ele)
      call write_ucd_connect_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nele, ucd%nnod_4_ele, ucd%ie, ucd%istack_merged_ele)
!
      call write_ucd_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_ucd_file_mpi
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_phys_mpi(file_name, ucd)
!
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
     if(my_rank .eq. 0) then
        write(*,*) 'UCD field by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_ucd_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_ucd_phys_mpi
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_grid_mpi(file_name, ucd)
!
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'UCD grid by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_ucd_node_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%ntot_comp, ucd%xx,                              &
     &    ucd%istack_merged_intnod, ucd%istack_merged_ele)
      call write_ucd_connect_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nele, ucd%nnod_4_ele, ucd%ie, ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_ucd_grid_mpi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_vtk_file_mpi(file_name, ucd)
!
      use vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
     if(my_rank .eq. 0) then
        write(*,*) 'VTK by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    ucd%istack_merged_intnod, ucd%istack_merged_ele)
!
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys_mpi(file_name, ucd)
!
      use vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
     if(my_rank .eq. 0) then
        write(*,*) 'VTK field by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_phys_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid_mpi(file_name, ucd)
!
      use vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
     if(my_rank .eq. 0) then
        write(*,*) 'VTK grid by MPI-IO: ', trim(file_name)
      end if
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    ucd%istack_merged_intnod, ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_grid_mpi
!
! -----------------------------------------------------------------------
!
      end module merged_udt_vtk_file_IO
