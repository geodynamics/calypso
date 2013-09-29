!>@file   hdf5_file_IO.f90
!!@brief  module hdf5_file_IO
!!
!!@author E. Heien
!!@date Written by E. Heien in June 2013
!
!>@brief  Thermal equation parameters to read
!!
!!@verbatim
!!      subroutine parallel_init_hdf5()
!!      subroutine parallel_finalize_hdf5()
!!      subroutine parallel_write_hdf5_mesh_file(file_prefix)
!!      subroutine parallel_write_hdf5_field_file(file_prefix, cur_step)
!!      subroutine parallel_write_xdmf_file(file_prefix, cur_vis_step,  &
!!     &          cur_simulation_time)
!!@endverbatim
!!
!!@param file_prefix            File prefix for XDMF files
!!@param cur_step               time step
!!@param cur_vis_step           stem number for field data output
!!@param cur_simulation_time    time
!
      module hdf5_file_IO
!
      use m_precision
      use calypso_mpi
      use m_constants
      use m_ucd_data
      use m_phys_constants
      use m_merged_ucd_data
      use set_ucd_data
      use set_ucd_file_names
      use set_parallel_file_name
#ifdef HDF5_IO
      use hdf5
#endif
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine parallel_init_hdf5()
!
#ifdef HDF5_IO
      integer :: hdferr
!
! Initialize Fortran interface
!
      call h5open_f(hdferr)
#endif
!
      end subroutine parallel_init_hdf5
!
!
! -----------------------------------------------------------------------
!
      subroutine parallel_finalize_hdf5()
!
#ifdef HDF5_IO
      integer :: hdferr
!
! Close Fortran interface
!
      call h5close_f(hdferr)
!
#endif
      end subroutine parallel_finalize_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine parallel_write_hdf5_mesh_file(file_prefix)
!
      character(len = kchara), intent(in) :: file_prefix
#ifdef HDF5_IO
!
      character(len = kchara) :: file_name
      integer(hid_t) :: id_hdf5
      integer(hid_t) :: plist_id      ! Property list identifier
      integer :: info
      integer :: mpi_err
      integer(kind=kint) :: local_node_elem_count(2)
      integer(kind=kint) :: start_node_elem(2)
      integer(kind=kint) :: total_node_elem(2)
      integer :: dataspace_dims
      integer(hsize_t) :: node_dataspace_dim(2), elem_dataspace_dim(2)
      integer(hid_t) :: node_dataspace_id, elem_dataspace_id
      character(len=kchara) :: node_dataset_name, elem_dataset_name
      integer(hid_t) :: node_dataset_id, elem_dataset_id
      integer(hsize_t) :: hyperslab_size(2), hyperslab_offset(2)
      integer(hid_t) :: node_memory_dataspace, elem_memory_dataspace
      integer(hid_t) :: node_file_dataspace, elem_file_dataspace
      integer(hsize_t) :: buf_dims(2)
      real(kind=kreal), allocatable ::   transpose_nodes(:,:)
      integer(kind=kint), allocatable :: transpose_elems(:,:)
      integer(kind=kint) :: istep, nstep
      integer :: hdferr
!
!
      call set_merged_hdf_mesh_file_name(file_prefix, file_name)
!
! Determine the global starting node and element index for this process
! Also calculate total number of nodes and elements
!
      local_node_elem_count(1) = istack_internod_ucd_list(my_rank+1)    &
     &                          - istack_internod_ucd_list(my_rank)
      local_node_elem_count(2) = nele_ucd
!
!        write(*,*) 'start vals: ', local_node_elem_count(1), &
!        local_node_elem_count(2)
      call mpi_scan(local_node_elem_count, start_node_elem,             &
     &    2, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, mpi_err)
      total_node_elem(1) = istack_internod_ucd_list(nprocs)
      total_node_elem(2) = istack_ele_ucd_list(nprocs)
!
! Remove our own counts from the offset
!
      start_node_elem(1) = start_node_elem(1) - local_node_elem_count(1)
      start_node_elem(2) = start_node_elem(2) - local_node_elem_count(2)
!
      write(*,*) 'vals: ', start_node_elem(1), start_node_elem(2)
      write(*,*) 'totals: ', total_node_elem(1), total_node_elem(2)
      info = MPI_INFO_NULL
!
! Setup file access property list with parallel I/O access.
!
      call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
      call h5pset_fapl_mpio_f(plist_id, CALYPSO_COMM, info, hdferr)
!
! Create new file collectively.
!
      call h5fcreate_f(file_name, H5F_ACC_TRUNC_F, id_hdf5,             &
     &    hdferr, access_prp = plist_id)
      call h5pclose_f(plist_id, hdferr)
!
! We first have to transpose the data so it fits correctly to the XDMF format
! We also take this opportunity to use the "real" (C-like) indexing
!
      allocate(transpose_nodes(3, local_node_elem_count(1)))
      allocate(transpose_elems(nnod_4_ele_ucd, local_node_elem_count(2)))
      do istep = 1, local_node_elem_count(1), 1
        do nstep = 1, 3, 1
          transpose_nodes(nstep, istep) = xx_ucd(istep, nstep)
        end do
      end do
      do istep = 1, local_node_elem_count(2), 1
        do nstep = 1, nnod_4_ele_ucd, 1
          transpose_elems(nstep, istep) = ie_ucd(istep, nstep)-1
        end do
      end do
!
! Node data set dimensions are (number of nodes) x 3
!
      dataspace_dims = 2
      node_dataspace_dim(1) = 3
      node_dataspace_dim(2) = total_node_elem(1)
      call h5screate_simple_f(dataspace_dims, node_dataspace_dim,       &
     &    node_dataspace_id, hdferr)
!
! Element data set dimensions are (number of elements) x (nodes per element)
!
      dataspace_dims = 2
      elem_dataspace_dim(1) = nnod_4_ele_ucd
      elem_dataspace_dim(2) = total_node_elem(2)
      call h5screate_simple_f(dataspace_dims, elem_dataspace_dim,       &
     &    elem_dataspace_id, hdferr)
!
! Create the datasets in the file
!
      node_dataset_name = "nodes"
      elem_dataset_name = "elements"
      call h5dcreate_f(id_hdf5, node_dataset_name, H5T_NATIVE_DOUBLE,   &
     &    node_dataspace_id, node_dataset_id, hdferr)
      call h5dcreate_f(id_hdf5, elem_dataset_name, H5T_NATIVE_INTEGER,  &
     &    elem_dataspace_id, elem_dataset_id, hdferr)
!
! We can close the created dataspaces now
!
      call h5sclose_f(node_dataspace_id, hdferr)
      call h5sclose_f(elem_dataspace_id, hdferr)
!
! Create dataspace for memory hyperslab for nodes
!
      hyperslab_size(1) = 3
      hyperslab_size(2) = local_node_elem_count(1)
      hyperslab_offset(1) = 0
      hyperslab_offset(2) = start_node_elem(1)
      call h5screate_simple_f(dataspace_dims, hyperslab_size,           &
     &    node_memory_dataspace, hdferr)
!
! Create dataspace for file hyperslab for nodes
!
      call h5dget_space_f(node_dataset_id, node_file_dataspace, hdferr)
      call h5sselect_hyperslab_f(node_file_dataspace, H5S_SELECT_SET_F, &
     &    hyperslab_offset, hyperslab_size, hdferr)
!
! And similarly create file/memory hyperslabs for elements
!
      hyperslab_size(1) = nnod_4_ele_ucd
      hyperslab_size(2) = local_node_elem_count(2)
      hyperslab_offset(1) = 0
      hyperslab_offset(2) = start_node_elem(2)

      call h5screate_simple_f(dataspace_dims, hyperslab_size,           &
     &    elem_memory_dataspace, hdferr)
!
      call h5dget_space_f(elem_dataset_id, elem_file_dataspace,         &
     &      hdferr)
      call h5sselect_hyperslab_f(elem_file_dataspace,                   &
     &      H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! Create a property list for collective write
!
      call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
      call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F,         &
     &    hdferr)
!
! Write the node data (finally!)
!
      buf_dims(1) = 3
      buf_dims(2) = local_node_elem_count(1)
      call h5dwrite_f(node_dataset_id, H5T_NATIVE_DOUBLE,               &
     &    transpose_nodes, buf_dims, hdferr, node_memory_dataspace,     &
     &    node_file_dataspace, plist_id)
!
! And the element data
!
      buf_dims(1) = nnod_4_ele_ucd
      buf_dims(2) = local_node_elem_count(2)
      call h5dwrite_f(elem_dataset_id, H5T_NATIVE_INTEGER,              &
     &    transpose_elems, buf_dims, hdferr, elem_memory_dataspace,     &
     &    elem_file_dataspace, plist_id)
!
! Close parallel access property list
!
      call h5pclose_f(plist_id, hdferr)
!
! Close the memory and file dataspaces
!
      call h5sclose_f(node_memory_dataspace, hdferr)
      call h5sclose_f(elem_memory_dataspace, hdferr)
      call h5sclose_f(node_file_dataspace, hdferr)
      call h5sclose_f(elem_file_dataspace, hdferr)
!
! Finally close the datasets
!
      call h5dclose_f(node_dataset_id, hdferr)
      call h5dclose_f(elem_dataset_id, hdferr)
!
! Deallocate transpose arrays
!
      deallocate(transpose_nodes)
      deallocate(transpose_elems)
!
! Close file
!
      call h5fclose_f(id_hdf5, hdferr)
!
#endif
      end subroutine parallel_write_hdf5_mesh_file
!
! -----------------------------------------------------------------------
!
      subroutine parallel_write_hdf5_field_file(file_prefix, cur_step)
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind=kint), intent(in) :: cur_step
#ifdef HDF5_IO
      character(len = kchara) :: file_name
      integer(hid_t) :: id_hdf5
      integer(hid_t) :: plist_id        ! Property list identifier
      integer(hid_t) :: field_dataspace_id
      integer(hid_t) :: field_dataset_id
      integer(hid_t) :: field_file_dataspace, field_memory_dataspace
      integer :: info
      integer :: mpi_err
      integer :: istep, jstep, nstep
      integer(kind=kint) :: local_node_count
      integer(kind=kint) :: start_node
      integer(kind=kint) :: total_node
      integer(hsize_t) :: field_dataspace_dim(2)
      integer :: dataspace_dims
      integer :: hdferr
      integer(hsize_t) :: buf_dims(2)
      integer(kind=kint) :: cur_field_offset
      integer(hsize_t) :: hyperslab_size(2), hyperslab_offset(2)
      real(kind=kreal), allocatable :: transpose_field(:,:)
!
! Setup the filename
!
      call set_merged_hdf_field_file_name(file_prefix, cur_step,        &
     &    file_name)
!
! Progress update
!
      if (my_rank .eq. 0) write(*,*) 'merged HDF5 field data: ',        &
     &                               trim(file_name)
!
! Determine the global starting node index for this process
! Also calculate total number of nodes
!
      local_node_count = istack_internod_ucd_list(my_rank+1)            &
     &                  - istack_internod_ucd_list(my_rank)
      call mpi_scan(local_node_count, start_node,                       &
     &        1, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, mpi_err)
      total_node = istack_internod_ucd_list(nprocs)
!
! Remove our own counts from the offset
!
      start_node = start_node - local_node_count
!
      info = MPI_INFO_NULL
!
! Setup file access property list with parallel I/O access.
!
      call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
      call h5pset_fapl_mpio_f(plist_id, CALYPSO_COMM, info, hdferr)
!
! Create new file collectively.
!
      call h5fcreate_f(file_name, H5F_ACC_TRUNC_F, id_hdf5,             &
     &    hdferr, access_prp = plist_id)
      call h5pclose_f(plist_id, hdferr)
!
! Go through each of the fields
!
      cur_field_offset = 0
      do istep = 1, num_field_ucd, 1
!
! Transpose the field data to fit XDMF standards
!
        allocate(transpose_field(num_comp_ucd(istep), local_node_count))
        do jstep = 1, local_node_count, 1
          do nstep = 1, num_comp_ucd(istep), 1
            transpose_field(nstep, jstep) = d_nod_ucd(jstep,            &
                                           cur_field_offset+nstep)
          end do
        end do
!
! Create a dataspace of the appropriate size
!
        dataspace_dims = 2
        field_dataspace_dim(1) = num_comp_ucd(istep)
        field_dataspace_dim(2) = total_node
        call h5screate_simple_f(dataspace_dims, field_dataspace_dim,    &
            field_dataspace_id, hdferr)
!
! Create the dataset for the field
!
        call h5dcreate_f(id_hdf5, trim(phys_name_ucd(istep)),           &
     &      H5T_NATIVE_DOUBLE, field_dataspace_id, field_dataset_id,    &
     &      hdferr)
!
! Now we can close the dataspace
!
        call h5sclose_f(field_dataspace_id, hdferr)
!
! Create dataspace for memory hyperslab
!
        hyperslab_size(1) = num_comp_ucd(istep)
        hyperslab_size(2) = local_node_count
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = start_node
        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            field_memory_dataspace, hdferr)
!
! Create dataspace for file hyperslab
!
        call h5dget_space_f(field_dataset_id, field_file_dataspace,     &
     &      hdferr)
        call h5sselect_hyperslab_f(field_file_dataspace,                &
     &      H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! Create a property list for collective write
!
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, hdferr)
!
! Write the field data
!
        buf_dims(1) = num_comp_ucd(istep)
        buf_dims(2) = local_node_count
        call h5dwrite_f(field_dataset_id, H5T_NATIVE_DOUBLE,            &
            transpose_field, buf_dims, hdferr, field_memory_dataspace,  &
            field_file_dataspace, plist_id)
!
! Close parallel access property list
!
        call h5pclose_f(plist_id, hdferr)
!
! Close the memory and file dataspaces
!
        call h5sclose_f(field_memory_dataspace, hdferr)
        call h5sclose_f(field_file_dataspace, hdferr)
!
! Close the field dataset
!
        call h5dclose_f(field_dataset_id, hdferr)
!
        deallocate(transpose_field)
        cur_field_offset = cur_field_offset + num_comp_ucd(istep)
      end do
!
! Close file
!
      call h5fclose_f(id_hdf5, hdferr)
#endif
!
      end subroutine parallel_write_hdf5_field_file
!
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_file(file_prefix, cur_vis_step,    &
     &          cur_simulation_time)
!
      use m_t_step_parameter
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind=kint), intent(in) :: cur_vis_step
      real(kind=kreal), intent(in) :: cur_simulation_time
#ifdef HDF5_IO
      character(len = kchara) :: field_name
      character(len = kchara) :: xdmf_dir_file
      character(len = kchara) :: mesh_dir_file, mesh_file_name
      character(len = kchara) :: field_dir_file, field_file_name
      integer(kind=kint) :: total_nodes, total_elements
      character(len = kchara) :: node_str, elem_str, time_str
      character(len = kchara) :: attr_str, tmp_str
      integer(kind=kint), parameter :: id_xdmf = 14
      integer :: istep
!
! Only write the file on process 0
!
      if (my_rank .gt. 0) return
!
      total_nodes = istack_internod_ucd_list(nprocs)
      total_elements = istack_ele_ucd_list(nprocs)
      call int_to_str(total_nodes, node_str)
      call int_to_str(total_elements, elem_str)
!
!Get the XDMF file location/name
      call set_merged_xdmf_file_name(file_prefix, xdmf_dir_file)
!
! If this is the simulation start, overwrite the old file
      if (cur_vis_step .eq. 1) go to 99
!
! Open the XDMF file to append

      open(id_xdmf, file=xdmf_dir_file, status='old',                   &
     &      position='append', err = 99)
! Back 2 lines
      backspace(id_xdmf)
      backspace(id_xdmf)
      go to 10
!
!
! Create new XDMF file
  99  continue
      open(id_xdmf, file=xdmf_dir_file)
      write(id_xdmf, '(a)') '<?xml version="1.0" ?>'
      write(id_xdmf, *) '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
      write(id_xdmf, *) '<Xdmf Version="2.0">'
      write(id_xdmf, *) '  <Domain>'
!
10  continue
!
!   Get the mesh file name
      call set_merged_hdf_mesh_file_name(file_prefix, mesh_dir_file)
      call delete_directory_name(mesh_dir_file, mesh_file_name)
!
!  Append field entry
      call real_to_str(cur_simulation_time, time_str)
      call set_merged_hdf_field_file_name(file_prefix, cur_vis_step,    &
     &    field_dir_file)
      call delete_directory_name(field_dir_file, field_file_name)
      write(id_xdmf, *)                                                 &
     &    '    <Grid Name="CellTime" GridType="Collection" ',           &
     &    'CollectionType="Temporal">'
      write(id_xdmf, *) '      <Grid Name="mesh" GridType="Uniform">'
      write(id_xdmf, *) '        <Time Value="', trim(time_str), '"/>'
      write(id_xdmf, *) '        <Geometry GeometryType="XYZ">'
      write(id_xdmf, *)                                                 &
     &    '          <DataItem Dimensions="', trim(node_str),           &
     &    ' 3" NumberType="Float" Precision="8" Format="HDF">'
      write(id_xdmf, *) '            ', trim(mesh_file_name), ':/nodes'
      write(id_xdmf, *) '          </DataItem>'
      write(id_xdmf, *) '        </Geometry>'
      write(id_xdmf, *) '        <Topology TopologyType="Hexahedron" ', &
     &                  'NumberOfElements="', trim(elem_str), '">'
      call int_to_str(nnod_4_ele_ucd, tmp_str)
      write(id_xdmf, *) '          <DataItem Dimensions="',             &
     &                  trim(elem_str), ' ', trim(tmp_str),             &
     &                  '" NumberType="UInt" Format="HDF">'
      write(id_xdmf, *) '            ', trim(mesh_file_name),           &
     &                  ':/elements'
      write(id_xdmf, *) '          </DataItem>'
      write(id_xdmf, *) '        </Topology>'
!
      do istep = 1, num_field_ucd, 1
!
        field_name = phys_name_ucd(istep)
!
        if (num_comp_ucd(istep) .eq. n_sym_tensor) then
          attr_str = "tensor6"
        else if (num_comp_ucd(istep) .eq. n_vector) then
          attr_str = "Vector"
        else
          attr_str = "Scalar"
        end if
!
        write(id_xdmf, *) '        <Attribute Name="',                  &
     &                    trim(field_name), '" AttributeType="',        &
     &                    trim(attr_str), '" Center="Node">'
        call int_to_str(num_comp_ucd(istep), tmp_str)
        write(id_xdmf, *) '          <DataItem Dimensions="',           &
     &                   trim(node_str), ' ', trim(tmp_str),            &
     &                   '" NumberType="Float" ',                       &
     &                   'Precision="8" Format="HDF">'
        write(id_xdmf, *) '            ', trim(field_file_name),        &
     &                   ':/', trim(field_name)
        write(id_xdmf, *) '          </DataItem>'
        write(id_xdmf, *) '        </Attribute>'
      end do
!
      write(id_xdmf, *) '      </Grid>'
      write(id_xdmf, *) '    </Grid>'
!
      write(id_xdmf, *) '  </Domain>'
      write(id_xdmf, *) '</Xdmf>'
      close(id_xdmf)
#endif
!
      end subroutine parallel_write_xdmf_file
!
! ----------------------------------------------------------------------
!
      end module hdf5_file_IO
