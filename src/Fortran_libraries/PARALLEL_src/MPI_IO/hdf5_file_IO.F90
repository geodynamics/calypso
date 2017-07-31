!
!      module hdf5_file_IO
!
!      Written by E. Heien in June 2013
!
!      subroutine parallel_init_hdf5(ucd, m_ucd)
!      subroutine parallel_finalize_hdf5(m_ucd)
!
!!      subroutine parallel_write_hdf5_mesh_file                        &
!!     &         (file_prefix, ucd, m_ucd)
!!      subroutine parallel_write_hdf5_field_file                       &
!!     &        (file_prefix, cur_step, ucd, m_ucd)
!
!!      subroutine parallel_write_xdmf_snap_file                        &
!!     &         (file_prefix, istep_hdf5, t_IO, ucd, m_ucd)
!!      subroutine parallel_write_xdmf_evo_file                         &
!!     &         (file_prefix, istep_hdf5, t_IO, ucd, m_ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!        type(merged_ucd_data), intent(in) :: m_ucd
!
      module hdf5_file_IO
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use calypso_mpi
!
      use t_time_data
      use t_ucd_data
!
      use set_ucd_file_names
      use set_parallel_file_name
!
#ifdef HDF5_IO
      use hdf5
#endif
!
      implicit none
!
      private :: parallel_write_xdmf_file
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine parallel_init_hdf5(ucd, m_ucd)
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
      integer(kind = kint) :: nnod_hdf
      integer :: hdferr
!
! Initialize Fortran interface
!
!
#ifdef HDF5_IO
      nnod_hdf = int(m_ucd%istack_merged_intnod(my_rank+1)              &
     &             - m_ucd%istack_merged_intnod(my_rank)  )
      call alloc_merged_hdt5_array(nnod_hdf, ucd, m_ucd)
      call h5open_f(hdferr)
#endif
!
      end subroutine parallel_init_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine parallel_finalize_hdf5(m_ucd)
!
      type(merged_ucd_data), intent(inout) :: m_ucd
      integer :: hdferr
!
! Close Fortran interface
!
#ifdef HDF5_IO
      call h5close_f(hdferr)
      call dealloc_merged_hdt5_array(m_ucd)
#endif
!
      end subroutine parallel_finalize_hdf5
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_ele_connect_for_hdf5(ucd, m_ucd)
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
      integer(kind = kint) :: k1
      integer(kind = kint_gl) :: iele, i
!
!
!$omp parallel do private(iele,k1,i)
      do iele = 1, ucd%nele
        do k1 = 1, ucd%nnod_4_ele
          i = k1 + (iele-1) * ucd%nnod_4_ele
          m_ucd%ie_hdf5(i) = int(ucd%ie(iele,k1)) - 1
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ele_connect_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_position_for_hdf5(intnod, ucd, m_ucd)
!
      integer(kind = kint), intent(in) :: intnod
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
      integer(kind = kint) :: inod
!
!
      m_ucd%ncomp_hdf5 = 3
!$omp parallel do
      do inod = 1, intnod
        m_ucd%fld_hdf5(3*inod-2) = ucd%xx(inod,1)
        m_ucd%fld_hdf5(3*inod-1) = ucd%xx(inod,2)
        m_ucd%fld_hdf5(3*inod  ) = ucd%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_position_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_scalar_field_for_hdf5(intnod, ist_fld,            &
     &          ucd, m_ucd)
!
      integer(kind = kint), intent(in) :: ist_fld, intnod
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
      integer(kind = kint) :: inod
!
!
      m_ucd%ncomp_hdf5 = 1
!$omp parallel do
      do inod = 1, intnod
        m_ucd%fld_hdf5(inod) = ucd%d_ucd(inod,ist_fld+1)
      end do
!$omp end parallel do
!
      end subroutine copy_scalar_field_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_vector_field_for_hdf5(intnod, ist_fld,            &
     &          ucd, m_ucd)
!
      integer(kind = kint), intent(in) :: ist_fld, intnod
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
      integer(kind = kint) :: inod
!
!
      m_ucd%ncomp_hdf5 = 3
!$omp parallel do
      do inod = 1, intnod
        m_ucd%fld_hdf5(3*inod-2) = ucd%d_ucd(inod,ist_fld+1)
        m_ucd%fld_hdf5(3*inod-1) = ucd%d_ucd(inod,ist_fld+2)
        m_ucd%fld_hdf5(3*inod  ) = ucd%d_ucd(inod,ist_fld+3)
      end do
!$omp end parallel do
!
      end subroutine copy_vector_field_for_hdf5
!
! -----------------------------------------------------------------------
!
      subroutine copy_sym_tensor_field_for_hdf5(intnod, ist_fld,        &
     &          ucd, m_ucd)
!
      integer(kind = kint), intent(in) :: ist_fld, intnod
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
      integer(kind = kint) :: inod
!
!
      m_ucd%ncomp_hdf5 = 9
!$omp parallel do
      do inod = 1, intnod
        m_ucd%fld_hdf5(9*inod-8) = ucd%d_ucd(inod,ist_fld+1)
        m_ucd%fld_hdf5(9*inod-7) = ucd%d_ucd(inod,ist_fld+2)
        m_ucd%fld_hdf5(9*inod-6) = ucd%d_ucd(inod,ist_fld+3)
        m_ucd%fld_hdf5(9*inod-5) = ucd%d_ucd(inod,ist_fld+1)
        m_ucd%fld_hdf5(9*inod-4) = ucd%d_ucd(inod,ist_fld+4)
        m_ucd%fld_hdf5(9*inod-3) = ucd%d_ucd(inod,ist_fld+5)
        m_ucd%fld_hdf5(9*inod-2) = ucd%d_ucd(inod,ist_fld+3)
        m_ucd%fld_hdf5(9*inod-1) = ucd%d_ucd(inod,ist_fld+5)
        m_ucd%fld_hdf5(9*inod  ) = ucd%d_ucd(inod,ist_fld+6)
      end do
!$omp end parallel do
!
      end subroutine copy_sym_tensor_field_for_hdf5
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine parallel_write_hdf5_mesh_file                          &
     &         (file_prefix, ucd, m_ucd)
!
      character(len = kchara), intent(in) :: file_prefix
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
#ifdef HDF5_IO
      character(len=kchara), parameter                                  &
     &                      :: node_dataset_name = "nodes"
      character(len=kchara), parameter                                  &
     &                      :: elem_dataset_name = "elements"
!
      integer(kind = kint) :: nnod
      character(len = kchara) :: file_name
      integer(hid_t) :: id_hdf5
      integer(hid_t) :: plist_id      ! Property list identifier
      integer :: info
      integer :: dataspace_dims
      integer(hsize_t) :: node_dataspace_dim(2), elem_dataspace_dim(2)
      integer(hid_t) :: node_dataspace_id, elem_dataspace_id
      integer(hid_t) :: node_dataset_id, elem_dataset_id
      integer(hsize_t) :: hyperslab_size(2), hyperslab_offset(2)
      integer(hid_t) :: node_memory_dataspace, elem_memory_dataspace
      integer(hid_t) :: node_file_dataspace, elem_file_dataspace
      integer(hsize_t) :: buf_dims(2)
      integer :: hdferr
!
!
      nnod = int(m_ucd%istack_merged_intnod(my_rank+1)                  &
     &         - m_ucd%istack_merged_intnod(my_rank)  )
!
      call set_merged_hdf_mesh_file_name(file_prefix, file_name)
!
! Remove our own counts from the offset
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
          hdferr, access_prp = plist_id)
      call h5pclose_f(plist_id, hdferr)
!
!
! We first have to transpose the data so it fits correctly to the XDMF format
! We also take this opportunity to use the "real" (C-like) indexing
!
        call copy_node_position_for_hdf5(nnod, ucd, m_ucd)
        call copy_ele_connect_for_hdf5(ucd, m_ucd)
!
! Node data set dimensions are (number of nodes) x 3
!
        dataspace_dims = 2
        node_dataspace_dim(1) = m_ucd%ncomp_hdf5
        node_dataspace_dim(2) = m_ucd%istack_merged_intnod(nprocs)
        call h5screate_simple_f(dataspace_dims, node_dataspace_dim,     &
            node_dataspace_id, hdferr)
!
! Element data set dimensions are (number of elements) x (nodes per element)
!
        dataspace_dims = 2
        elem_dataspace_dim(1) = ucd%nnod_4_ele
        elem_dataspace_dim(2) = m_ucd%istack_merged_ele(nprocs)
        call h5screate_simple_f(dataspace_dims, elem_dataspace_dim,     &
            elem_dataspace_id, hdferr)
!
! Create the datasets in the file
!
        call h5dcreate_f(id_hdf5, node_dataset_name, H5T_NATIVE_DOUBLE, &
            node_dataspace_id, node_dataset_id, hdferr)
        call h5dcreate_f(id_hdf5, elem_dataset_name,                    &
            H5T_NATIVE_INTEGER, elem_dataspace_id, elem_dataset_id,     &
     &      hdferr)
!
! We can close the created dataspaces now
!
        call h5sclose_f(node_dataspace_id, hdferr)
        call h5sclose_f(elem_dataspace_id, hdferr)
!
! Create dataspace for memory hyperslab for nodes
!
        hyperslab_size(1) =   3
        hyperslab_size(2) = nnod
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = m_ucd%istack_merged_intnod(my_rank)
        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            node_memory_dataspace, hdferr)
!
! Create dataspace for file hyperslab for nodes
!
        call h5dget_space_f(node_dataset_id, node_file_dataspace,       &
     &      hdferr)
        call h5sselect_hyperslab_f(node_file_dataspace,                 &
     &      H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! And similarly create file/memory hyperslabs for elements
!
        hyperslab_size(1) = ucd%nnod_4_ele
        hyperslab_size(2) = ucd%nele
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = m_ucd%istack_merged_ele(my_rank)

        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            elem_memory_dataspace, hdferr)
!
        call h5dget_space_f(elem_dataset_id, elem_file_dataspace,       &
     &      hdferr)
        call h5sselect_hyperslab_f(elem_file_dataspace,                 &
            H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! Create a property list for collective write
!
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, hdferr)
!
! Write the node data (finally!)
!
        buf_dims(1) = 3
        buf_dims(2) = nnod
        call h5dwrite_f(node_dataset_id, H5T_NATIVE_DOUBLE,             &
            m_ucd%fld_hdf5(1), buf_dims, hdferr, node_memory_dataspace, &
            node_file_dataspace, plist_id)
!
! And the element data
!
        buf_dims(1) = ucd%nnod_4_ele
        buf_dims(2) = ucd%nele
        call h5dwrite_f(elem_dataset_id, H5T_NATIVE_INTEGER,            &
            m_ucd%ie_hdf5(1), buf_dims, hdferr, elem_memory_dataspace,  &
            elem_file_dataspace, plist_id)
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
        call dealloc_merged_hdt5_ele_list(m_ucd)
!
! Close file
!
        call h5fclose_f(id_hdf5, hdferr)
#endif
!
      end subroutine parallel_write_hdf5_mesh_file
!
! -----------------------------------------------------------------------
!
      subroutine parallel_write_hdf5_field_file                         &
     &        (file_prefix, cur_step, ucd, m_ucd)
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind=kint), intent(in) :: cur_step
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
#ifdef HDF5_IO
      integer(kind = kint) :: istep, icou, nnod
      character(len = kchara) :: file_name
      integer(hid_t) :: id_hdf5
      integer(hid_t) :: plist_id      ! Property list identifier
      integer(hid_t) :: field_dataspace_id
      integer(hid_t) :: field_dataset_id
      integer(hid_t) :: field_file_dataspace, field_memory_dataspace
      integer :: info
      integer(hsize_t) :: field_dataspace_dim(2)
      integer :: dataspace_dims
      integer :: hdferr
      integer(hsize_t) :: buf_dims(2)
      integer(hsize_t) :: hyperslab_size(2), hyperslab_offset(2)
!
! Setup the filename
!
      call set_merged_hdf_field_file_name                               &
     &   (file_prefix, cur_step, file_name)
!
! Progress update
!
      if (my_rank .eq. 0) write(*,*) 'merged HDF5 field data: ',        &
     &                   trim(file_name)
! Remove our own counts from the offset
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
        hdferr, access_prp = plist_id)
      call h5pclose_f(plist_id, hdferr)
!
! Go through each of the fields
!
      icou = 0
      do istep = 1, ucd%num_field, 1
        nnod = int(m_ucd%istack_merged_intnod(my_rank+1)                &
     &           - m_ucd%istack_merged_intnod(my_rank)  )
!
! Transpose the field data to fit XDMF standards
!
        if(ucd%num_comp(istep) .eq. n_scalar) then
          call copy_scalar_field_for_hdf5(nnod, icou, ucd, m_ucd)
        else if(ucd%num_comp(istep) .eq. n_vector) then
          call copy_vector_field_for_hdf5(nnod, icou, ucd, m_ucd)
        else if(ucd%num_comp(istep) .eq. 6) then
          call copy_sym_tensor_field_for_hdf5(nnod, icou, ucd, m_ucd)
        end if
        icou = icou + ucd%num_comp(istep)
!
! Create a dataspace of the appropriate size
!
        dataspace_dims = 2
        field_dataspace_dim(1) = m_ucd%ncomp_hdf5
        field_dataspace_dim(2) = m_ucd%istack_merged_intnod(nprocs)
        call h5screate_simple_f(dataspace_dims, field_dataspace_dim,    &
            field_dataspace_id, hdferr)

!
! Create the dataset for the field
!
        call h5dcreate_f(id_hdf5, trim(ucd%phys_name(istep)),           &
     &      H5T_NATIVE_DOUBLE,  &
            field_dataspace_id, field_dataset_id, hdferr)
!
! Now we can close the dataspace
!
        call h5sclose_f(field_dataspace_id, hdferr)
!
! Create dataspace for memory hyperslab
!
        hyperslab_size(1) = m_ucd%ncomp_hdf5
        hyperslab_size(2) = nnod
        hyperslab_offset(1) = 0
        hyperslab_offset(2) = m_ucd%istack_merged_intnod(my_rank)
        call h5screate_simple_f(dataspace_dims, hyperslab_size,         &
            field_memory_dataspace, hdferr)
!
! Create dataspace for file hyperslab
!
        call h5dget_space_f(field_dataset_id, field_file_dataspace,     &
     &      hdferr)
        call h5sselect_hyperslab_f(field_file_dataspace,                &
            H5S_SELECT_SET_F, hyperslab_offset, hyperslab_size, hdferr)
!
! Create a property list for collective write
!
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F,       &
     &      hdferr)
!
! Write the field data
!
        buf_dims(1) = m_ucd%ncomp_hdf5
        buf_dims(2) = nnod
        call h5dwrite_f(field_dataset_id, H5T_NATIVE_DOUBLE,            &
            m_ucd%fld_hdf5(1), buf_dims, hdferr,                        &
            field_memory_dataspace, field_file_dataspace, plist_id)
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
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_snap_file                          &
     &         (file_prefix, istep_hdf5, t_IO, ucd, m_ucd)
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind=kint), intent(in) :: istep_hdf5
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len = kchara) :: xdmf_dir_file
!
! Only write the file on process 0
      if (my_rank .ne. 0) return
!
! Get the XDMF file location/name
      call set_merged_snap_xdmf_file_name                               &
     &   (file_prefix, istep_hdf5, xdmf_dir_file)
! Open the XDMF file to append
      call parallel_write_xdmf_file(file_prefix, xdmf_dir_file,         &
     &    istep_hdf5, t_IO, ucd, m_ucd)
!
      end subroutine parallel_write_xdmf_snap_file
!
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_evo_file                           &
     &         (file_prefix, istep_hdf5, t_IO, ucd, m_ucd)
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind=kint), intent(in) :: istep_hdf5
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len = kchara) :: xdmf_dir_file
!
! Only write the file on process 0
      if (my_rank .ne. 0) return
!
! Get the XDMF file location/name
      call set_merged_xdmf_file_name(file_prefix, xdmf_dir_file)
! Open the XDMF file to append
      call parallel_write_xdmf_file(file_prefix, xdmf_dir_file,         &
     &    istep_hdf5, t_IO, ucd, m_ucd)
!
      end subroutine parallel_write_xdmf_evo_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine parallel_write_xdmf_file(file_prefix, xdmf_dir_file,   &
     &          istep_hdf5, t_IO, ucd, m_ucd)
!
      character(len = kchara), intent(in) :: file_prefix
      character(len = kchara), intent(in) :: xdmf_dir_file
      integer(kind=kint), intent(in) :: istep_hdf5
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len = kchara) :: mesh_dir_file, mesh_file_name
      character(len = kchara) :: field_dir_file, field_file_name
      character(len = kchara) :: bite_str, dim_str
      character(len = kchara) :: node_str, elem_str
      character(len = kchara) :: time_str, attr_str, tmp_str
      integer(kind=kint), parameter :: id_xdmf = 14
      integer(kind=kint) :: i_adjusted
      integer(kind=kint) :: istep
!
! Only write the file on process 0
      if (my_rank .ne. 0) return
!
      i_adjusted = kreal
      call int_to_str(i_adjusted, bite_str)
      call int_to_str(ithree, dim_str)
      call lint_to_str(m_ucd%istack_merged_intnod(nprocs), node_str)
      call lint_to_str(m_ucd%istack_merged_ele(nprocs), elem_str)
!
! Open the XDMF file to append
      open(id_xdmf, file=xdmf_dir_file, status='old',                   &
     &      position='append', err = 99)
! Back 2 lines
      backspace(id_xdmf)
      backspace(id_xdmf)
      go to 10
!
! Create new XDMF file
  99  continue
      open(id_xdmf, file=xdmf_dir_file, status='replace')
      write(id_xdmf, '(a)') '<?xml version="1.0" ?>'
      write(id_xdmf, '(a)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
      write(id_xdmf, '(a)') '<Xdmf Version="2.0">'
      write(id_xdmf, '(a)') '  <Domain>'
!
  10  continue
!
!
!   Get the mesh file name
      call set_merged_hdf_mesh_file_name(file_prefix, mesh_dir_file)
      call delete_directory_name(mesh_dir_file, mesh_file_name)
!
!  Append field entry
      call real_to_str(t_IO%time, time_str)
      call set_merged_hdf_field_file_name                               &
     &   (file_prefix, istep_hdf5, field_dir_file)
      call delete_directory_name(field_dir_file, field_file_name)
      write(id_xdmf, '(2a)')                                            &
     &         '    <Grid Name="CellTime" GridType="Collection" ',      &
     &         'CollectionType="Temporal">'
      write(id_xdmf, '(a)')                                             &
     &         '      <Grid Name="mesh" GridType="Uniform">'
      write(id_xdmf, '(3a)')                                            &
     &         '        <Time Value="', trim(time_str), '"/>'
      write(id_xdmf, '(a)')                                             &
     &         '        <Geometry GeometryType="XYZ">'
      write(id_xdmf, '(7a)')                                            &
     &         '          <DataItem Dimensions="',                      &
     &         trim(node_str), ' ', trim(dim_str),                      &
     &         '" NumberType="Float" Precision="', trim(bite_str),      &
     &         '" Format="HDF">'
      write(id_xdmf, '(3a)')                                            &
     &         '            ', trim(mesh_file_name), ':/nodes'
      write(id_xdmf, '(a)') '          </DataItem>'
      write(id_xdmf, '(a)') '        </Geometry>'
      write(id_xdmf, '(4a)')                                            &
     &         '        <Topology TopologyType="Hexahedron" ',          &
               'NumberOfElements="', trim(elem_str), '">'
!
      call int_to_str(ucd%nnod_4_ele, tmp_str)
      write(id_xdmf, '(5a)')                                            &
     &         '          <DataItem Dimensions="', trim(elem_str),      &
     &         ' ', trim(tmp_str), '" NumberType="UInt" Format="HDF">'
      write(id_xdmf, '(3a)')                                            &
     &         '            ', trim(mesh_file_name), ':/elements'
      write(id_xdmf, '(a)') '          </DataItem>'
      write(id_xdmf, '(a)') '        </Topology>'
!
      do istep = 1, ucd%num_field, 1
        if (ucd%num_comp(istep) .eq. 1) then
          attr_str = "Scalar"
        else if (ucd%num_comp(istep) .eq. 3) then
          attr_str = "Vector"
        else if (ucd%num_comp(istep) .eq. 6) then
          attr_str = "Tensor"
        end if
!
        write(id_xdmf, '(5a)')                                          &
     &         '        <Attribute Name="', trim(ucd%phys_name(istep)), &
     &         '" AttributeType="', trim(attr_str), '" Center="Node">'
        call int_to_str(ucd%num_comp(istep), tmp_str)
        write(id_xdmf, '(8a)')                                          &
     &              '          <DataItem Dimensions="', trim(node_str), &
     &              ' ', trim(tmp_str), '" NumberType="Float" ',        &
                    'Precision="', trim(bite_str), '" Format="HDF">'
        write(id_xdmf,'(4a)') '            ',                           &
     &         trim(field_file_name), ':/', trim(ucd%phys_name(istep))
        write(id_xdmf, '(a)') '          </DataItem>'
        write(id_xdmf, '(a)') '        </Attribute>'
      end do
      write(id_xdmf, '(a)')     '      </Grid>'
      write(id_xdmf, '(a)')     '    </Grid>'
!
      write(id_xdmf, '(a)')         '  </Domain>'
      write(id_xdmf, '(a)')         '</Xdmf>'
      close(id_xdmf)
!
      end subroutine parallel_write_xdmf_file
!
! ----------------------------------------------------------------------
!
      end module hdf5_file_IO
