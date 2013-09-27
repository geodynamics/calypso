!merged_udt_vtk_file_IO.f90
!------- module merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine init_merged_ucd
!      subroutine finalize_merged_ucd
!
!      subroutine write_merged_ucd_file(istep)
!      subroutine write_merged_udt_file(istep)
!      subroutine write_merged_grd_file
!
!      subroutine write_merged_vtk_file(istep)
!      subroutine write_merged_vtk_phys(istep)
!      subroutine write_merged_vtk_grid
!
      module merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_ucd_data
      use m_field_file_format
      use m_merged_ucd_data
      use set_ucd_file_names
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_merged_ucd
!
      use m_geometry_parameter
      use m_nod_comm_table
      use m_merged_ucd_data
      use hdf5_file_IO
!
!
      call allocate_merged_ucd_num
      call count_merged_ucd(numnod, internal_node, nele_ucd)
!
      call allocate_merged_ucd_data(numnod,                             &
     &    nnod_4_ele_ucd, ntot_comp_ucd)
      call set_node_double_address                                      &
     &         (num_neib, id_neib, istack_import, item_import,          &
     &          istack_export, item_export)
!
      call update_ele_by_double_address(nele_ucd, nnod_4_ele_ucd,       &
     &    ie_ucd)
!
      if (itype_ucd_data_file .eq. iflag_sgl_hdf5) then
        call parallel_init_hdf5
      end if
!
      end subroutine init_merged_ucd
!
!  ---------------------------------------------------------------------
!
      subroutine finalize_merged_ucd
!
      use m_merged_ucd_data
      use hdf5_file_IO
!
!
      if (itype_ucd_data_file .eq. iflag_sgl_hdf5) then
        call parallel_finalize_hdf5()
      end if
      call deallocate_merged_ucd_data
!
      end subroutine finalize_merged_ucd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_merged_ucd_file(istep)
!
      use merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd_header_name, iflag_ucd,       &
     &      istep, file_name)
!
        write(*,*) 'single UCD data: ', trim(file_name)
        open(ucd_file_code,file=ucd_file_name, form='formatted')
      end if
!
      call write_merged_ucd_mesh(ucd_file_code, nnod_ucd, nele_ucd,     &
     &    nnod_4_ele_ucd, xx_ucd, ie_ucd, ntot_comp_ucd)
!
      call write_merged_ucd_fields(ucd_file_code, nnod_ucd,             &
     &    num_field_ucd, ntot_comp_ucd, num_comp_ucd, phys_name_ucd,    &
     &    d_nod_ucd)
!
      if(my_rank .eq. 0) close(ucd_file_code)
!
      end subroutine write_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_udt_file(istep)
!
      use merged_ucd_data_IO
!
      integer(kind = kint), intent(in) ::  istep
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd_header_name, iflag_udt,       &
     &      istep, file_name)
!
        write(*,*) 'single UCD field data: ', trim(file_name)
        open(ucd_file_code,file=ucd_file_name, form='formatted')
      end if
!
      call write_merged_ucd_fields(ucd_file_code, nnod_ucd,             &
     &    num_field_ucd, ntot_comp_ucd, num_comp_ucd, phys_name_ucd,    &
     &    d_nod_ucd)
!
      if(my_rank .eq. 0) close(ucd_file_code)
!
      end subroutine write_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_grd_file
!
      use merged_ucd_data_IO
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd_header_name, iflag_udt,       &
     &      file_name)
!
        write(*,*) 'single UCD grid data: ', trim(file_name)
        open (ucd_file_code, file=ucd_file_name, status='replace')
      end if
!
      call write_merged_ucd_mesh(ucd_file_code, nnod_ucd, nele_ucd,     &
     &    nnod_4_ele_ucd, xx_ucd, ie_ucd, ntot_comp_ucd)
!
      if(my_rank .eq. 0) close(ucd_file_code)
!
      end subroutine write_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_merged_vtk_file(istep)
!
      use merged_vtk_data_IO
!
      integer(kind = kint), intent(in) ::  istep
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd_header_name, iflag_vtk,       &
     &      istep, file_name)
!
        write(*,*) 'single VTK data: ', trim(file_name)
        open (ucd_file_code, file=file_name, form='formatted',          &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_mesh(ucd_file_code, nnod_ucd,               &
     &    nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd,                     &
     &    istack_nod_ucd_list, istack_internod_ucd_list,                &
     &    istack_ele_ucd_list)
!
      call write_merged_vtk_fields(ucd_file_code, nnod_ucd,             &
     &    num_field_ucd, ntot_comp_ucd, num_comp_ucd, phys_name_ucd,    &
     &    d_nod_ucd, istack_nod_ucd_list, istack_internod_ucd_list)
!
      if(my_rank .eq. 0) close(ucd_file_code)
!
      end subroutine write_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_phys(istep)
!
      use merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd_header_name, iflag_vtd,       &
     &      istep, file_name)
!
        write(*,*) 'single VTK field data: ', file_name
        open (ucd_file_code, file=file_name, form='formatted',          &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_fields(ucd_file_code, nnod_ucd,             &
     &    num_field_ucd, ntot_comp_ucd, num_comp_ucd, phys_name_ucd,    &
     &    d_nod_ucd, istack_nod_ucd_list, istack_internod_ucd_list)
!
      if(my_rank .eq. 0) close(ucd_file_code)
!
      end subroutine write_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_grid
!
      use merged_vtk_data_IO
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd_header_name, iflag_vtd,       &
     &      file_name)
!
        write(*,*) 'single VTK grid data:     ', trim(file_name)
        open (ucd_file_code,  file=file_name, form='formatted',         &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_mesh(ucd_file_code, nnod_ucd,               &
     &    nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd,                     &
     &    istack_nod_ucd_list, istack_internod_ucd_list,                &
     &    istack_ele_ucd_list)
!
      if(my_rank .eq. 0) close(ucd_file_code)
!
      end subroutine write_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module merged_udt_vtk_file_IO
