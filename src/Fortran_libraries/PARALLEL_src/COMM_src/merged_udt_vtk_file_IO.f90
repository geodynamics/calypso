!merged_udt_vtk_file_IO.f90
!------- module merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine init_merged_ucd(ucd, m_ucd)
!      subroutine finalize_merged_ucd(ucd, m_ucd)
!
!      subroutine write_merged_ucd_file(istep, ucd, m_ucd)
!      subroutine write_merged_udt_file(istep, ucd, m_ucd)
!      subroutine write_merged_grd_file(ucd, m_ucd)
!
!      subroutine write_merged_vtk_file(istep, ucdv)
!      subroutine write_merged_vtk_phys(istep, ucd, m_ucd)
!      subroutine write_merged_vtk_grid(ucd, m_ucd)
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
      subroutine init_merged_ucd(ucd, m_ucd)
!
      use m_geometry_parameter
      use m_nod_comm_table
      use m_merged_ucd_data
      use hdf5_file_IO
!
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      call allocate_merged_ucd_num(m_ucd)
      call count_merged_ucd(numnod, internal_node, ucd%nele, m_ucd)
!
      call allocate_merged_ucd_data(numnod,                             &
     &    ucd%nnod_4_ele, ucd%ntot_comp)
      call set_node_double_address                                      &
     &         (num_neib, id_neib, istack_import, item_import,          &
     &          istack_export, item_export)
!
      call update_ele_by_double_address(ucd%nele, ucd%nnod_4_ele,       &
     &    ucd%ie, m_ucd)
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
      use merged_ucd_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_ucd,       &
     &      istep, file_name)
!
        write(*,*) 'single UCD data: ', trim(file_name)
        open(id_vtk_file,file=file_name, form='formatted')
      end if
!
      call write_merged_ucd_mesh(id_vtk_file, ucd%nnod, ucd%nele,       &
     &    ucd%nnod_4_ele, ucd%xx, ucd%ie, ucd%ntot_comp,                &
     &    m_ucd%istack_merged_nod,  m_ucd%istack_merged_intnod,         &
     &    m_ucd%istack_merged_ele)
      call write_merged_ucd_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd, m_ucd%istack_merged_nod,                           &
     &    m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_udt_file(istep, ucd, m_ucd)
!
      use merged_ucd_data_IO
!
      integer(kind = kint), intent(in) ::  istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_udt,       &
     &      istep, file_name)
!
        write(*,*) 'single UCD field data: ', trim(file_name)
        open(id_vtk_file,file=file_name, form='formatted')
      end if
!
      call write_merged_ucd_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd, m_ucd%istack_merged_nod,                           &
     &    m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_merged_grd_file(ucd, m_ucd)
!
      use merged_ucd_data_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd%file_prefix, iflag_udt,       &
     &      file_name)
!
        write(*,*) 'single UCD grid data: ', trim(file_name)
        open (id_vtk_file, file=file_name, status='replace')
      end if
!
      call write_merged_ucd_mesh(id_vtk_file, ucd%nnod, ucd%nele,       &
     &    ucd%nnod_4_ele, ucd%xx, ucd%ie, ucd%ntot_comp,                &
     &    m_ucd%istack_merged_nod,  m_ucd%istack_merged_intnod,         &
     &    m_ucd%istack_merged_ele)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_merged_vtk_file(istep, ucd, m_ucd)
!
      use merged_vtk_data_IO
!
      integer(kind = kint), intent(in) ::  istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_vtk,       &
     &      istep, file_name)
!
        write(*,*) 'single VTK data: ', trim(file_name)
        open (id_vtk_file, file=file_name, form='formatted',            &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_mesh(id_vtk_file, ucd%nnod,                 &
     &    ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,                     &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod,          &
     &    m_ucd%istack_merged_ele)
!
      call write_merged_vtk_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd, m_ucd%istack_merged_nod,                           &
     &    m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_phys(istep, ucd, m_ucd)
!
      use merged_vtk_data_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_ucd_file_name(ucd%file_prefix, iflag_vtd,       &
     &      istep, file_name)
!
        write(*,*) 'single VTK field data: ', file_name
        open (id_vtk_file, file=file_name, form='formatted',            &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_fields(id_vtk_file, ucd%nnod,               &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd, m_ucd%istack_merged_nod,                           &
     &    m_ucd%istack_merged_intnod)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_grid(ucd, m_ucd)
!
      use merged_vtk_data_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        call set_single_grd_file_name(ucd%file_prefix, iflag_vtd,       &
     &      file_name)
!
        write(*,*) 'single VTK grid data:     ', trim(file_name)
        open (id_vtk_file,  file=file_name, form='formatted',           &
     &                  status ='unknown')
      end if
!
      call write_merged_vtk_mesh(id_vtk_file, ucd%nnod,                 &
     &    ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,                     &
     &    m_ucd%istack_merged_nod, m_ucd%istack_merged_intnod,          &
     &    m_ucd%istack_merged_ele)
!
      if(my_rank .eq. 0) close(id_vtk_file)
!
      end subroutine write_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module merged_udt_vtk_file_IO
