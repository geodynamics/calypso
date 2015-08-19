!>@file   write_ucd_to_vtk_file.f90
!!@brief  module write_ucd_to_vtk_file
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on July, 2006
!!@n       Modified by H.Matsui on March, 2013
!
!>@brief Output FEM field data to distributed VTK file
!!
!!@verbatim
!!      subroutine write_parallel_vtk_file(my_rank, nprocs, istep, ucd)
!!
!!      subroutine write_udt_data_2_vtk_file(my_rank, istep, ucd)
!!      subroutine write_udt_data_2_vtk_phys(my_rank, istep, ucd)
!!      subroutine write_udt_data_2_vtk_grid(my_rank, ucd)
!!
!!      subroutine write_sgl_udt_2_vtk_phys(istep, ucd)
!!      subroutine write_sgl_udt_data_2_vtk_grid(ucd)
!!
!!      subroutine read_udt_data_2_vtk_file(my_rank, istep, ucd)
!!      subroutine read_udt_data_2_vtk_phys(my_rank, istep, ucd)
!!      subroutine read_udt_data_2_vtk_grid(my_rank, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param my_rank    subdomain ID
!!@param istep      Step number for VTK data
!!@param ucd      Structure for FEM field data IO
!
      module write_ucd_to_vtk_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_field_file_format
!
      use t_ucd_data
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
      private :: read_ucd_mesh_by_vtk, read_ucd_field_by_vtk
!
!-----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_parallel_vtk_file(my_rank, nprocs, istep, ucd)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, nprocs, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara)  :: file_name, fname_tmp
      character(len=kchara) :: fname_nodir
      integer(kind = kint) :: ip
!
!
      if(my_rank .gt. 0) return
!
      call delete_directory_name(ucd%file_prefix, fname_nodir)
      call add_int_suffix(istep, ucd%file_prefix, fname_tmp)
      call add_pvtk_extension(fname_tmp, file_name)
!
      write(*,*) 'Write parallel VTK file: ', trim(file_name)
      open(id_vtk_file, file=file_name)
!
      write(id_vtk_file,'(a)') '<File version="pvtk-1.0"'
      write(id_vtk_file,'(a)')                                          &
     &     '       dataType="vtkUnstructuredGrid"'
      write(id_vtk_file,'(a,i6,a)')                                     &
     &     '       numberOfPieces="', nprocs, '" >'
      do ip = 0, nprocs-1
        call set_parallel_ucd_file_name(fname_nodir, iflag_vtk,         &
     &      ip, istep, file_name)
        write(id_vtk_file,'(3a)') '   <Piece fileName="',               &
     &                       trim(file_name), '" />'
      end do
      write(id_vtk_file,'(a)') '</File>'
!
      close(id_vtk_file)
!
      end subroutine write_parallel_vtk_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_file(my_rank, istep, ucd)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_vtk,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK file: ', trim(file_name)
!
      call write_vtk_file(file_name, id_vtk_file, ucd)
!
      end subroutine write_udt_data_2_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_phys(my_rank, istep, ucd)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_vtd,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK fields: ', trim(file_name)
!
      call write_vtk_phys(file_name, id_vtk_file, ucd)
!
      end subroutine write_udt_data_2_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_grid(my_rank, ucd)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd%file_prefix, iflag_vtd,       &
     &    my_rank, file_name)
!
      if(my_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK mesh: ', trim(file_name)
!
      call write_vtk_grid(file_name, id_vtk_file, ucd)
!
      end subroutine write_udt_data_2_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sgl_udt_2_vtk_phys(istep, ucd)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_vtd,         &
     &    istep, file_name)
!
      if(i_debug .gt. 0) write(*,*)                                     &
     &     'Write ascii VTK fields: ', trim(file_name)
!
      call write_vtk_phys(file_name, id_vtk_file, ucd)
!
      end subroutine write_sgl_udt_2_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_sgl_udt_data_2_vtk_grid(ucd)
!
      use vtk_file_IO
!
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_single_grd_file_name(ucd%file_prefix, iflag_vtd,       &
     &    file_name)
!
      if(i_debug .gt. 0) write(*,*)                                     &
     &     'Write ascii VTK mesh: ', trim(file_name)
!
      call write_vtk_grid(file_name, id_vtk_file, ucd)
!
      end subroutine write_sgl_udt_data_2_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_udt_data_2_vtk_file(my_rank, istep, ucd)
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_vtk,       &
     &    my_rank, istep, file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_mesh_by_vtk(ucd)
      call read_ucd_field_by_vtk(ucd)
      close(id_vtk_file)
!
      end subroutine read_udt_data_2_vtk_file
!
!-----------------------------------------------------------------------
!
      subroutine read_udt_data_2_vtk_phys(my_rank, istep, ucd)
!
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_vtd,       &
     &    my_rank, istep, file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_mesh_by_vtk(ucd)
      close(id_vtk_file)
!
      end subroutine read_udt_data_2_vtk_phys
!
!-----------------------------------------------------------------------
!
      subroutine read_udt_data_2_vtk_grid(my_rank, ucd)
!
      integer(kind = kint), intent(in) ::  my_rank
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd%file_prefix, iflag_vtd,       &
     &    my_rank, file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_field_by_vtk(ucd)
      close(id_vtk_file)
!
      end subroutine read_udt_data_2_vtk_grid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_by_vtk(ucd)
!
      use vtk_data_IO
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: inod, iele
!
!
      call read_vtk_node_head(id_vtk_file, ucd%nnod)
      call allocate_ucd_node(ucd)
!
      call read_vtk_each_field(id_vtk_file, ucd%nnod, ithree,           &
     &    ucd%nnod, ucd%xx)
!
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = inod
      end do
!
      call read_vtk_connect_head(id_vtk_file, ucd%nele, ucd%nnod_4_ele)
      call allocate_ucd_ele(ucd)
!
      call read_vtk_connect_data(id_vtk_file, ucd%nele, ucd%nnod_4_ele, &
     &    ucd%nele, ucd%ie)
      call read_vtk_cell_type(id_vtk_file, ucd%nele)
!
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = iele
      end do
!
      end subroutine read_ucd_mesh_by_vtk
!
!-----------------------------------------------------------------------
!
      subroutine read_ucd_field_by_vtk(ucd)
!
      use vtk_data_IO
!
      type(ucd_data), intent(inout) :: ucd
      type(ucd_data) :: tmp
!
      integer(kind = kint) :: iflag_end, ncomp_field
      character(len=kchara)  :: field_name
      real(kind = kreal), allocatable :: d_tmp(:,:)
!
!
      call read_vtk_fields_head(id_vtk_file, ucd%nnod)
!
      tmp%nnod =      ucd%nnod
      ucd%num_field = 0
      ucd%ntot_comp = 0
      call allocate_ucd_phys_name(ucd)
      call allocate_ucd_phys_data(ucd)
!
      do
        call read_vtk_each_field_head(id_vtk_file, iflag_end,           &
     &      ncomp_field, field_name)
        if(iflag_end .ne. izero) exit
!
        tmp%num_field = ucd%num_field
        tmp%ntot_comp = ucd%ntot_comp
        call allocate_ucd_phys_name(tmp)
        call allocate_ucd_phys_data(tmp)
!
        call append_new_ucd_field_name(field_name, ncomp_field,         &
     &      tmp, ucd)
!
        allocate(d_tmp(ucd%nnod,ncomp_field))
!
        call read_vtk_each_field(id_vtk_file, ucd%nnod, ncomp_field,    &
     &      ucd%nnod, d_tmp(1,1) )
        call append_new_ucd_field_data(ncomp_field, d_tmp,              &
     &      tmp, ucd)
!
        deallocate(d_tmp)
        call deallocate_ucd_data(tmp)
!
        iflag_end = izero
      end do
!
      end subroutine read_ucd_field_by_vtk
!
!-----------------------------------------------------------------------
!
      end module write_ucd_to_vtk_file
