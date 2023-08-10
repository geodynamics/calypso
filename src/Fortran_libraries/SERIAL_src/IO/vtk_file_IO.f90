!>@file  vtk_file_IO.f90
!!       module vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in March, 2007
!!@n      Modified by H. Matsui in July, 2013
!
!> @brief Output VTK file
!!
!!@verbatim
!!      subroutine write_parallel_vtk_file                              &
!!     &         (id_rank, nprocs, istep, file_prefix)
!!
!!      subroutine write_vtk_file(id_rank, file_name, ucd)
!!      subroutine write_vtk_phys(id_rank, file_name, ucd)
!!      subroutine write_vtk_grid(id_rank, file_name, ucd)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_vtk_file(id_rank, file_name, ucd)
!!      subroutine read_vtk_phys(id_rank, file_name, ucd)
!!      subroutine read_vtk_grid(id_rank, file_name, ucd)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine read_alloc_vtk_file(id_rank, file_name, ucd, iend)
!!      subroutine read_alloc_vtk_phys(id_rank, file_name, ucd, iend)
!!      subroutine read_alloc_vtk_grid(id_rank, file_name, ucd)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module vtk_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use udt_to_VTK_data_IO
      use t_ucd_data
!
      implicit none
!
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_parallel_vtk_file                                &
     &         (id_rank, nprocs, istep, file_prefix)
!
      use set_parallel_file_name
      use set_ucd_file_names
      use set_ucd_extensions
!
      character(len=kchara), intent(in) :: file_prefix
      integer, intent(in) :: id_rank, nprocs
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara)  :: file_name, fname_tmp
      character(len=kchara) :: fname_nodir
      integer :: ip
!
!
      if(id_rank .gt. 0) return
!
      fname_nodir = delete_directory_name(file_prefix)
      fname_tmp =   add_int_suffix(istep, file_prefix)
      file_name =   add_pvtk_extension(fname_tmp)
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
        file_name = set_parallel_vtk_file_name(fname_nodir, ip, istep)
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
!
      subroutine write_vtk_file(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK file: ', trim(file_name)
!
      open(id_vtk_file, file=file_name, form='formatted',               &
     &     status ='unknown')
      call write_ucd_mesh_to_VTK(id_vtk_file, ucd)
      call write_ucd_field_to_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine write_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK fields: ', trim(file_name)
!
      open(id_vtk_file, file=file_name, form='formatted',               &
     &     status ='unknown')
      call write_ucd_field_to_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine write_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK mesh: ', trim(file_name)
!
      open(id_vtk_file, file=file_name, form='formatted',               &
     &     status ='unknown')
      call write_ucd_mesh_to_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine write_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_vtk_file(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK file: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_mesh_from_VTK(id_vtk_file, ucd)
      call read_udt_field_from_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_vtk_file
!
!-----------------------------------------------------------------------
!
      subroutine read_vtk_phys(id_rank, file_name, ucd)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK fields: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_udt_field_from_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_vtk_phys
!
!-----------------------------------------------------------------------
!
      subroutine read_vtk_grid(id_rank, file_name, ucd)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK mesh: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_mesh_from_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_alloc_vtk_file(id_rank, file_name, ucd, iend)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint), intent(inout) ::  iend
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK file: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_alloc_ucd_mesh_from_VTK(id_vtk_file, ucd)
      call read_alloc_udt_field_from_VTK(id_vtk_file, ucd, iend)
      close(id_vtk_file)
!
      end subroutine read_alloc_vtk_file
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_vtk_phys(id_rank, file_name, ucd, iend)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint), intent(inout) ::  iend
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK fields: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_alloc_udt_field_from_VTK(id_vtk_file, ucd, iend)
      close(id_vtk_file)
!
      end subroutine read_alloc_vtk_phys
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_vtk_grid(id_rank, file_name, ucd)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK mesh: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_alloc_ucd_mesh_from_VTK(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_alloc_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module vtk_file_IO
