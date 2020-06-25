!>@file  udt_file_IO.f90
!!       module udt_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief UCD format data IO
!!
!!@verbatim
!!      subroutine write_ucd_file(id_rank, file_name, ucd)
!!      subroutine write_udt_file(id_rank, file_name, ucd)
!!      subroutine write_grd_file(id_rank, file_name, ucd)
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_udt_file(id_rank, file_name, ucd)
!!      subroutine read_ucd_file(id_rank, file_name, ucd)
!!      subroutine read_grd_file(id_rank, file_name, nnod_ele, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine read_alloc_udt_params(id_rank, file_name, ucd)
!!      subroutine read_alloc_udt_file(id_rank, file_name, ucd)
!!      subroutine read_alloc_ucd_file(id_rank, file_name, ucd)
!!      subroutine read_alloc_grd_file(id_rank, file_name, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank  process ID
!!@param file_name    file name
!!@param ucd      Structure for FEM field data IO
!
      module udt_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ucd_data
!
      implicit none
!
!>      file ID for UCD file
      integer(kind = kint), parameter, private :: id_ucd_file = 16
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD file: ', trim(file_name)
!
      open(id_ucd_file,file=file_name, form='formatted')
      call write_ucd_type_mesh(id_ucd_file, ucd)
      call write_udt_type_fields(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine write_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_udt_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD field: ', trim(file_name)
!
      open(id_ucd_file,file=file_name, form='formatted')
      call write_udt_type_fields(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine write_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_grd_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD mesh: ', trim(file_name)
!
      open (id_ucd_file, file=file_name, status='replace')
      call write_ucd_type_mesh(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine write_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_udt_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii UDT data: ', trim(file_name)
      open(id_ucd_file, file=file_name, status='old')
      call read_udt_field(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine read_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_ucd_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii UDT data: ', trim(file_name)
      open(id_ucd_file, file=file_name, status='old')
      call read_ucd_mesh_data(id_ucd_file, ucd)
      call read_udt_field(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine read_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine read_grd_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii PSF grid data: ', trim(file_name)
!
      open(id_ucd_file, file=file_name, form='formatted',               &
     &     status='old')
      call read_ucd_mesh_data(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine read_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_alloc_udt_params(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii UDT data: ', trim(file_name)
!
      open (id_ucd_file, file=file_name, status='old')
      call read_alloc_udt_field(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine read_alloc_udt_params
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_udt_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
      use udt_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii UDT data: ', trim(file_name)
!
      open (id_ucd_file, file=file_name, status='old')
      call read_alloc_udt_field(id_ucd_file, ucd)
!
      call read_ucd_field_data(id_ucd_file, ucd%nnod,                   &
     &    ucd%ntot_comp, ucd%d_ucd)
      close(id_ucd_file)
!
      end subroutine read_alloc_udt_file
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_ucd_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
      use udt_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii UCD data: ', trim(file_name)
!
      open(id_ucd_file, file=file_name, status='old')
      call read_alloc_ucd_mesh_data(id_ucd_file, ucd)
      call read_alloc_udt_field(id_ucd_file, ucd)
!
      call read_ucd_field_data(id_ucd_file, ucd%nnod,                   &
     &    ucd%ntot_comp, ucd%d_ucd)
      close(id_ucd_file)
!
      end subroutine read_alloc_ucd_file
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_grd_file(id_rank, file_name, ucd)
!
      use udt_type_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii PSF grid data: ', trim(file_name)
!
      open(id_ucd_file, file=file_name, form='formatted',               &
     &     status='old')
      call read_alloc_ucd_mesh_data(id_ucd_file, ucd)
      close(id_ucd_file)
!
      end subroutine read_alloc_grd_file
!
!-----------------------------------------------------------------------
!
      end module udt_file_IO
