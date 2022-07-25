!>@file  read_psf_binary_file.f90
!!       module read_psf_binary_file
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!      subroutine read_alloc_psf_bin_grid(file_name, np_udt, ucd_b)
!!      subroutine read_alloc_psf_bin_file                              &
!!     &         (file_name, np_udt, t_IO, ucd_b)
!!      subroutine read_alloc_iso_bin_file(file_name, t_IO, ucd_b)
!!      subroutine read_alloc_nostep_psf_bin_file(file_name, ucd_b)
!!      subroutine read_alloc_nostep_iso_bin_file(file_name, ucd_b)
!!        character(len = kchara), intent(in) :: file_name
!!        type(ucd_data), intent(inout) :: ucd_b
!!
!!      subroutine read_psf_bin_grid(file_name, np_udt, ucd_b)
!!      subroutine read_psf_bin_file(file_name, np_udt, t_IO, ucd_b)
!!      subroutine read_iso_bin_file(file_name, t_IO, ucd_b)
!!        character(len = kchara), intent(in) :: file_name
!!        type(ucd_data), intent(inout) :: ucd_b
!!@endverbatim
!
      module read_psf_binary_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_binary_IO_buffer
      use t_ucd_data
!
      implicit none
!
      type(binary_IO_buffer), save, private :: bbuf_ucd
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_grid(file_name, np_udt, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      integer, intent(inout) :: np_udt
      type(ucd_data), intent(inout) :: ucd_b
!
!
      write(*,*) 'read binary section grid: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, np_udt)
!
      call read_alloc_psf_bin_grid_data(np_udt, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_alloc_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_file                                &
     &         (file_name, np_udt, t_IO, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      integer, intent(in) :: np_udt
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: np_read
!
!
      write(*,*) 'read binary section data: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
!
      call read_psf_bin_time_data(np_read,                              &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, bbuf_ucd)
!
      if(np_read .ne. np_udt) then
        write(*,*)                                                      &
     &     '#. of subdomeins does not match between grid and field',    &
     &     np_udt, np_read
        stop
      end if
!
      call read_alloc_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_alloc_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_iso_bin_file(file_name, t_IO, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: np_read
!
!
      write(*,*) 'read binary isosurface file: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, np_read)
!
      call read_alloc_psf_bin_grid_data(np_read, ucd_b, bbuf_ucd)
!
      call read_psf_bin_time_data(np_read,                              &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, bbuf_ucd)
      call read_alloc_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_alloc_iso_bin_file
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_nostep_psf_bin_file(file_name, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: np_read
!
!
      write(*,*) 'read binary section data: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
!
      call read_one_integer_b(bbuf_ucd, np_read)
!
      call read_alloc_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_alloc_nostep_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_nostep_iso_bin_file(file_name, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: np_read, nprocs2
!
!
      write(*,*) 'read binary isosurface file: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, np_read)
!
      call read_alloc_psf_bin_grid_data(np_read, ucd_b, bbuf_ucd)
!
      call read_one_integer_b(bbuf_ucd, nprocs2)
      if(nprocs2 .ne. np_read) stop 'Wrong mesh and field data'
!
      call read_alloc_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
 
      end subroutine read_alloc_nostep_iso_bin_file
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_grid(file_name, np_udt, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      integer, intent(inout) :: np_udt
      type(ucd_data), intent(inout) :: ucd_b
!
!
      write(*,*) 'read binary section grid: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, np_udt)
      call read_psf_bin_grid_data(np_udt, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_file(file_name, np_udt, t_IO, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      integer, intent(in) :: np_udt
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_b
!
      type(binary_IO_buffer), save :: bbuf_ucd
      integer :: np_read
!
!
      write(*,*) 'read binary section data: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
!
      call read_psf_bin_time_data(np_read,                              &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, bbuf_ucd)
!
      if(np_read .ne. np_udt) then
        write(*,*)                                                      &
     &     '#. of subdomeins does not match between grid and field',    &
     &     np_udt, np_read
        stop
      end if
!
      call read_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_iso_bin_file(file_name, t_IO, ucd_b)
!
      use binary_IO
      use read_udt_from_binary_data
!
      character(len = kchara), intent(in) :: file_name
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: np_read
!
!
      write(*,*) 'read binary isosurface file: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, np_read)
!
      call read_psf_bin_grid_data(np_read, ucd_b, bbuf_ucd)
!
      call read_psf_bin_time_data(np_read,                              &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, bbuf_ucd)
      call read_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_iso_bin_file
!
! -----------------------------------------------------------------------
!
      end module read_psf_binary_file
