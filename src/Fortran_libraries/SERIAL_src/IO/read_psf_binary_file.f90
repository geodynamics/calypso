!>@file  read_psf_binary_file.f90
!!       module read_psf_binary_file
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!        subroutine read_alloc_iso_bin_file(file_name, ucd_b)
!!        subroutine read_alloc_psf_bin_grid(file_name, nprocs, ucd_b)
!!        subroutine read_alloc_psf_bin_file(file_name, nprocs, ucd_b)
!!          character(len = kchara), intent(in) :: file_name
!!          type(ucd_data), intent(inout) :: ucd_b
!!
!!        subroutine read_iso_bin_file(file_name, ucd_b)
!!        subroutine read_psf_bin_grid(file_name, nprocs, ucd_b)
!!        subroutine read_psf_bin_file(file_name, nprocs, ucd_b)
!!          character(len = kchara), intent(in) :: file_name
!!          type(ucd_data), intent(inout) :: ucd_b
!!@endverbatim
!
      module read_psf_binary_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_binary_IO_buffer
      use t_ucd_data
!
      implicit none
!
      type(binary_IO_buffer), save, private :: bbuf_ucd
      integer(kind = kint_gl), allocatable, private :: itmp1_mp(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_iso_bin_file(file_name, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: nprocs
!
!
      write(*,*) 'read binary isosurface file: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_alloc_psf_bin_grid_data                                 &
     &   (nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call read_alloc_psf_bin_field_data                                &
     &   (nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_alloc_iso_bin_file
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_grid(file_name, nprocs, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      integer, intent(inout) :: nprocs
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
!
      write(*,*) 'read binary section grid: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_alloc_psf_bin_grid_data                                 &
     &   (nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_alloc_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_file(file_name, nprocs, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      integer, intent(in) :: nprocs
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
!
      write(*,*) 'read binary section data: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      allocate(itmp1_mp(nprocs))
!
      call read_alloc_psf_bin_field_data                                &
     &   (nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_alloc_psf_bin_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_iso_bin_file(file_name, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: nprocs
!
!
      write(*,*) 'read binary isosurface file: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_bin_grid_data(nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call read_psf_bin_field_data(nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_iso_bin_file
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_bin_grid(file_name, nprocs, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      integer, intent(inout) :: nprocs
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
!
      write(*,*) 'read binary section grid: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_bin_grid_data(nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_file(file_name, nprocs, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      integer, intent(in) :: nprocs
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      type(binary_IO_buffer), save :: bbuf_ucd
!
!
      write(*,*) 'read binary section data: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_bin_field_data(nprocs, ucd_b, bbuf_ucd, itmp1_mp)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      end module read_psf_binary_file
