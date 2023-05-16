!>@file  gz_read_psf_binary_file.f90
!!       module gz_read_psf_binary_file
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read gzipped binary section file
!!
!!@verbatim
!!      subroutine gz_read_alloc_psf_bin_grid(gzip_name, np_udt, ucd_z)
!!      subroutine gz_read_alloc_psf_bin_file                           &
!!     &         (gzip_name, np_udt, t_IO, ucd_z)
!!      subroutine gz_read_alloc_iso_bin_file(gzip_name, t_IO, ucd_z)
!!        character(len = kchara), intent(in) :: gzip_name
!!        type(ucd_data), intent(inout) :: ucd_z
!!
!!      subroutine gz_read_psf_bin_grid(gzip_name, np_udt, ucd_z)
!!      subroutine gz_read_psf_bin_file(gzip_name, np_udt, t_IO, ucd_z)
!!      subroutine gz_read_iso_bin_file(gzip_name, t_IO, ucd_z)
!!        character(len = kchara), intent(in) :: gzip_name
!!        type(ucd_data), intent(inout) :: ucd_z
!!@endverbatim
!
      module gz_read_psf_binary_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_buffer_4_gzip
      use t_ucd_data
!
      implicit none
!
      type(buffer_4_gzip), save, private :: zbuf_ucd
      character, pointer, private, save :: FPz_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_grid(gzip_name, np_udt, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      integer, intent(inout) :: np_udt
      type(ucd_data), intent(inout) :: ucd_z
!
!
      call open_rd_gzfile_b(FPz_psf, gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(FPz_psf, zbuf_ucd, np_udt)
      call gz_read_alloc_psf_bin_grid_data                              &
     &   (FPz_psf, np_udt, ucd_z, zbuf_ucd)
!
      call close_gzfile_b(FPz_psf)
!
      end subroutine gz_read_alloc_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_file                             &
     &         (gzip_name, np_udt, t_IO, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      integer, intent(in) :: np_udt
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read
!
      call open_rd_gzfile_b(FPz_psf, gzip_name, izero, zbuf_ucd)
      call gz_read_psf_bin_time_data(FPz_psf, np_read,                  &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_ucd)
!
      if(np_read .ne. np_udt) then
        write(*,*)                                                      &
     &     '#. of subdomeins does not match between grid and field',    &
     &     np_udt, np_read
        stop
      end if
!
      call gz_read_alloc_psf_bin_fld_data                               &
     &   (FPz_psf, np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b(FPz_psf)
!
      end subroutine gz_read_alloc_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_iso_bin_file(gzip_name, t_IO, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read
!
!
      call open_rd_gzfile_b(FPz_psf, gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(FPz_psf, zbuf_ucd, np_read)
      call gz_read_alloc_psf_bin_grid_data                              &
     &   (FPz_psf, np_read, ucd_z, zbuf_ucd)
!
      call gz_read_psf_bin_time_data(FPz_psf, np_read,                  &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_ucd)
      call gz_read_alloc_psf_bin_fld_data                               &
     &   (FPz_psf, np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b(FPz_psf)
!
      end subroutine gz_read_alloc_iso_bin_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gz_read_psf_bin_grid(gzip_name, np_udt, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      integer, intent(inout) :: np_udt
      type(ucd_data), intent(inout) :: ucd_z
!
!
      call open_rd_gzfile_b(FPz_psf, gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(FPz_psf, zbuf_ucd, np_udt)
      call gz_read_psf_bin_grid_data(FPz_psf, np_udt, ucd_z, zbuf_ucd)
      call close_gzfile_b(FPz_psf)
!
      end subroutine gz_read_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_psf_bin_file(gzip_name, np_udt, t_IO, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      integer, intent(in) :: np_udt
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read
!
!
      call open_rd_gzfile_b(FPz_psf, gzip_name, izero, zbuf_ucd)
!
      call gz_read_psf_bin_time_data(FPz_psf, np_read,                  &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_ucd)
!
      if(np_read .ne. np_udt) then
        write(*,*)                                                      &
     &     '#. of subdomeins does not match between grid and field',    &
     &     np_udt, np_read
        stop
      end if
!
      call gz_read_psf_bin_field_data                                   &
     &   (FPz_psf, np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b(FPz_psf)
!
      end subroutine gz_read_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_iso_bin_file(gzip_name, t_IO, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read
!
!
      call open_rd_gzfile_b(FPz_psf, gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(FPz_psf, zbuf_ucd, np_read)
!
      call gz_read_psf_bin_grid_data(FPz_psf, np_read, ucd_z, zbuf_ucd)
!
      call gz_read_psf_bin_time_data(FPz_psf, np_read,                  &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_ucd)
      call gz_read_psf_bin_field_data                                   &
     &   (FPz_psf, np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b(FPz_psf)
!
      end subroutine gz_read_iso_bin_file
!
!  ---------------------------------------------------------------------
!
      end module gz_read_psf_binary_file
