!>@file  ucd_IO_select.F90
!!       module ucd_IO_select
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui on July, 2006
!!@n           Modified by H.Matsui on May, 2009
!
!> @brief UCD data IO selector
!!
!!@verbatim
!!      subroutine set_ucd_file_define(plt, ucd_param)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: ucd_param
!!
!!      subroutine sel_write_ucd_file                                   &
!!     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!!      subroutine sel_write_udt_file                                   &
!!     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!!      subroutine sel_write_grd_file(id_rank, ucd_param, ucd)
!!
!!      subroutine sel_read_udt_param                                   &
!!     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!!      subroutine sel_read_alloc_udt_file                              &
!!     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!!      subroutine sel_read_udt_file                                    &
!!     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!!      subroutine sel_read_ucd_file                                    &
!!     &         (id_rank, istep_ucd, nnod_ele, ucd_param, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank  process ID
!!@param istep_ucd    step number for output
!
      module ucd_IO_select
!
      use m_precision
      use m_constants
      use m_file_format_switch
      use m_field_file_format
!
      use udt_file_IO
      use ucd_field_file_IO
      use ucd_field_file_IO_b
      use write_ucd_to_vtk_file
      use set_ucd_file_names
!
#ifdef ZLIB_IO
      use gz_udt_file_IO
      use gz_ucd_field_file_IO
      use gz_write_ucd_to_vtk_file
#endif
!
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_ucd_file_define(plt, ucd_param)
!
      use t_ctl_data_4_platforms
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      ucd_param%iflag_IO = plt%field_file_prefix%iflag
      if(ucd_param%iflag_IO .gt. 0)                                     &
     &       ucd_param%file_prefix = plt%field_file_prefix%charavalue
!
      call choose_ucd_file_format(plt%field_file_fmt_ctl%charavalue,    &
     &    plt%field_file_fmt_ctl%iflag, ucd_param%iflag_format)
!
      end subroutine set_ucd_file_define
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_ucd_file                                     &
     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!
      use write_ucd_to_vtk_file
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
      if(ucd_param%iflag_format .eq. iflag_vtk) then
        call write_udt_data_2_vtk_file(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_vtk_gz) then
        call write_ucd_data_2_gz_vtk(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_vtd_gz) then
        call write_ucd_data_2_gz_vtk_phys(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_ucd_gz) then
        call write_gz_ucd_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        call write_gz_udt_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_fld_gz) then
        call write_ucd_2_gz_fld_file(id_rank, file_name, t_IO, ucd)
#endif
!
      else if (ucd_param%iflag_format .eq. iflag_bin) then
        call write_ucd_2_fld_file_b                                     &
     &     (id_rank, file_name, t_IO, ucd, ierr)
      else if(ucd_param%iflag_format .eq. iflag_vtd) then
        call write_udt_data_2_vtk_phys(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_ucd) then
        call write_ucd_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt) then
        call write_udt_file(id_rank, file_name, ucd)
      else
        call write_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
      end if
!
      end subroutine sel_write_ucd_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_udt_file                                     &
     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!
      use write_ucd_to_vtk_file
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
!
      if(ucd_param%iflag_format .eq. iflag_vtk) then
        call write_udt_data_2_vtk_file(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_vtk_gz) then
        call write_ucd_data_2_gz_vtk(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_vtd_gz) then
        call write_ucd_data_2_gz_vtk_phys(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_ucd_gz) then
        call write_gz_ucd_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        call write_gz_udt_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_fld_gz) then
        call write_ucd_2_gz_fld_file(id_rank, file_name, t_IO, ucd)
#endif
!
      else if (ucd_param%iflag_format .eq. iflag_bin) then
        call write_ucd_2_fld_file_b                                     &
     &     (id_rank, file_name, t_IO, ucd, ierr)
      else if(ucd_param%iflag_format .eq. iflag_vtd) then
        call write_udt_data_2_vtk_phys(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_ucd) then
        call write_ucd_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt) then
        call write_udt_file(id_rank, file_name, ucd)
      else
        call write_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
      end if
!
      end subroutine sel_write_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_grd_file(id_rank, ucd_param, ucd)
!
      use write_ucd_to_vtk_file
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_grd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank)
!
!
      if(ucd_param%iflag_format .eq. iflag_vtd) then
        call write_udt_data_2_vtk_grid(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_vtd_gz) then
        call write_ucd_data_2_gz_vtk_grid(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        call write_gz_grd_file(id_rank, file_name, ucd)
#endif
!
      else if(ucd_param%iflag_format .eq. iflag_udt) then
        call write_grd_file(id_rank, file_name, ucd)
      end if
!
      end subroutine sel_write_grd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_udt_param                                     &
     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint) :: ierr = 0
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
!
      if(ucd_param%iflag_format .eq. iflag_udt) then
        call read_and_alloc_udt_params(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        call read_alloc_gz_udt_head(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_fld_gz) then
        call read_alloc_ucd_2_gz_fld_file                               &
     &     (id_rank, file_name, t_IO, ucd, ierr)
#endif
!
      else if (ucd_param%iflag_format .eq. iflag_bin) then
        call read_alloc_ucd_2_fld_header_b                              &
     &     (id_rank, file_name, t_IO, ucd, ierr)
      else
        call read_alloc_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
      end if
!
      if(ierr .gt. 0) stop "sel_read_udt_file error"
!
      end subroutine sel_read_udt_param
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_udt_file                                &
     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint) :: ierr = 0
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
!
      if(ucd_param%iflag_format .eq. iflag_udt) then
        call read_and_alloc_udt_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_vtd) then
        call read_udt_data_2_vtk_phys(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        call read_alloc_gz_udt_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_fld_gz) then
        call read_alloc_ucd_2_gz_fld_file                               &
     &     (id_rank, file_name, t_IO, ucd, ierr)
#endif
!
      else if (ucd_param%iflag_format .eq. iflag_bin) then
        call read_alloc_ucd_2_fld_file_b                                &
     &     (id_rank, file_name, t_IO, ucd, ierr)
      else
        call read_alloc_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
      end if
!
      if(ierr .gt. 0) stop "sel_read_udt_file error"
!
      end subroutine sel_read_alloc_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_udt_file                                      &
     &         (id_rank, istep_ucd, ucd_param, t_IO, ucd)
!
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint) :: ierr = 0
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
      if(ucd_param%iflag_format .eq. iflag_udt) then
        call read_udt_file(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        call read_gz_udt_file(id_rank, file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_fld_gz) then
        call read_ucd_2_gz_fld_file                                     &
     &     (id_rank, file_name, t_IO, ucd, ierr)
#endif
!
      else if (ucd_param%iflag_format .eq. iflag_bin) then
        call read_ucd_2_fld_file_b(id_rank, file_name, t_IO, ucd, ierr)
      else
        call read_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
      end if
!
      if(ierr .gt. 0) stop "sel_read_udt_file error"
!
      end subroutine sel_read_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_ucd_file                                      &
     &         (id_rank, istep_ucd, nnod_ele, ucd_param, ucd)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd, nnod_ele
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name, grid_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
      if (ucd_param%iflag_format .eq. iflag_ucd) then
        call read_and_alloc_ucd_file(id_rank, file_name, nnod_ele, ucd)
      else if(ucd_param%iflag_format .eq. iflag_vtk) then
        call read_udt_data_2_vtk_file(id_rank, file_name, ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_ucd_gz) then
        call read_alloc_gz_ucd_file(id_rank, file_name, nnod_ele, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_gz) then
        grid_name = set_parallel_grd_file_name(ucd_param%file_prefix,   &
     &             ucd_param%iflag_format, id_rank)
        call read_gz_ucd_grd(id_rank, grid_name, nnod_ele, ucd)
        call read_alloc_gz_udt_file(id_rank, file_name, ucd)
#endif
!
      else
        grid_name = set_parallel_grd_file_name(ucd_param%file_prefix,   &
     &             ucd_param%iflag_format, id_rank)
        call read_grd_file(id_rank, grid_name, nnod_ele, ucd)
        call read_and_alloc_udt_file(id_rank, file_name, ucd)
      end if
!
      end subroutine sel_read_ucd_file
!
!------------------------------------------------------------------
!
      end module ucd_IO_select
