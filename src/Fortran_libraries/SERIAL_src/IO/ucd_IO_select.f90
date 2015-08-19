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
!!      subroutine set_ucd_file_define(ucd)
!!
!!      subroutine sel_write_ucd_file(my_rank, istep_ucd, ucd)
!!      subroutine sel_write_udt_file(my_rank, istep_ucd, ucd)
!!      subroutine sel_write_grd_file(my_rank, ucd)
!!
!!      subroutine sel_read_udt_param(my_rank, istep_ucd, ucd)
!!      subroutine sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
!!      subroutine sel_read_udt_file(my_rank, istep_ucd, ucd)
!!      subroutine sel_read_ucd_file(my_rank, istep_ucd, nnod_ele, ucd)
!!@endverbatim
!!
!!@param my_rank  process ID
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
!      use ucd_field_file_IO_b
!
!#ifdef ZLIB_IO
!      use gz_udt_file_IO
!      use gz_ucd_field_file_IO
!      use gz_write_ucd_to_vtk_file
!#endif
!
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
      subroutine set_ucd_file_define(ucd)
!
      use m_ctl_data_4_platforms
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%ifmt_file = udt_file_head_ctl%iflag
      if (ucd%ifmt_file .gt. 0)                                         &
     &         ucd%file_prefix = udt_file_head_ctl%charavalue
!
      call choose_ucd_file_format(udt_file_fmt_ctl%charavalue,          &
     &    udt_file_fmt_ctl%iflag, ucd%ifmt_file)
!
      end subroutine set_ucd_file_define
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_ucd_file(my_rank, istep_ucd, ucd)
!
      use write_ucd_to_vtk_file
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(in) :: ucd
!
!
      if(ucd%ifmt_file .eq. iflag_vtk) then
        call write_udt_data_2_vtk_file(my_rank, istep_ucd, ucd)
!
!#ifdef ZLIB_IO
!      else if(ucd%ifmt_file .eq. iflag_vtk_gz) then
!        call write_ucd_data_2_gz_vtk(my_rank, istep_ucd, ucd)
!      else if (ucd%ifmt_file .eq. iflag_vtd_gz) then
!        call write_ucd_data_2_gz_vtk_phys(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_ucd_gz) then
!        call write_gz_ucd_file(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call write_gz_udt_file(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_fld_gz) then
!        call write_ucd_2_gz_fld_file(my_rank, istep_ucd, ucd)
!#endif
!
!      else if (ucd%ifmt_file .eq. iflag_bin) then
!        call write_ucd_2_fld_file_b(my_rank, istep_ucd, ucd)
      else if (ucd%ifmt_file .eq. iflag_vtd) then
        call write_udt_data_2_vtk_phys(my_rank, istep_ucd, ucd)
      else if(ucd%ifmt_file .eq. iflag_ucd) then
        call write_ucd_file(my_rank, istep_ucd, ucd)
      else if(ucd%ifmt_file .eq. iflag_udt) then
        call write_udt_file(my_rank, istep_ucd, ucd)
      else
        call write_ucd_2_fld_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_write_ucd_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_udt_file(my_rank, istep_ucd, ucd)
!
      use write_ucd_to_vtk_file
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(in) :: ucd
!
!
      if(ucd%ifmt_file .eq. iflag_vtk) then
        call write_udt_data_2_vtk_file(my_rank, istep_ucd, ucd)
!
!#ifdef ZLIB_IO
!      else if(ucd%ifmt_file .eq. iflag_vtk_gz) then
!        call write_ucd_data_2_gz_vtk(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_vtd_gz) then
!        call write_ucd_data_2_gz_vtk_phys(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_ucd_gz) then
!        call write_gz_ucd_file(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call write_gz_udt_file(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_fld_gz) then
!        call write_ucd_2_gz_fld_file(my_rank, istep_ucd, ucd)
!#endif
!
!      else if (ucd%ifmt_file .eq. iflag_bin) then
!        call write_ucd_2_fld_file_b(my_rank, istep_ucd, ucd)
      else if(ucd%ifmt_file .eq. iflag_vtd) then
        call write_udt_data_2_vtk_phys(my_rank, istep_ucd, ucd)
      else if(ucd%ifmt_file .eq. iflag_ucd) then
        call write_ucd_file(my_rank, istep_ucd, ucd)
      else if(ucd%ifmt_file .eq. iflag_udt) then
        call write_udt_file(my_rank, istep_ucd, ucd)
      else
        call write_ucd_2_fld_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_write_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_grd_file(my_rank, ucd)
!
      use udt_file_IO
      use write_ucd_to_vtk_file
!
      integer(kind=kint), intent(in) :: my_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(ucd%ifmt_file .eq. iflag_vtd) then
        call write_udt_data_2_vtk_grid(my_rank, ucd)
!
!#ifdef ZLIB_IO
!      else if(ucd%ifmt_file .eq. iflag_vtd_gz) then
!        call write_ucd_data_2_gz_vtk_grid(my_rank, ucd)
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call write_gz_grd_file(my_rank, ucd)
!#endif
!
      else if(ucd%ifmt_file .eq. iflag_udt) then
        call write_grd_file(my_rank, ucd)
      end if
!
      end subroutine sel_write_grd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_udt_param(my_rank, istep_ucd, ucd)
!
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
      if(ucd%ifmt_file .eq. iflag_udt) then
        call read_and_alloc_udt_params(my_rank, istep_ucd, ucd)
!
!#ifdef ZLIB_IO
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call read_alloc_gz_udt_head(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_fld_gz) then
!        call read_alloc_ucd_2_gz_fld_file(my_rank, istep_ucd, ucd)
!#endif
!
!      else if (ucd%ifmt_file .eq. iflag_bin) then
!        call read_alloc_ucd_2_fld_header_b(my_rank, istep_ucd, ucd)
      else
        call read_alloc_ucd_2_fld_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_read_udt_param
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
!
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
      if(ucd%ifmt_file .eq. iflag_udt) then
        call read_and_alloc_udt_file(my_rank, istep_ucd, ucd)
!
!#ifdef ZLIB_IO
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call read_alloc_gz_udt_file(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_fld_gz) then
!        call read_alloc_ucd_2_gz_fld_file(my_rank, istep_ucd, ucd)
!#endif
!
!      else if (ucd%ifmt_file .eq. iflag_bin) then
!        call read_alloc_ucd_2_fld_file_b(my_rank, istep_ucd, ucd)
      else
        call read_alloc_ucd_2_fld_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_read_alloc_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_udt_file(my_rank, istep_ucd, ucd)
!
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
      if(ucd%ifmt_file .eq. iflag_udt) then
        call read_udt_file(my_rank, istep_ucd, ucd)
!
!#ifdef ZLIB_IO
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call read_gz_udt_file(my_rank, istep_ucd, ucd)
!      else if(ucd%ifmt_file .eq. iflag_fld_gz) then
!        call read_ucd_2_gz_fld_file(my_rank, istep_ucd, ucd)
!#endif
!
!      else if (ucd%ifmt_file .eq. iflag_bin) then
!        call read_ucd_2_fld_file_b(my_rank, istep_ucd, ucd)
      else
        call read_ucd_2_fld_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_read_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_ucd_file(my_rank, istep_ucd, nnod_ele, ucd)
!
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd, nnod_ele
      type(ucd_data), intent(inout) :: ucd
!
!
      if (ucd%ifmt_file .eq. iflag_ucd) then
        call read_and_alloc_ucd_file(my_rank, istep_ucd, nnod_ele, ucd)
!
!#ifdef ZLIB_IO
!      else if (ucd%ifmt_file .eq. iflag_ucd_gz) then
!        call read_alloc_gz_ucd_file(my_rank, istep_ucd, nnod_ele, ucd)
!      else if(ucd%ifmt_file .eq. iflag_udt_gz) then
!        call read_gz_ucd_grd(my_rank, nnod_ele, ucd)
!        call read_alloc_gz_udt_file(my_rank, istep_ucd, ucd)
!#endif
!
      else
        call read_grd_file(my_rank, nnod_ele, ucd)
        call read_and_alloc_udt_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_read_ucd_file
!
!------------------------------------------------------------------
!
      end module ucd_IO_select
