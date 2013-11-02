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
      ucd%ifmt_file = i_udt_header
      if (i_udt_header .gt. 0) ucd%file_prefix = udt_file_head_ctl
!
      call choose_ucd_file_format(udt_file_fmt_ctl,                     &
     &    i_udt_files_fmt, ucd%ifmt_file)
!
      end subroutine set_ucd_file_define
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_ucd_file(my_rank, istep_ucd, ucd)
!
      use ucd_field_file_IO
      use write_ucd_to_vtk_file
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(in) :: ucd
!
!
        call write_udt_data_2_vtk_file(my_rank, istep_ucd, ucd)
!
      end subroutine sel_write_ucd_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_udt_file(my_rank, istep_ucd, ucd)
!
      use ucd_field_file_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(in) :: ucd
!
!
        call write_ucd_2_fld_file(my_rank, istep_ucd, ucd)
!
      end subroutine sel_write_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_grd_file(my_rank, ucd)
!
      use write_ucd_to_vtk_file
!
      integer(kind=kint), intent(in) :: my_rank
      type(ucd_data), intent(in) :: ucd
!
!
        call write_udt_data_2_vtk_grid(my_rank, ucd)
!
      end subroutine sel_write_grd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_udt_param(my_rank, istep_ucd, ucd)
!
      use ucd_field_file_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
        call read_alloc_ucd_2_fld_file(my_rank, istep_ucd, ucd)
!
      end subroutine sel_read_udt_param
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
!
      use ucd_field_file_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
        call read_alloc_ucd_2_fld_file(my_rank, istep_ucd, ucd)
!
      end subroutine sel_read_alloc_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_udt_file(my_rank, istep_ucd, ucd)
!
      use ucd_field_file_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
        call read_ucd_2_fld_file(my_rank, istep_ucd, ucd)
!
      end subroutine sel_read_udt_file
!
!------------------------------------------------------------------
!
      end module ucd_IO_select
