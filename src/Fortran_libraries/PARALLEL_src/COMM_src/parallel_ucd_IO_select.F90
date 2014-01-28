!parallel_ucd_IO_select.F90
!      module parallel_ucd_IO_select
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!!      subroutine set_merged_ucd_file_define(ucd)
!!
!!      subroutine sel_write_parallel_ucd_file(istep_ucd, ucd, m_ucd)
!!      subroutine sel_write_parallel_ucd_mesh(ucd, m_ucd)
!!
!
      module parallel_ucd_IO_select
!
      use m_precision
      use m_file_format_switch
      use m_field_file_format
!
      use t_ucd_data
!
      implicit none
!
      private :: choose_para_fld_file_format
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_merged_ucd_file_define(ucd)
!
      use m_ctl_data_4_platforms
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%ifmt_file = i_udt_header
      if(i_udt_header .gt. 0) ucd%file_prefix = udt_file_head_ctl
!
      call choose_para_fld_file_format(udt_file_fmt_ctl,                &
     &    i_udt_files_fmt, ucd%ifmt_file)
!
      end subroutine set_merged_ucd_file_define
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_file(istep_ucd, ucd, m_ucd)
!
      use ucd_IO_select
      use write_ucd_to_vtk_file
      use merged_udt_vtk_file_IO
!
      use hdf5_file_IO
!
      integer(kind=kint), intent(in) :: istep_ucd
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if      (ucd%ifmt_file .eq. iflag_sgl_vtk) then
        call write_merged_vtk_file(istep_ucd, ucd, m_ucd)
!
#ifdef HDF5_IO
      else if(ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_field_file(istep_ucd, ucd, m_ucd)
        call parallel_write_xdmf_snap_file(istep_ucd, ucd, m_ucd)
        call parallel_write_xdmf_evo_file(istep_ucd, ucd, m_ucd)
#endif
!
      else
        call write_parallel_vtk_file(my_rank, nprocs, istep_ucd, ucd)
        call write_udt_data_2_vtk_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_write_parallel_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_mesh(ucd, m_ucd)
!
      use merged_udt_vtk_file_IO
!
      use hdf5_file_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if(ucd%ifmt_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_grid(ucd, m_ucd)
!
#ifdef HDF5_IO
      else if(ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_mesh_file(ucd, m_ucd)
#endif
!
      end if
!
      end subroutine sel_write_parallel_ucd_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine choose_para_fld_file_format(file_fmt_ctl, i_file_fmt,  &
     &          id_field_file_format)
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(inout) :: id_field_file_format
!
!
      if (i_file_fmt .eq. 0) then
        id_field_file_format = iflag_udt
        return
      end if
!
      if(     file_fmt_ctl.eq.'merged_vtk'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK'                              &
     &   .or. file_fmt_ctl.eq.'merged_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_ASCII') then
           id_field_file_format = iflag_sgl_vtk
      else if(file_fmt_ctl.eq.'merged_hdf5'                             &
     &   .or. file_fmt_ctl.eq.'merged_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'Merged_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'MERGED_HDF5') then
           id_field_file_format = iflag_sgl_hdf5
      else
        call choose_ucd_file_format(file_fmt_ctl, i_file_fmt,           &
     &          id_field_file_format)
      end if
!
      end subroutine choose_para_fld_file_format
!
! -----------------------------------------------------------------------
!
      end module parallel_ucd_IO_select
