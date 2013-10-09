!>@file   parallel_ucd_IO_select.F90
!!@brief  module parallel_ucd_IO_select
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    Modified in May,  2009
!!@n    Modified in June, 2013
!!
!>@brief Select field data output routine including merged field data
!!
!!
!!@verbatim
!!      subroutine set_control_parallel_field_def
!!      subroutine sel_write_parallel_ucd_file(istep_ucd)
!!      subroutine sel_write_parallel_ucd_mesh
!!@endverbatim
!!
!!@param istep_ucd  setp number for field data output
!
!
      module parallel_ucd_IO_select
!
      use m_precision
      use calypso_mpi
      use m_file_format_switch
      use m_field_file_format
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
      subroutine set_control_parallel_field_def
!
      use m_ctl_data_4_platforms
      use m_ucd_data
!
!
      itype_ucd_data_file = i_udt_header
      if (i_udt_header .gt. 0) then
        ucd_header_name = udt_file_head_ctl
      end if
!
      call choose_para_fld_file_format(udt_file_fmt_ctl,                &
     &    i_udt_files_fmt, itype_ucd_data_file)
!
      end subroutine set_control_parallel_field_def
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_file(istep_ucd)
!
      use m_ucd_data
      use hdf5_file_IO
      use m_t_step_parameter
!
      use write_ucd_to_vtk_file
      use merged_udt_vtk_file_IO
!
      integer(kind=kint), intent(in) :: istep_ucd
!
!
      if (itype_ucd_data_file .eq. iflag_sgl_vtk) then
        call write_merged_vtk_file(istep_ucd)
#ifdef HAVE_HDF5
      else if(itype_ucd_data_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_field_file(ucd_header_name, istep_ucd)
        call parallel_write_xdmf_file(ucd_header_name, istep_ucd, time)
#endif
      else if(itype_ucd_data_file .eq. iflag_vtk) then
        call write_parallel_vtk_file(my_rank, nprocs, istep_ucd)
        call write_udt_data_2_vtk_file(my_rank, istep_ucd)
      end if
!
      end subroutine sel_write_parallel_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_mesh
!
      use m_ucd_data
      use hdf5_file_IO
!
      use merged_udt_vtk_file_IO
!
!
      if(itype_ucd_data_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_grid
#ifdef HAVE_HDF5
      else if(itype_ucd_data_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_mesh_file(ucd_header_name)
#endif
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
        id_field_file_format = iflag_sgl_vtk
        return
      end if
!
      if(file_fmt_ctl.eq.'single_vtk'                                   &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK'                              &
     &   .or. file_fmt_ctl.eq.'single_VTK'                              &
     &   .or. file_fmt_ctl.eq.'single_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_vtk'                              &
     &   .or. file_fmt_ctl.eq.'merged_VTK'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK'                              &
     &   .or. file_fmt_ctl.eq.'merged_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_ASCII') then
           id_field_file_format = iflag_sgl_vtk
      else if(file_fmt_ctl.eq.'single_vtk_gzip'                         &
     &   .or. file_fmt_ctl.eq.'single_VTK_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_vtk_gzip'                         &
     &   .or. file_fmt_ctl.eq.'merged_VTK_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_vtk_gz'                           &
     &   .or. file_fmt_ctl.eq.'single_VTK_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_vtk_gz'                           &
     &   .or. file_fmt_ctl.eq.'merged_VTK_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_GZ') then
           id_field_file_format = iflag_sgl_vtk + iflag_gzip
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
