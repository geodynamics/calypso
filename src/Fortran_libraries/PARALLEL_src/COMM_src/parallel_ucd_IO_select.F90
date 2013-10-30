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
!!      subroutine set_merged_ucd_file_define(ucd)
!!
!!      subroutine sel_write_parallel_ucd_file(istep_ucd, ucd, m_ucd)
!!      subroutine sel_write_parallel_ucd_mesh(ucd, m_ucd)
!!@endverbatim
!!
!!@param istep_ucd  setp number for field data output
!
      module parallel_ucd_IO_select
!
      use m_precision
      use m_file_format_switch
      use m_field_file_format
!
      use calypso_mpi
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
      use gz_merged_udt_vtk_file_IO
      use gz_write_ucd_to_vtk_file
      use hdf5_file_IO
!
      integer(kind=kint), intent(in) :: istep_ucd
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if      (ucd%ifmt_file .eq. iflag_sgl_vtk) then
        call write_merged_vtk_file(istep_ucd, ucd, m_ucd)
      else if (ucd%ifmt_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_phys(istep_ucd, ucd, m_ucd)
      else if (ucd%ifmt_file .eq. iflag_sgl_ucd) then
        call write_merged_ucd_file(istep_ucd, ucd, m_ucd)
      else if (ucd%ifmt_file .eq. iflag_sgl_udt) then
        call write_merged_udt_file(istep_ucd, ucd, m_ucd)
!
#ifdef ZLIB_IO
      else if (ucd%ifmt_file .eq. iflag_sgl_vtk_gz) then
        call write_gz_merged_vtk_file(istep_ucd, ucd, m_ucd)
      else if (ucd%ifmt_file .eq. iflag_sgl_vtd_gz) then
        call write_gz_merged_vtk_phys(istep_ucd, ucd, m_ucd)
      else if (ucd%ifmt_file .eq. iflag_sgl_ucd_gz) then
        call write_gz_merged_ucd_file(istep_ucd, ucd, m_ucd)
      else if (ucd%ifmt_file .eq. iflag_sgl_udt_gz) then
        call write_gz_merged_udt_file(istep_ucd, ucd, m_ucd)
!
      else if(ucd%ifmt_file .eq. iflag_vtk_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_ucd,     &
     &      ucd)
        call write_ucd_data_2_gz_vtk(my_rank, istep_ucd, ucd)
      else if (ucd%ifmt_file .eq. iflag_vtd_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_ucd,     &
     &      ucd)
        call write_ucd_data_2_gz_vtk_phys(my_rank, istep_ucd, ucd)
#endif
!
#ifdef HDF5_IO
      else if(ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_field_file(istep_ucd, ucd, m_ucd)
        call parallel_write_xdmf_snap_file(istep_ucd, ucd, m_ucd)
        call parallel_write_xdmf_evo_file(istep_ucd, ucd, m_ucd)
#endif
!
      else if(ucd%ifmt_file .eq. iflag_vtk) then
        call write_parallel_vtk_file(my_rank, nprocs, istep_ucd, ucd)
        call write_udt_data_2_vtk_file(my_rank, istep_ucd, ucd)
      else if (ucd%ifmt_file .eq. iflag_vtd) then
        call write_parallel_vtk_file(my_rank, nprocs, istep_ucd, ucd)
        call write_udt_data_2_vtk_phys(my_rank, istep_ucd, ucd)
      else
        call sel_write_ucd_file(my_rank, istep_ucd, ucd)
      end if
!
      end subroutine sel_write_parallel_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_mesh(ucd, m_ucd)
!
      use ucd_IO_select
      use write_ucd_to_vtk_file
      use merged_udt_vtk_file_IO
!
      use gz_merged_udt_vtk_file_IO
      use gz_write_ucd_to_vtk_file
      use hdf5_file_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if(ucd%ifmt_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_grid(ucd, m_ucd)
      else if(ucd%ifmt_file .eq. iflag_sgl_udt) then
        call write_merged_grd_file(ucd, m_ucd)
!
#ifdef ZLIB_IO
      else if (ucd%ifmt_file .eq. iflag_sgl_vtd_gz) then
        call write_gz_merged_vtk_grid(ucd, m_ucd)
      else if(ucd%ifmt_file .eq. iflag_sgl_udt_gz) then
        call write_gz_merged_grd_file(ucd, m_ucd)
#endif
!
#ifdef HDF5_IO
      else if(ucd%ifmt_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_mesh_file(ucd, m_ucd)
#endif
!
      else
        call sel_write_grd_file(my_rank, ucd)
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
      use skip_comment_f
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
      if     (cmp_no_case(file_fmt_ctl, 'merged_UDT_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_ascii') .gt.     0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_UDT') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged') .gt.     0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged') .gt.           0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UDT_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_UDT') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_ascii') .gt.     0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single') .gt.     0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single') .gt.     0) then
           id_field_file_format = iflag_sgl_udt
      else if(cmp_no_case(file_fmt_ctl, 'merged_UDT_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UDT_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_gzip') .gt.     0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_gz') .gt.       0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_UDT') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_UDT') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged') .gt.     0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged') .gt.       0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UDT_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UDT_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_UDT') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_UDT') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_gzip') .gt.     0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_gz') .gt.       0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_sigle') .gt.      0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single') .gt.       0) then
           id_field_file_format = iflag_sgl_udt + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'merged_UCD_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_UCD') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UCD') .gt.       0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_UCD') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD') .gt.       0      &
     &       ) then
           id_field_file_format = iflag_sgl_ucd
      else if(cmp_no_case(file_fmt_ctl, 'merged_UCD_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UCD_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_UCD') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_UCD') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_UCD') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_UCD') .gt.   0) then
           id_field_file_format = iflag_sgl_ucd + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTD_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_VTD') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTD') .gt.       0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_VTD') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD') .gt.       0      &
     &       ) then
           id_field_file_format = iflag_sgl_vtd
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTD_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTD_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_VTD') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_VTD') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_VTD') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_VTD') .gt.   0) then
           id_field_file_format = iflag_sgl_vtd + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTK_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_VTK') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTK') .gt.       0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK_ascii') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_VTK') .gt. 0      &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK') .gt.       0      &
     &       ) then
           id_field_file_format = iflag_sgl_vtk
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTK_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTK_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_VTK') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_VTK') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK_gzip') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK_gz') .gt.   0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_VTK') .gt. 0       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_VTK') .gt.   0) then
           id_field_file_format = iflag_sgl_vtk + iflag_gzip
      else if(cmp_no_case(file_fmt_ctl, 'merged_HDF5') .gt. 0           &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_HDF5') .gt. 0) then
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
