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
!!      subroutine set_merged_ucd_file_define(plt, ucd_param)
!!      subroutine set_merged_ucd_file_ctl(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl, ucd_param)
!!        type(platform_data_control), intent(in) :: plt
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine sel_write_parallel_ucd_file                          &
!!     &         (istep_ucd, ucd_param, t_IO, ucd, m_ucd)
!!      subroutine sel_write_parallel_ucd_mesh(ucd_param, ucd, m_ucd)
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
      subroutine set_merged_ucd_file_define(plt, ucd_param)
!
      use t_ctl_data_4_platforms
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: ucd_param
!
      character(len=kchara), parameter :: default_ucd_prefix = 'out'
!
!
      call set_merged_ucd_file_ctl(default_ucd_prefix,                  &
     &    plt%field_file_prefix, plt%field_file_fmt_ctl, ucd_param)
!
      end subroutine set_merged_ucd_file_define
!
! -----------------------------------------------------------------------
!
      subroutine set_merged_ucd_file_ctl(default_prefix,                &
     &          file_prefix_ctl, file_format_ctl, ucd_param)
!
      use t_control_elements
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      ucd_param%iflag_IO = file_prefix_ctl%iflag
      if(ucd_param%iflag_IO .eq. 0) then
        ucd_param%iflag_format = -1
        ucd_param%file_prefix = default_prefix
        return
      else
        ucd_param%file_prefix = file_prefix_ctl%charavalue
      end if
!
      call choose_para_fld_file_format                                  &
     &   (file_format_ctl%charavalue, file_format_ctl%iflag,            &
     &    ucd_param%iflag_format)
!
      end subroutine set_merged_ucd_file_ctl
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_file                            &
     &         (istep_ucd, ucd_param, t_IO, ucd, m_ucd)
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
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if( (ucd_param%iflag_format/icent) .eq. 1) then
        call set_single_ucd_file_name                                   &
     &     (ucd_param%file_prefix, ucd_param%iflag_format,              &
     &      istep_ucd, file_name)
      else
        call set_parallel_ucd_file_name                                 &
     &     (ucd_param%file_prefix, ucd_param%iflag_format,              &
     &      my_rank, istep_ucd, file_name)
      end if
!
      if      (ucd_param%iflag_format .eq. iflag_sgl_vtk) then
        call write_vtk_file_mpi(file_name, ucd, m_ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtd) then
        call write_vtk_phys_mpi(file_name, ucd, m_ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_ucd) then
        call write_ucd_file_mpi(file_name, ucd, m_ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_udt) then
        call write_ucd_phys_mpi(file_name, ucd, m_ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtk_gz) then
        call gz_write_vtk_file_mpi(file_name, ucd, m_ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtd_gz) then
        call gz_write_vtk_phys_mpi(file_name, ucd, m_ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_ucd_gz) then
        call gz_write_ucd_file_mpi(file_name, ucd, m_ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_udt_gz) then
        call gz_write_ucd_phys_mpi(file_name, ucd, m_ucd)
!
      else if(ucd_param%iflag_format .eq. iflag_vtk_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_ucd,     &
     &      ucd_param%file_prefix)
        call write_ucd_data_2_gz_vtk(my_rank, file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_vtd_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_ucd,     &
     &      ucd_param%file_prefix)
        call write_ucd_data_2_gz_vtk_phys(my_rank, file_name, ucd)
#endif
!
#ifdef HDF5_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_field_file                             &
     &     (ucd_param%file_prefix, istep_ucd, ucd, m_ucd)
        call parallel_write_xdmf_snap_file                              &
     &     (ucd_param%file_prefix, istep_ucd, t_IO, ucd, m_ucd)
        call parallel_write_xdmf_evo_file                               &
     &     (ucd_param%file_prefix, istep_ucd, t_IO, ucd, m_ucd)
#endif
!
      else if(ucd_param%iflag_format .eq. iflag_vtk) then
        call write_parallel_vtk_file                                    &
     &     (my_rank, nprocs, istep_ucd, ucd_param%file_prefix)
        call write_udt_data_2_vtk_file(my_rank, file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_vtd) then
        call write_parallel_vtk_file                                    &
     &     (my_rank, nprocs, istep_ucd, ucd_param%file_prefix)
        call write_udt_data_2_vtk_phys(my_rank, file_name, ucd)
      else
        call sel_write_ucd_file                                         &
     &     (my_rank, istep_ucd, ucd_param, t_IO, ucd)
      end if
!
      end subroutine sel_write_parallel_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_mesh(ucd_param, ucd, m_ucd)
!
      use ucd_IO_select
      use write_ucd_to_vtk_file
      use merged_udt_vtk_file_IO
!
      use gz_merged_udt_vtk_file_IO
      use gz_write_ucd_to_vtk_file
      use hdf5_file_IO
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
      character(len=kchara) :: file_name
!
!
      if( (ucd_param%iflag_format/icent) .eq. 1) then
        call set_single_grd_file_name                                   &
     &     (ucd_param%file_prefix, ucd_param%iflag_format, file_name)
      end if
!
      if(ucd_param%iflag_format .eq. iflag_sgl_vtd) then
        call write_vtk_grid_mpi(file_name, ucd, m_ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_udt) then
        call write_ucd_grid_mpi(file_name, ucd, m_ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtd_gz) then
        call gz_write_vtk_grid_mpi(file_name, ucd, m_ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_udt_gz) then
        call gz_write_ucd_grid_mpi(file_name, ucd, m_ucd)
#endif
!
#ifdef HDF5_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_mesh_file                              &
     &     (ucd_param%file_prefix, ucd, m_ucd)
#endif
!
      else
        call sel_write_grd_file(my_rank, ucd_param, ucd)
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
      use t_control_elements
      use skip_comment_f
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
      if     (cmp_no_case(file_fmt_ctl, 'merged_UDT_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UDT')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_ascii')                 &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_UDT')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged')                 &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UDT_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_UDT')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_ascii')                 &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single')                 &
     &   .or. cmp_no_case(file_fmt_ctl, 'single')          ) then
           id_field_file_format = iflag_sgl_udt
      else if(cmp_no_case(file_fmt_ctl, 'merged_UDT_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UDT_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_gzip')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_gz')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_UDT')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_UDT')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UDT_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UDT_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_UDT')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_UDT')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_gzip')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_gz')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_sigle')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single')             ) then
           id_field_file_format = iflag_sgl_udt + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'merged_UCD_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_UCD')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UCD')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_UCD')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD')       ) then
           id_field_file_format = iflag_sgl_ucd
      else if(cmp_no_case(file_fmt_ctl, 'merged_UCD_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_UCD_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_UCD')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_UCD')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_UCD_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_UCD')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_UCD')     ) then
           id_field_file_format = iflag_sgl_ucd + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTD_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_VTD')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTD')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_VTD')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD')       ) then
           id_field_file_format = iflag_sgl_vtd
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTD_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTD_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_VTD')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_VTD')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTD_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_VTD')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_VTD')     ) then
           id_field_file_format = iflag_sgl_vtd + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTK_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_merged_VTK')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTK')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK_ascii')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_single_VTK')             &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK')        ) then
           id_field_file_format = iflag_sgl_vtk
      else if(cmp_no_case(file_fmt_ctl, 'merged_VTK_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'merged_VTK_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_VTK')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_merged_VTK')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK_gzip')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_VTK_gz')                &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_single_VTK')              &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_single_VTK')     ) then
           id_field_file_format = iflag_sgl_vtk + iflag_gzip
      else if(cmp_no_case(file_fmt_ctl, 'merged_HDF5')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'single_HDF5')       ) then
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
