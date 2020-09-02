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
!!      subroutine sel_write_parallel_ucd_mesh(ucd_param, ucd)
!!
!!      subroutine sel_read_parallel_udt_file                           &
!!     &         (istep_ucd, ucd_param, t_IO, ucd)
!!      subroutine sel_read_alloc_para_udt_file                         &
!!     &         (istep_ucd, ucd_param, t_IO, ucd)
!!        integer(kind=kint), intent(in) :: istep_ucd
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
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
      use t_control_array_character
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
      ucd_param%iflag_format                                            &
     &        = choose_para_fld_file_format(file_format_ctl%charavalue, &
     &                                      file_format_ctl%iflag)
!
      end subroutine set_merged_ucd_file_ctl
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_file                            &
     &         (istep_ucd, ucd_param, t_IO, ucd)
!
      use ucd_IO_select
      use vtk_file_IO
      use merged_udt_vtk_file_IO
      use ucd_field_MPI_IO
      use ucd_field_MPI_IO_b
!
#ifdef ZLIB_IO
      use gz_ucd_field_MPI_IO
      use gz_ucd_field_MPI_IO_b
      use gz_merged_udt_vtk_file_IO
      use gz_vtk_file_IO
#endif
#ifdef HDF5_IO
      use hdf5_file_IO
#endif
!
      integer(kind=kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(istep_ucd .lt. 0) return
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, my_rank, istep_ucd)
!
      if     (ucd_param%iflag_format .eq. iflag_single) then
        call write_ucd_field_file_mpi(file_name, t_IO, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_bin) then
        call write_ucd_field_file_mpi_b(file_name, t_IO, ucd)
!
      else if(ucd_param%iflag_format .eq. iflag_sgl_vtk) then
        call write_vtk_file_mpi(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtd) then
        call write_vtk_phys_mpi(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_ucd) then
        call write_ucd_file_mpi(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_udt) then
        call write_ucd_phys_mpi(file_name, ucd)
!
      else if(ucd_param%iflag_format .eq. iflag_ucd_bin                 &
     &   .or. ucd_param%iflag_format .eq. iflag_sgl_ucd_bin) then
        call write_ucd_file_mpi_b(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_bin                 &
     &   .or. ucd_param%iflag_format .eq. iflag_sgl_udt_bin) then
        call write_ucd_phys_mpi_b(file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_gz) then
        call gz_write_ucd_field_file_mpi(file_name, t_IO, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_bin_gz) then
        call gz_write_ucd_field_file_mpi_b(file_name, t_IO, ucd)
!
      else if(ucd_param%iflag_format .eq. iflag_ucd_bin_gz              &
     &   .or. ucd_param%iflag_format .eq. iflag_sgl_ucd_bin_gz) then
        call gz_write_ucd_file_mpi_b(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_bin_gz              &
     &   .or. ucd_param%iflag_format .eq. iflag_sgl_udt_bin_gz) then
        call gz_write_ucd_phys_mpi_b(file_name, ucd)
!
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtk_gz) then
        call gz_write_vtk_file_mpi(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtd_gz) then
        call gz_write_vtk_phys_mpi(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_ucd_gz) then
        call gz_write_ucd_file_mpi(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_sgl_udt_gz) then
        call gz_write_ucd_phys_mpi(file_name, ucd)
!
      else if(ucd_param%iflag_format .eq. iflag_vtk_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_ucd,     &
     &      ucd_param%file_prefix)
        call write_gz_vtk_file(my_rank, file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_vtd_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_ucd,     &
     &      ucd_param%file_prefix)
        call write_gz_vtk_phys(my_rank, file_name, ucd)
#endif
!
#ifdef HDF5_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_field_file                             &
     &     (ucd_param%file_prefix, istep_ucd, ucd)
        call parallel_write_xdmf_snap_file                              &
     &     (ucd_param%file_prefix, istep_ucd, t_IO, ucd)
        call parallel_write_xdmf_evo_file                               &
     &     (ucd_param%file_prefix, istep_ucd, t_IO, ucd)
#endif
!
      else if(ucd_param%iflag_format .eq. iflag_vtk) then
        call write_parallel_vtk_file                                    &
     &     (my_rank, nprocs, istep_ucd, ucd_param%file_prefix)
        call write_vtk_file(my_rank, file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_vtd) then
        call write_parallel_vtk_file                                    &
     &     (my_rank, nprocs, istep_ucd, ucd_param%file_prefix)
        call write_vtk_phys(my_rank, file_name, ucd)
      else
!
        call sel_write_ucd_file                                         &
     &     (my_rank, istep_ucd, ucd_param, t_IO, ucd)
      end if
!
      end subroutine sel_write_parallel_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_mesh(ucd_param, ucd)
!
      use ucd_IO_select
      use merged_udt_vtk_file_IO
!
      use gz_merged_udt_vtk_file_IO
      use hdf5_file_IO
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_grd_file_name                            &
     &        (ucd_param%file_prefix, ucd_param%iflag_format, my_rank)
!
      if(     (ucd_param%iflag_format .eq. iflag_single)                &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_bin)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_gz)                &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_bin_gz)) then
        return
!
      else if(ucd_param%iflag_format .eq. iflag_sgl_vtd) then
        call write_vtk_grid_mpi(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_udt) then
        call write_ucd_grid_mpi(file_name, ucd)
!
      else if(ucd_param%iflag_format .eq. iflag_udt_bin                 &
     &   .or. ucd_param%iflag_format .eq. iflag_sgl_udt_bin) then
        call write_ucd_grid_mpi_b(file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_udt_bin_gz              &
     &   .or. ucd_param%iflag_format .eq. iflag_sgl_udt_bin_gz) then
        call gz_write_ucd_grid_mpi_b(file_name, ucd)
!
      else if (ucd_param%iflag_format .eq. iflag_sgl_vtd_gz) then
        call gz_write_vtk_grid_mpi(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_udt_gz) then
        call gz_write_ucd_grid_mpi(file_name, ucd)
#endif
!
#ifdef HDF5_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_mesh_file                              &
     &     (ucd_param%file_prefix, ucd)
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
      subroutine sel_read_parallel_udt_file                             &
     &         (istep_ucd, ucd_param, t_IO, ucd)
!
      use ucd_IO_select
      use ucd_field_MPI_IO
      use ucd_field_MPI_IO_b
!
#ifdef ZLIB_IO
      use gz_ucd_field_MPI_IO
      use gz_ucd_field_MPI_IO_b
#endif
!
      integer(kind=kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(istep_ucd .lt. 0) return
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, my_rank, istep_ucd)
!
      if(     (ucd_param%iflag_format .eq. iflag_sgl_vtk)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_vtd)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd_bin)           &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_bin)           &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd_bin_gz)        &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_bin_gz)        &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_bin_gz)        &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_vtk_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_vtd_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_hdf5)) then
        return
!
      else if(ucd_param%iflag_format .eq. iflag_single) then
        call read_ucd_field_file_mpi                                    &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_bin) then
        call read_ucd_field_file_mpi_b                                  &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_gz) then
        call gz_read_ucd_field_file_mpi                                 &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_bin_gz) then
        call gz_read_ucd_field_file_mpi_b                               &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
#endif
      else
        call sel_read_udt_file                                          &
     &     (my_rank, istep_ucd, ucd_param, t_IO, ucd)
      end if
!
      end subroutine sel_read_parallel_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_para_udt_file                           &
     &         (istep_ucd, ucd_param, t_IO, ucd)
!
      use ucd_IO_select
      use ucd_field_MPI_IO
      use ucd_field_MPI_IO_b
!
#ifdef ZLIB_IO
      use gz_ucd_field_MPI_IO
      use gz_ucd_field_MPI_IO_b
#endif
!
      integer(kind=kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(istep_ucd .lt. 0) return
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, my_rank, istep_ucd)
!
      if(     (ucd_param%iflag_format .eq. iflag_sgl_vtk)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_vtd)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd)               &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd_bin)           &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_bin)           &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd_bin_gz)        &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_bin_gz)        &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_bin_gz)        &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_vtk_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_vtd_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_ucd_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_udt_gz)            &
     &   .or. (ucd_param%iflag_format .eq. iflag_sgl_hdf5)) then
        return
!
      else if(ucd_param%iflag_format .eq. iflag_single) then
        call read_alloc_ucd_fld_file_mpi                                &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_bin) then
        call read_alloc_ucd_fld_file_mpi_b                              &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_sgl_gz) then
        call gz_read_alloc_ucd_fld_file_mpi                             &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
      else if(ucd_param%iflag_format .eq. iflag_sgl_bin_gz) then
        call gz_rd_alloc_ucd_fld_file_mpi_b                             &
     &     (file_name, nprocs, my_rank, t_IO, ucd)
#endif
      else
        call sel_read_alloc_udt_file                                    &
     &     (my_rank, istep_ucd, ucd_param, t_IO, ucd)
      end if
!
      end subroutine sel_read_alloc_para_udt_file
!
!------------------------------------------------------------------
!
      end module parallel_ucd_IO_select
