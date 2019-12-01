!>@file   viewer_mesh_MPI_IO_select.F90
!!@brief  module viewer_mesh_MPI_IO_select
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief  Viewer mesh file IO selector
!!
!!@verbatim
!!      subroutine sel_mpi_output_surface_grid                          &
!!     &         (ifmt_file, mgd_v_mesh, mgd_view_prm)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(merged_viewer_mesh), intent(in) :: mgd_v_mesh
!!        type(mpi_viewer_mesh_param), intent(in) :: mgd_view_prm
!!@endverbatim
!
      module viewer_mesh_MPI_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_merged_viewer_mesh
      use t_file_IO_parameter
!
      use MPI_viewer_mesh_file_IO
      use set_parallel_file_name
!
#ifdef ZLIB_IO
      use gz_MPI_viewer_mesh_file_IO
#endif
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_output_surface_grid                            &
     &         (mesh_file, mgd_v_mesh, mgd_view_prm)
!
      use set_mesh_extensions
!
      type(field_IO_params), intent(in) :: mesh_file
      type(merged_viewer_mesh), intent(in) :: mgd_v_mesh
      type(mpi_viewer_mesh_param), intent(in) :: mgd_view_prm
!
      character(len = kchara) :: file_name
!
!
      file_name = add_ksm_extension(mesh_file%file_prefix)
!
#ifdef ZLIB_IO
      if(mod(mesh_file%iflag_format,10) .eq. id_gzip_txt_file_fmt) then
        call gz_mpi_write_viewer_mesh_file                              &
     &     (file_name, mgd_v_mesh, mgd_view_prm)
        return
      end if
#endif
!
      call mpi_write_viewer_mesh_file                                   &
     &   (file_name, mgd_v_mesh, mgd_view_prm)
!
      end subroutine sel_mpi_output_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_mesh_MPI_IO_select
