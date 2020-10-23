!>@file  gz_write_udt_file_IO_b.f90
!!       module gz_write_udt_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2006
!!@n      Modified  in May, 2015
!
!> @brief Output merged VTK or UCD  file usging MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_file_mpi_b(gzip_name, t_IO, ucd)
!!      subroutine gz_write_ucd_phys_mpi_b(gzip_name, t_IO, ucd)
!!      subroutine gz_write_ucd_grid_mpi_b(gzip_name, ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!@endverbatim
!
      module gz_write_udt_file_IO_b
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_field_file_format
!
      use t_time_data
      use t_ucd_data
      use t_calypso_mpi_IO_param
!
      use set_ucd_file_names
!
      implicit none
!
      type(calypso_MPI_IO_params), private :: IO_param
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_ucd_file_mpi_b(gzip_name, t_IO, ucd)
!
      use t_time_data
      use m_error_IDs
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
      use gz_ucd_file_MPI_IO
      use gz_field_block_MPI_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'write gzipped binary mesh and field data by MPI-IO: ',        &
     &    trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!
      call gz_mpi_write_ucd_mesh_data_b(IO_param,                       &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    ucd%istack_merged_intnod)
!
      call gz_write_field_time_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call gz_write_ucd_data_mpi_b(IO_param,                            &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_ucd_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_phys_mpi_b(gzip_name, t_IO, ucd)
!
      use t_time_data
      use m_error_IDs
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
      use gz_ucd_file_MPI_IO
      use gz_field_block_MPI_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'write gzipped binary field file by MPI-IO: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!
      call gz_write_field_time_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call gz_write_ucd_data_mpi_b(IO_param,                            &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_ucd_phys_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_grid_mpi_b(gzip_name, ucd)
!
      use m_machine_parameter
      use gz_MPI_binary_datum_IO
      use MPI_binary_head_IO
      use gz_ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary grid file by MPI-IO: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!
      call gz_mpi_write_ucd_mesh_data_b(IO_param,                       &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_ucd_grid_mpi_b
!
!  ---------------------------------------------------------------------
!
      end module gz_write_udt_file_IO_b
