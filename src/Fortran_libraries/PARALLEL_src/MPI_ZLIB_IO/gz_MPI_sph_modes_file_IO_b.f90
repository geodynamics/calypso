!>@file   gz_MPI_sph_modes_file_IO_b.f90
!!@brief  module gz_MPI_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_mpi_read_geom_rtp_file_b                          &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!      subroutine gz_mpi_read_spectr_rj_file_b                         &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!      subroutine gz_mpi_read_geom_rtm_file_b                          &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!      subroutine gz_mpi_read_modes_rlm_file_b                         &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine gz_mpi_write_geom_rtp_file_b                         &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!      subroutine gz_mpi_write_spectr_rj_file_b                        &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!      subroutine gz_mpi_write_geom_rtm_file_b                         &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!      subroutine gz_mpi_write_modes_rlm_file_b                        &
!!     &         (file_name, num_pe, id_rank, sph_file)
!!        type(sph_file_data_type), intent(in) :: sph_file
!!@endverbatim
!!
!!@param num_pe  Number of subdomain
!!@param id_rank    Domain ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_MPI_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_data_IO
      use t_calypso_mpi_IO_param
      use gz_MPI_sph_modes_data_IO_b
      use gz_MPI_binary_datum_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtp_file_b                            &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped merged binary grid file: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_geom_rtp_data_b(IO_param,                        &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_spectr_rj_file_b                           &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped merged binary spectr modes file: ',           &
     &       trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_spectr_rj_data_b(IO_param,                       &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtm_file_b                            &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped merged binary grid file: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_geom_rtm_data_b                                  &
     &   (IO_param, sph_file%comm_IO, sph_file%sph_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_modes_rlm_file_b                           &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read merged gzipped binary spectr modes file: ',           &
     &       trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_modes_rlm_data_b                                 &
     &   (IO_param, sph_file%comm_IO, sph_file%sph_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtp_file_b                          &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write merged gzipped binary grid file: ', trim(file_name)
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_geom_rtp_data_b(IO_param,                       &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_spectr_rj_file_b                          &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'gzipped merged binary spectr modes file: ',                &
     &       trim(file_name)
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_spectr_rj_data_b(IO_param,                      &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtm_file_b                           &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped merged binary grid file: ', trim(file_name)
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_geom_rtm_data_b                                 &
     &   (IO_param, sph_file%comm_IO, sph_file%sph_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_modes_rlm_file_b                          &
     &         (file_name, num_pe, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write gzipped merged binary spectr modes file: ',           &
     &      trim(file_name)
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_modes_rlm_data_b                                &
     &   (IO_param, sph_file%comm_IO, sph_file%sph_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_sph_modes_file_IO_b
