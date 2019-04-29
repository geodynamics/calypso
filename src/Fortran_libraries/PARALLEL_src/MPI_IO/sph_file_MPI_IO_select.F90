!>@file   sph_file_MPI_IO_select.f90
!!@brief  module sph_file_MPI_IO_select
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine sel_mpi_read_geom_rtp_file                           &
!!     &         (num_pe, id_rank, sph_file)
!!      subroutine sel_mpi_read_spectr_rj_file                          &
!!     &         (num_pe, id_rank, sph_file)
!!      subroutine sel_mpi_read_geom_rtm_file                           &
!!     &         (num_pe, id_rank, sph_file)
!!      subroutine sel_mpi_read_modes_rlm_file                          &
!!     &         (num_pe, id_rank, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine sel_mpi_write_geom_rtp_file                          &
!!     &         (num_pe, id_rank, sph_file)
!!      subroutine sel_mpi_write_spectr_rj_file                         &
!!     &         (num_pe, id_rank, sph_file)
!!      subroutine sel_mpi_write_geom_rtm_file                          &
!!     &         (num_pe, id_rank, sph_file)
!!      subroutine sel_mpi_write_modes_rlm_file                         &
!!     &         (num_pe, id_rank, sph_file)
!!        type(sph_file_data_type), intent(in) :: sph_file
!!@endverbatim
!!
!!@param sph_file_name  file name for IO (.gz is appended in this module)
!
      module sph_file_MPI_IO_select
!
      use m_precision
!
      use calypso_mpi
      use m_file_format_switch
      use t_spheric_data_IO
!
      use set_parallel_file_name
      use set_mesh_file_names
      use sph_file_IO_select
!
      use MPI_sph_modes_file_IO
      use MPI_sph_modes_file_IO_b
!
#ifdef ZLIB_IO
      use gz_MPI_sph_modes_file_IO
      use gz_MPI_sph_modes_file_IO_b
#endif
!
      implicit none
!
      character(len=kchara), private :: sph_file_name
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_geom_rtp_file                            &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer(kind = kint) :: ierr = 0
!
!
      sph_file_name = set_sph_rtp_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_geom_rtp_file_b                                   &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_read_geom_rtp_file                                     &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geom_rtp_file_b                                &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geom_rtp_file                                  &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_read_geom_rtp_file(id_rank, sph_file, ierr)
        end if
      end if
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Spectr RTP data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_spectr_rj_file                            &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rj_file_name                              &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_spectr_rj_file_b                                  &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_read_spectr_rj_file                                    &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_spectr_rj_file_b                               &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_spectr_rj_file                                 &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_read_spectr_rj_file(id_rank, sph_file, ierr)
        end if
      end if
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Spectr RJ data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_geom_rtm_file                             &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rtm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_geom_rtm_file_b                                   &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_read_geom_rtm_file                                     &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geom_rtm_file_b                                &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geom_rtm_file                                  &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_read_geom_rtm_file(id_rank, sph_file, ierr)
        end if
      end if
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Spectr RTM data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_modes_rlm_file                            &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rlm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_modes_rlm_file_b                                  &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_read_modes_rlm_file                                    &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_modes_rlm_file_b                               &
     &     (sph_file_name, num_pe, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_modes_rlm_file                                 &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_read_modes_rlm_file(id_rank, sph_file, ierr)
        end if
      end if
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Spectr RLM data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_geom_rtp_file                            &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rtp_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_geom_rtp_file_b                                  &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_write_geom_rtp_file                                    &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_geom_rtp_file_b                               &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_geom_rtp_file                                 &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_write_geom_rtp_file(id_rank, sph_file, ierr)
        end if
      end if
!
      end subroutine sel_mpi_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_spectr_rj_file                           &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rj_file_name                              &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_spectr_rj_file_b                                 &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_write_spectr_rj_file                                   &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_spectr_rj_file_b                              &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_spectr_rj_file                                &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_write_spectr_modes_rj_file(id_rank, sph_file, ierr)
        end if
      end if
!
      end subroutine sel_mpi_write_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_geom_rtm_file                            &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rtm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_geom_rtm_file_b                                  &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_write_geom_rtm_file                                    &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_geom_rtm_file_b                               &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_geom_rtm_file                                 &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_write_geom_rtm_file(id_rank, sph_file, ierr)
        end if
      end if
!
      end subroutine sel_mpi_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_modes_rlm_file                           &
     &         (num_pe, id_rank, sph_file)
!
      integer, intent(in) :: num_pe, id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
      integer(kind = kint) ::  ierr = 0
!
!
      sph_file_name = set_sph_rlm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_modes_rlm_file_b                                 &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_write_modes_rlm_file                                   &
     &     (sph_file_name, num_pe, id_rank, sph_file)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_modes_rlm_file_b                              &
     &     (sph_file_name, nprocs, id_rank, sph_file)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_modes_rlm_file                                &
     &     (sph_file_name, num_pe, id_rank, sph_file)
#endif
!
      else
        if(id_rank .lt. num_pe) then
          call sel_write_modes_rlm_file(id_rank, sph_file, ierr)
        end if
      end if
!
      end subroutine sel_mpi_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_file_MPI_IO_select
