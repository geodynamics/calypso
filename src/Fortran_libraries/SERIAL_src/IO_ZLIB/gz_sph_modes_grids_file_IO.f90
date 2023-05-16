!>@file   gz_sph_modes_grids_file_IO.f90
!!@brief  module gz_sph_modes_grids_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Spectr data IO routines using zlib
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_file                                &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine gz_read_spectr_modes_rj_file                         &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine gz_read_geom_rtm_file                                &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine gz_read_modes_rlm_file                               &
!!     &         (file_name, id_rank, sph_file, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine gz_write_geom_rtp_file                               &
!!     &         (file_name, id_rank, sph_file)
!!      subroutine gz_write_spectr_modes_rj_file                        &
!!     &         (file_name, id_rank, sph_file)
!!      subroutine gz_write_geom_rtm_file                               &
!!     &         (file_name, id_rank, sph_file)
!!      subroutine gz_write_modes_rlm_file                              &
!!     &         (file_name, id_rank, sph_file)
!!        type(sph_file_data_type), intent(in) :: sph_file
!!@endverbatim
!!
!!@param id_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_grids_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_data_IO
      use set_parallel_file_name
      use gz_sph_modes_grids_data_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_s
      character, pointer, private, save :: FPz_sph
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtp_file                                  &
     &         (file_name, id_rank, sph_file, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_sph, file_name, zbuf_s)
      call read_geom_rtp_data_gz(FPz_sph, id_rank,                      &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO,       &
     &    zbuf_s, ierr)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_file                           &
     &         (file_name, id_rank, sph_file, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_sph, file_name, zbuf_s)
      call read_spectr_modes_rj_data_gz(FPz_sph, id_rank,               &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO,       &
     &    zbuf_s, ierr)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_file                                  &
     &         (file_name, id_rank, sph_file, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_sph, file_name, zbuf_s)
      call read_geom_rtm_data_gz(FPz_sph, id_rank, sph_file%comm_IO,    &
     &    sph_file%sph_IO, zbuf_s, ierr)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_file                                 &
     &         (file_name, id_rank, sph_file, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_sph, file_name, zbuf_s)
      call read_spectr_modes_rlm_data_gz(FPz_sph, id_rank,              &
     &    sph_file%comm_IO, sph_file%sph_IO, zbuf_s, ierr)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_file                                 &
     &         (file_name, id_rank, sph_file)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped grid file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_sph, file_name, zbuf_s)
      call write_geom_rtp_data_gz                                       &
     &   (FPz_sph, id_rank, sph_file%comm_IO, sph_file%sph_IO,          &
     &    sph_file%sph_grp_IO, zbuf_s)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_file                          &
     &         (file_name, id_rank, sph_file)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped spectr modes file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_sph, file_name, zbuf_s)
      call write_spectr_modes_rj_data_gz                                &
     &   (FPz_sph, id_rank, sph_file%comm_IO, sph_file%sph_IO,          &
     &    sph_file%sph_grp_IO, zbuf_s)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_file                                 &
     &         (file_name, id_rank, sph_file)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped grid file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_sph, file_name, zbuf_s)
      call write_geom_rtm_data_gz                                       &
     &   (FPz_sph, id_rank, sph_file%comm_IO, sph_file%sph_IO, zbuf_s)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_file                                &
     &         (file_name, id_rank, sph_file)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped spectr modes file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_sph, file_name, zbuf_s)
      call write_modes_rlm_data_gz                                      &
     &   (FPz_sph, id_rank, sph_file%comm_IO, sph_file%sph_IO, zbuf_s)
      call close_gzfile_a(FPz_sph, zbuf_s)
!
      end subroutine gz_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_file_IO
