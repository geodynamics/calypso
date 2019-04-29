!>@file   gz_sph_modes_file_IO_b.f90
!!@brief  module gz_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped binary spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_file_b                              &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine gz_read_spectr_modes_rj_file_b                       &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine gz_read_geom_rtm_file_b                              &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine gz_read_modes_rlm_file_b                             &
!!     &         (file_name, id_rank, sph_file, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine gz_write_geom_rtp_file_b                             &
!!     &         (file_name, id_rank, sph_file)
!!      subroutine gz_write_spectr_modes_rj_file_b                      &
!!     &         (file_name, id_rank, sph_file)
!!      subroutine gz_write_geom_rtm_file_b                             &
!!     &         (file_name, id_rank, sph_file)
!!      subroutine gz_write_modes_rlm_file_b                            &
!!     &         (file_name, id_rank, sph_file)
!!        type(sph_file_data_type), intent(in) :: sph_file
!!@endverbatim
!!
!!@param id_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_data_IO
      use gz_sph_modes_data_IO_b
      use binary_IO
      use skip_gz_comment
!
      implicit none
!
      type(binary_IO_flags), private :: gz_sphflags
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtp_file_b                                &
     &         (file_name, id_rank, sph_file, ierr)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary grid file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, id_rank, gz_sphflags)
      if(gz_sphflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_geom_rtp_data_b(id_rank, gz_sphflags,                &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_sphflags%ierr_IO
!
      end subroutine gz_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_file_b                         &
     &         (file_name, id_rank, sph_file, ierr)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary spectr modes file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, id_rank, gz_sphflags)
      if(gz_sphflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_spectr_modes_rj_data_b(id_rank, gz_sphflags,         &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_sphflags%ierr_IO
!
      end subroutine gz_read_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_file_b                                &
     &         (file_name, id_rank, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary grid file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, id_rank, gz_sphflags)
      if(gz_sphflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_geom_rtm_data_b                                      &
     &   (id_rank, gz_sphflags, sph_file%comm_IO, sph_file%sph_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_sphflags%ierr_IO
!
      end subroutine gz_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_file_b                               &
     &         (file_name, id_rank, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary spectr modes file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, id_rank, gz_sphflags)
      if(gz_sphflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_modes_rlm_data_b                                     &
     &   (id_rank, gz_sphflags, sph_file%comm_IO, sph_file%sph_IO)
!
  99  continue
      call close_gzfile_f
      ierr = gz_sphflags%ierr_IO
!
      end subroutine gz_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_file_b                               &
     &         (file_name, id_rank, sph_file)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary grid file: ', trim(file_name)
      call open_wt_gzfile_b(file_name, gz_sphflags)
      call gz_write_geom_rtp_data_b(id_rank, sph_file%comm_IO,          &
     &    sph_file%sph_IO, sph_file%sph_grp_IO, gz_sphflags)
      call close_gzfile_f
!
      end subroutine gz_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_file_b                        &
     &         (file_name, id_rank, sph_file)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'gzipped binary spectr modes file: ', trim(file_name)
      call open_wt_gzfile_b(file_name, gz_sphflags)
      call gz_write_spectr_modes_rj_data_b(id_rank, sph_file%comm_IO,   &
     &    sph_file%sph_IO, sph_file%sph_grp_IO, gz_sphflags)
      call close_gzfile_f
!
      end subroutine gz_write_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_file_b                               &
     &         (file_name, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary grid file: ', trim(file_name)
      call open_wt_gzfile_b(file_name, gz_sphflags)
      call gz_write_geom_rtm_data_b                                     &
     &   (id_rank, sph_file%comm_IO, sph_file%sph_IO, gz_sphflags)
      call close_gzfile_f
!
      end subroutine gz_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_file_b                              &
     &         (file_name, id_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary spectr modes file: ', trim(file_name)
      call open_wt_gzfile_b(file_name, gz_sphflags)
      call gz_write_modes_rlm_data_b                                    &
     &   (id_rank, sph_file%comm_IO, sph_file%sph_IO, gz_sphflags)
      call close_gzfile_f
!
      end subroutine gz_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_file_IO_b
