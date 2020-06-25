!>@file   sph_modes_grids_file_IO_b.f90
!!@brief  module sph_modes_grids_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Binary spectr data IO routines
!!
!!@verbatim
!!      subroutine read_geom_rtp_file_b                                 &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine read_spectr_modes_rj_file_b                          &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine read_geom_rtm_file_b                                 &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine read_modes_rlm_file_b                                &
!!     &         (file_name, id_rank, sph_file, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine write_geom_rtp_file_b                                &
!!     &         (file_name, id_rank, sph_file), ierr
!!      subroutine write_spectr_modes_rj_file_b                         &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine write_geom_rtm_file_b                                &
!!     &         (file_name, id_rank, sph_file, ierr)
!!      subroutine write_modes_rlm_file_b                               &
!!     &         (file_name, id_rank, sph_file, ierr)
!!        type(sph_file_data_type), intent(in) :: sph_file
!!@endverbatim
!!
!!@param id_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_modes_grids_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_data_IO
      use t_binary_IO_buffer
      use sph_modes_grids_data_IO_b
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_sph =  21
      integer(kind = kint), parameter :: id_write_sph = 22
      type(binary_IO_buffer) :: bbuf_sph
      private :: id_read_sph, id_write_sph, bbuf_sph
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_file_b                                   &
     &         (file_name, id_rank, sph_file, ierr)
!
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary grid file: ', trim(file_name)
      bbuf_sph%id_binary = id_read_sph
      call open_read_binary_file(file_name, id_rank, bbuf_sph)
      if(bbuf_sph%ierr_bin .ne. 0) goto 99
      call read_geom_rtp_data_b(id_rank, bbuf_sph,                      &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file_b                            &
     &         (file_name, id_rank, sph_file, ierr)
!
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary spectr modes file: ', trim(file_name)
      bbuf_sph%id_binary = id_read_sph
      call open_read_binary_file(file_name, id_rank, bbuf_sph)
      if(bbuf_sph%ierr_bin .ne. 0) goto 99
      call read_spectr_modes_rj_data_b(id_rank, bbuf_sph,               &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine read_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file_b                                   &
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
     &      'Read binary grid file: ', trim(file_name)
      bbuf_sph%id_binary = id_read_sph
      call open_read_binary_file(file_name, id_rank, bbuf_sph)
      if(bbuf_sph%ierr_bin .ne. 0) goto 99
      call read_geom_rtm_data_b                                         &
     &   (id_rank, bbuf_sph, sph_file%comm_IO, sph_file%sph_IO)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file_b                                  &
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
     &      'Read binary spectr modes file: ', trim(file_name)
      bbuf_sph%id_binary = id_read_sph
      call open_read_binary_file(file_name, id_rank, bbuf_sph)
      if(bbuf_sph%ierr_bin .ne. 0) goto 99
      call read_modes_rlm_data_b                                        &
     &   (id_rank, bbuf_sph, sph_file%comm_IO, sph_file%sph_IO)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file_b                                  &
     &         (file_name, id_rank, sph_file, ierr)
!
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary grid file: ', trim(file_name)
      bbuf_sph%id_binary = id_write_sph
      call open_write_binary_file(file_name, bbuf_sph)
      if(bbuf_sph%ierr_bin .gt. 0) go to 99
      call write_geom_rtp_data_b(id_rank, sph_file%comm_IO,             &
     &    sph_file%sph_IO, sph_file%sph_grp_IO, bbuf_sph)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file_b                           &
     &         (file_name, id_rank, sph_file, ierr)
!
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'binary spectr modes file: ', trim(file_name)
      bbuf_sph%id_binary = id_write_sph
      call open_write_binary_file(file_name, bbuf_sph)
      if(bbuf_sph%ierr_bin .gt. 0) go to 99
      call write_spectr_modes_rj_data_b(id_rank, sph_file%comm_IO,      &
     &    sph_file%sph_IO, sph_file%sph_grp_IO, bbuf_sph)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine write_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file_b                                  &
     &         (file_name, id_rank, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary grid file: ', trim(file_name)
      bbuf_sph%id_binary = id_write_sph
      call open_write_binary_file(file_name, bbuf_sph)
      if(bbuf_sph%ierr_bin .gt. 0) go to 99
      call write_geom_rtm_data_b                                        &
     &   (id_rank, sph_file%comm_IO, sph_file%sph_IO, bbuf_sph)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file_b                                 &
     &         (file_name, id_rank, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary spectr modes file: ', trim(file_name)
      bbuf_sph%id_binary = id_write_sph
      call open_write_binary_file(file_name, bbuf_sph)
      if(bbuf_sph%ierr_bin .gt. 0) go to 99
      call write_modes_rlm_data_b                                       &
     &   (id_rank, sph_file%comm_IO, sph_file%sph_IO, bbuf_sph)
!
  99  continue
      call close_binary_file(bbuf_sph)
      ierr = bbuf_sph%ierr_bin
!
      end subroutine write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_file_IO_b
