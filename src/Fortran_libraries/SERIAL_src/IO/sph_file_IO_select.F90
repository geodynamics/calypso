!>@file   sph_file_IO_select.f90
!!@brief  module sph_file_IO_select
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine set_sph_mesh_file_fmt_prefix(iflag_fmt, file_head)
!!
!!      subroutine sel_read_geom_rtp_file(id_rank, sph_file, ierr)
!!      subroutine sel_read_spectr_rj_file(id_rank, sph_file, ierr)
!!      subroutine sel_read_geom_rtm_file(id_rank, sph_file, ierr)
!!      subroutine sel_read_modes_rlm_file(id_rank, sph_file, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine sel_write_geom_rtp_file(id_rank, sph_file, ierr)
!!      subroutine sel_write_spectr_modes_rj_file                       &
!!     &         (id_rank, sph_file, ierr)
!!      subroutine sel_write_geom_rtm_file(id_rank, sph_file, ierr)
!!      subroutine sel_write_modes_rlm_file(id_rank, sph_file, ierr)
!!        type(sph_file_data_type), intent(in) :: sph_file
!!
!!      integer(kind = kint) function check_exsist_rtp_file(id_rank)
!!      integer(kind = kint) function check_exsist_rj_file(id_rank)
!!      integer(kind = kint) function check_exsist_rtm_file(id_rank)
!!      integer(kind = kint) function check_exsist_rlm_file(id_rank)
!!@endverbatim
!!
!!@param id_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_file_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_spheric_data_IO
      use set_parallel_file_name
      use set_mesh_file_names
!
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
!
#ifdef ZLIB_IO
      use gz_sph_modes_grids_file_IO
      use gz_sph_modes_file_IO_b
#endif
!
      implicit none
!
      character(len=kchara) :: sph_file_head =     "in_sph"
      integer(kind = kint) :: iflag_sph_file_fmt = 0
!
      integer(kind = kint), parameter, private :: mesh_file_id = 14
!
      character(len=kchara), private :: sph_file_name
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_sph_mesh_file_fmt_prefix(iflag_fmt, file_head)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
!
      iflag_sph_file_fmt = iflag_fmt
      write(sph_file_head,'(a)') trim(file_head)
!
      end subroutine set_sph_mesh_file_fmt_prefix
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine sel_read_geom_rtp_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rtp_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_geom_rtp_file_b                                       &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geom_rtp_file_b                                    &
     &    (sph_file_name, id_rank, sph_file, ierr)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_geom_rtp_file                                      &
     &     (sph_file_name, id_rank, sph_file, ierr)
#endif
!
      else
        call read_geom_rtp_file                                         &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file, ierr)
      end if
!
      end subroutine sel_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_spectr_rj_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rj_file_name                              &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_spectr_modes_rj_file_b                                &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_spectr_modes_rj_file_b                             &
     &     (sph_file_name, id_rank, sph_file, ierr)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_spectr_modes_rj_file                               &
     &     (sph_file_name, id_rank, sph_file, ierr)
#endif
!
      else
        call read_spectr_modes_rj_file                                  &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file, ierr)
      end if
!
      end subroutine sel_read_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtm_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rtm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_geom_rtm_file_b                                       &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geom_rtm_file_b                                    &
     &     (sph_file_name, id_rank, sph_file, ierr)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_geom_rtm_file                                      &
     &     (sph_file_name, id_rank, sph_file, ierr)
#endif
!
      else
        call read_geom_rtm_file                                         &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file, ierr)
      end if
!
      end subroutine sel_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_modes_rlm_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rlm_file_name                             &
     &             (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_modes_rlm_file_b                                      &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_modes_rlm_file_b                                   &
     &     (sph_file_name, id_rank, sph_file, ierr)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_modes_rlm_file                                     &
     &     (sph_file_name, id_rank, sph_file, ierr)
#endif
!
      else
        call read_modes_rlm_file                                        &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file, ierr)
      end if
!
      end subroutine sel_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtp_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rtp_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_geom_rtp_file_b                                      &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_geom_rtp_file_b(sph_file_name, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_geom_rtp_file(sph_file_name, id_rank, sph_file)
#endif
!
      else
        call write_geom_rtp_file                                        &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file)
      end if
!
      end subroutine sel_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_spectr_modes_rj_file                         &
     &         (id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rj_file_name                              &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_spectr_modes_rj_file_b                               &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_spectr_modes_rj_file_b                            &
     &     (sph_file_name, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_spectr_modes_rj_file                              &
     &     (sph_file_name, id_rank, sph_file)
#endif
!
      else
        call write_spectr_modes_rj_file                                 &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file)
      end if
!
      end subroutine sel_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtm_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rtm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_geom_rtm_file_b                                      &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_geom_rtm_file_b(sph_file_name, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_geom_rtm_file(sph_file_name, id_rank, sph_file)
#endif
!
      else
        call write_geom_rtm_file                                        &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file)
      end if
!
      end subroutine sel_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_modes_rlm_file(id_rank, sph_file, ierr)
!
      integer, intent(in) :: id_rank
      type(sph_file_data_type), intent(in) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_file_name = set_sph_rlm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt,  id_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_modes_rlm_file_b                                     &
     &     (sph_file_name, id_rank, sph_file, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_modes_rlm_file_b                                  &
     &     (sph_file_name, id_rank, sph_file)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_modes_rlm_file(sph_file_name, id_rank, sph_file)
#endif
!
      else
        call write_modes_rlm_file                                       &
     &     (sph_file_name, mesh_file_id, id_rank, sph_file)
      end if
!
      end subroutine sel_write_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rtp_file(id_rank)
!
      use delete_data_files
!
      integer, intent(in) :: id_rank
!
!
      sph_file_name = set_sph_rtp_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
      check_exsist_rtp_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rtp_file
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rj_file(id_rank)
!
      use delete_data_files
!
      integer, intent(in) :: id_rank
!
!
      sph_file_name = set_sph_rj_file_name                              &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
      check_exsist_rj_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rj_file
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rtm_file(id_rank)
!
      use delete_data_files
!
      integer, intent(in) :: id_rank
!
!
      sph_file_name = set_sph_rtm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt, id_rank)
      check_exsist_rtm_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rtm_file
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rlm_file(id_rank)
!
      use delete_data_files
!
      integer, intent(in) :: id_rank
!
!
      sph_file_name = set_sph_rlm_file_name                             &
     &            (sph_file_head, iflag_sph_file_fmt,  id_rank)
      check_exsist_rlm_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_file_IO_select
