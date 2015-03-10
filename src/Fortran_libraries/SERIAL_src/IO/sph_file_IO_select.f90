!>@file   sph_modes_grids_file_IO.f90
!!@brief  module sph_modes_grids_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine sel_read_geom_rtp_file(my_rank)
!!      subroutine sel_read_spectr_modes_rj_file(my_rank)
!!      subroutine sel_read_geom_rtm_file(my_rank)
!!      subroutine sel_read_modes_rlm_file(my_rank)
!!
!!      subroutine sel_write_geom_rtp_file(my_rank)
!!      subroutine sel_write_spectr_modes_rj_file(my_rank)
!!      subroutine sel_write_geom_rtm_file(my_rank)
!!      subroutine sel_write_modes_rlm_file(my_rank)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_file_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use set_parallel_file_name
!
      implicit none
!
      character(len=kchara), private :: sph_file_name
      character(len=kchara), private :: fname_tmp
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtp_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtp_extension(fname_tmp, sph_file_name)
!
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call read_geom_rtp_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call read_geom_rtp_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call read_geom_rtp_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_spectr_modes_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rj_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call read_spectr_modes_rj_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call read_spectr_modes_rj_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call read_spectr_modes_rj_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtm_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call read_geom_rtm_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call read_geom_rtm_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call read_geom_rtm_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_modes_rlm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rlm_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call read_modes_rlm_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call read_modes_rlm_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call read_modes_rlm_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtp_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtp_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call write_geom_rtp_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call write_geom_rtp_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call write_geom_rtp_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_spectr_modes_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rj_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call write_spectr_modes_rj_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call write_spectr_modes_rj_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call write_spectr_modes_rj_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtm_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call write_geom_rtm_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call write_geom_rtm_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call write_geom_rtm_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_modes_rlm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!      use sph_modes_grids_file_IO_b
!      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rlm_extension(fname_tmp, sph_file_name)
!
!      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
!        call write_modes_rlm_file_b(my_rank, sph_file_name)
!
!#ifdef ZLIB_IO
!      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
!        call write_modes_rlm_file_gz(my_rank, sph_file_name)
!#endif
!
!      else
        call write_modes_rlm_file(my_rank, sph_file_name)
!      end if
!
      end subroutine sel_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_file_IO_select
