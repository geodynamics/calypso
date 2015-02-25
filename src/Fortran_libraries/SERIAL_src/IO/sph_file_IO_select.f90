!sph_file_IO_select.F90
!      module sph_file_IO_select
!
!     Written by H. Matsui on July, 2007
!
!      subroutine sel_read_geom_rtp_file(my_rank)
!      subroutine sel_read_spectr_modes_rj_file(my_rank)
!      subroutine sel_read_geom_rtm_file(my_rank)
!      subroutine sel_read_modes_rlm_file(my_rank)
!
!      subroutine sel_write_geom_rtp_file(my_rank)
!      subroutine sel_write_spectr_modes_rj_file(my_rank)
!      subroutine sel_write_geom_rtm_file(my_rank)
!      subroutine sel_write_modes_rlm_file(my_rank)
!
!      subroutine sel_read_int_4_sph_coriolis
!      subroutine sel_write_int_4_sph_coriolis
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
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtp_extension(fname_tmp, sph_file_name)
!
      call read_geom_rtp_file(my_rank, sph_file_name)
!
      end subroutine sel_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_spectr_modes_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rj_extension(fname_tmp, sph_file_name)
!
      call read_spectr_modes_rj_file(my_rank, sph_file_name)
!
      end subroutine sel_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtm_extension(fname_tmp, sph_file_name)
!
      call read_geom_rtm_file(my_rank, sph_file_name)
!
      end subroutine sel_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_modes_rlm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rlm_extension(fname_tmp, sph_file_name)
!
      call read_modes_rlm_file(my_rank, sph_file_name)
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
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtp_extension(fname_tmp, sph_file_name)
!
      call write_geom_rtp_file(my_rank, sph_file_name)
!
      end subroutine sel_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_spectr_modes_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rj_extension(fname_tmp, sph_file_name)
!
      call write_spectr_modes_rj_file(my_rank, sph_file_name)
!
      end subroutine sel_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rtm_extension(fname_tmp, sph_file_name)
!
      call write_geom_rtm_file(my_rank, sph_file_name)
!
      end subroutine sel_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_modes_rlm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call add_int_suffix(my_rank, sph_file_head, fname_tmp)
      call add_rlm_extension(fname_tmp, sph_file_name)
!
      call write_modes_rlm_file(my_rank, sph_file_name)
!
      end subroutine sel_write_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO
!
!
      call read_int_4_sph_coriolis
!
      end subroutine sel_read_int_4_sph_coriolis
!
!------------------------------------------------------------------
!
      subroutine sel_write_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO
!
!
      call write_int_4_sph_coriolis
!
      end subroutine sel_write_int_4_sph_coriolis
!
!------------------------------------------------------------------
!
      end module sph_file_IO_select
