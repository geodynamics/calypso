!>@file   load_data_for_sph_IO.f90
!!@brief  module load_data_for_sph_IO
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief load spherical harmonics indexing data
!!
!!@verbatim
!!      subroutine input_geom_rtp_sph_trans(my_rank)
!!      subroutine input_modes_rj_sph_trans(my_rank)
!!      subroutine input_geom_rtm_sph_trans(my_rank)
!!      subroutine input_modes_rlm_sph_trans(my_rank)
!!
!!      subroutine output_geom_rtp_sph_trans(my_rank)
!!      subroutine output_modes_rj_sph_trans(my_rank)
!!      subroutine output_geom_rtm_sph_trans(my_rank)
!!      subroutine output_modes_rlm_sph_trans(my_rank)
!!@endverbatim
!
      module load_data_for_sph_IO
!
      use m_precision
!
      use m_node_id_spherical_IO
      use copy_geom_4_sph_trans_IO
      use sph_file_IO_select
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtp_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call sel_read_geom_rtp_file(my_rank)
      call copy_geom_rtp_sph_trans(my_rank)
!
      end subroutine input_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rj_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call sel_read_spectr_modes_rj_file(my_rank)
      call copy_spectr_modes_rj_sph_trans(my_rank)
!
      end subroutine input_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtm_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call sel_read_geom_rtm_file(my_rank)
      call copy_geom_rtm_sph_trans(my_rank)
!
      end subroutine input_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rlm_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call sel_read_modes_rlm_file(my_rank)
      call copy_modes_rlm_sph_trans(my_rank)
!
      end subroutine input_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtp_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_geom_rtp_IO(my_rank)
      call sel_write_geom_rtp_file(my_rank)
!
      end subroutine output_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rj_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_spectr_modes_rj_IO(my_rank)
      call sel_write_spectr_modes_rj_file(my_rank)
!
      end subroutine output_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtm_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_geom_rtm_IO(my_rank)
      call sel_write_geom_rtm_file(my_rank)
!
      end subroutine output_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rlm_sph_trans(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      call copy_modes_rlm_IO(my_rank)
      call sel_write_modes_rlm_file(my_rank)
!
      end subroutine output_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
!
      end module load_data_for_sph_IO
