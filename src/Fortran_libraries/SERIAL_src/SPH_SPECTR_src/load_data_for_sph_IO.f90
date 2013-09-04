!
!      module load_data_for_sph_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine input_sph_trans_grids(my_rank)
!
!      subroutine input_geom_rtp_sph_trans(my_rank)
!      subroutine input_modes_rj_sph_trans(my_rank)
!      subroutine input_geom_rtm_sph_trans(my_rank)
!      subroutine input_modes_rlm_sph_trans(my_rank)
!
!      subroutine output_geom_rtp_sph_trans(my_rank)
!      subroutine output_modes_rj_sph_trans(my_rank)
!      subroutine output_geom_rtm_sph_trans(my_rank)
!      subroutine output_modes_rlm_sph_trans(my_rank)
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
      subroutine input_sph_trans_grids(my_rank)
!
      use m_machine_parameter
      use m_spheric_parameter
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtp_sph_trans'
      call input_geom_rtp_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtm_sph_trans'
      call input_geom_rtm_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rlm_sph_trans'
      call input_modes_rlm_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp
!
!
      call set_special_degree_order_flags(nidx_rj(2), nidx_rlm(2),      &
     &    idx_gl_1d_rj_j, idx_gl_1d_rlm_j, idx_rj_degree_zero,          &
     &    idx_rj_degree_one,  ist_rtm_order_zero,                       &
     &    ist_rtm_order_1s, ist_rtm_order_1c)
!
!
      end subroutine input_sph_trans_grids
!
! -----------------------------------------------------------------------
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
