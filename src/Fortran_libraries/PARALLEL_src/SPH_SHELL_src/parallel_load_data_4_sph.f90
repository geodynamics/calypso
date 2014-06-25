!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_para_sph_mesh
!!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_para_sph_mesh
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
!
      use load_data_for_sph_IO
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
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
      call set_sph_rj_center_flag(nnod_rj, nidx_rj, inod_rj_center)
!
      iflag_rj_center = 0
      call MPI_allREDUCE(inod_rj_center, iflag_rj_center, ione,         &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(iflag_rj_center .gt. 0) iflag_rj_center = 1
!
      end subroutine load_para_sph_mesh
!
! -----------------------------------------------------------------------
!
      end module parallel_load_data_4_sph
