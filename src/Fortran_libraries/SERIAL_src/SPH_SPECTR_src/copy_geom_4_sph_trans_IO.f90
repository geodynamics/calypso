!>@file   copy_geom_4_sph_trans_IO.f90
!!@brief  module copy_geom_4_sph_trans_IO
!!
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Construct matrices for 4th order FDM
!!
!!@verbatim
!!      subroutine copy_geom_rtp_sph_trans
!!      subroutine copy_spectr_modes_rj_sph_trans
!!      subroutine copy_geom_rtm_sph_trans
!!      subroutine copy_modes_rlm_sph_trans
!!
!!      subroutine copy_geom_rtp_IO(my_rank)
!!      subroutine copy_spectr_modes_rj_IO(my_rank)
!!      subroutine copy_geom_rtm_IO(my_rank)
!!      subroutine copy_modes_rlm_IO(my_rank)
!!@endverbatim
!
      module copy_geom_4_sph_trans_IO
!
      use m_precision
!
      use copy_sph_node_4_IO
      use copy_sph_comm_table_4_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_geom_rtp_sph_trans
!
      use m_spheric_parameter
      use copy_sph_groups_from_IO
!
!
      call copy_sph_node_rtp_from_IO
      call copy_comm_rtp_from_IO(nnod_rtp)
!
      call copy_rtp_nod_grp_from_IO
      call copy_rtp_radial_grp_from_IO
      call copy_rtp_theta_grp_from_IO
      call copy_rtp_zonal_grp_from_IO
!
      end subroutine copy_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_spectr_modes_rj_sph_trans
!
      use m_spheric_parameter
      use copy_sph_groups_from_IO
!
!
      call copy_sph_node_rj_from_IO
      call copy_comm_rj_from_IO(nnod_rj)
!
      call copy_rj_radial_grp_from_IO
      call copy_rj_sphere_grp_from_IO
!
      end subroutine copy_spectr_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_geom_rtm_sph_trans
!
      use m_spheric_parameter
!
!
      call copy_sph_node_rtm_from_IO
      call copy_comm_rtm_from_IO(nnod_rtm)
!
      end subroutine copy_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_modes_rlm_sph_trans
!
      use m_spheric_parameter
!
!
      call copy_sph_node_rlm_from_IO
      call copy_comm_rlm_from_IO(nnod_rlm)
!
      end subroutine copy_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_geom_rtp_IO(my_rank)
!
      use copy_sph_groups_to_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_sph_node_rtp_to_IO
      call copy_comm_rtp_to_IO(my_rank)
!
      call copy_rtp_nod_grp_to_IO
      call copy_rtp_radial_grp_to_IO
      call copy_rtp_theta_grp_to_IO
      call copy_rtp_zonal_grp_to_IO
!
      end subroutine copy_geom_rtp_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_spectr_modes_rj_IO(my_rank)
!
      use copy_sph_groups_to_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_sph_node_rj_to_IO
      call copy_comm_rj_to_IO(my_rank)
!
      call copy_rj_radial_grp_to_IO
      call copy_rj_sphere_grp_to_IO
!
      end subroutine copy_spectr_modes_rj_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_geom_rtm_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_sph_node_rtm_to_IO
      call copy_comm_rtm_to_IO(my_rank)
!
      end subroutine copy_geom_rtm_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_modes_rlm_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_sph_node_rlm_to_IO
      call copy_comm_rlm_to_IO(my_rank)
!
      end subroutine copy_modes_rlm_IO
!
! -----------------------------------------------------------------------
!
      end module copy_geom_4_sph_trans_IO
