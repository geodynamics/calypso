!>@file   gz_MPI_sph_modes_data_IO_b.f90
!!@brief  module gz_MPI_sph_modes_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_mpi_read_geom_rtp_data_b                          &
!!     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine gz_mpi_read_spectr_rj_data_b                         &
!!     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine gz_mpi_read_geom_rtm_data_b                          &
!!     &         (IO_param, comm_IO, sph_IO)
!!      subroutine gz_mpi_read_modes_rlm_data_b                         &
!!     &         (IO_param, comm_IO, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!
!!      subroutine gz_mpi_write_geom_rtp_data_b                         &
!!     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine gz_mpi_write_spectr_rj_data_b                        &
!!     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine gz_mpi_write_geom_rtm_data_b                         &
!!     &         (IO_param, comm_IO, sph_IO)
!!      subroutine gz_mpi_write_modes_rlm_data_b                        &
!!     &         (IO_param, comm_IO, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(sph_group_data), intent(in) :: sph_grps_IO
!!@endverbatim
!
      module gz_MPI_sph_modes_data_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_node_id_spherical_IO
      use t_spheric_group
      use t_calypso_mpi_IO_param
      use gz_MPI_domain_data_IO_b
      use gz_MPI_spherical_model_IO_b
      use gz_MPI_sph_gl_1d_idx_IO_b
      use gz_MPI_binary_datum_IO
      use gz_MPI_groups_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtp_data_b                            &
     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
!
      sph_IO%numdir_sph =  3
!
      call gz_mpi_read_domain_info_b(IO_param, comm_IO)
      call gz_mpi_read_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rank_4_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rtp_gl_1d_table_b(IO_param, sph_IO)
!
      call gz_mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
      call gz_mpi_read_import_data_b(IO_param, comm_IO)
!
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, sph_grps_IO%bc_rtp_grp)
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, sph_grps_IO%radial_rtp_grp)
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, sph_grps_IO%theta_rtp_grp)
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, sph_grps_IO%zonal_rtp_grp)
!
      end subroutine gz_mpi_read_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_spectr_rj_data_b                           &
     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
!
      sph_IO%numdir_sph =  2
!
      call gz_mpi_read_domain_info_b(IO_param, comm_IO)
      call gz_mpi_read_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rank_4_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rj_gl_1d_table_b(IO_param, sph_IO)
!
      call gz_mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
      call gz_mpi_read_import_data_b(IO_param, comm_IO)
!
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, sph_grps_IO%radial_rj_grp)
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, sph_grps_IO%sphere_rj_grp)
!
      end subroutine gz_mpi_read_spectr_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtm_data_b                            &
     &         (IO_param, comm_IO, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph =  3
!
      call gz_mpi_read_domain_info_b(IO_param, comm_IO)
      call gz_mpi_read_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rank_4_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rtp_gl_1d_table_b(IO_param, sph_IO)
!
      call gz_mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
      call gz_mpi_read_import_data_b(IO_param, comm_IO)
!
      end subroutine gz_mpi_read_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_modes_rlm_data_b                           &
     &         (IO_param, comm_IO, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph =  2
!
      call gz_mpi_read_domain_info_b(IO_param, comm_IO)
      call gz_mpi_read_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rank_4_sph_b(IO_param, sph_IO)
      call gz_mpi_read_rj_gl_1d_table_b(IO_param, sph_IO)
!
      call gz_mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
      call gz_mpi_read_import_data_b(IO_param, comm_IO)
!
      end subroutine gz_mpi_read_modes_rlm_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtp_data_b                           &
     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(sph_group_data), intent(in) :: sph_grps_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, comm_IO)
      call gz_mpi_write_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_write_rank_4_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_rtp_gl_1d_table_b(IO_param, sph_IO)
      call gz_mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_import_data_b(IO_param, comm_IO)
!
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, sph_grps_IO%bc_rtp_grp)
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, sph_grps_IO%radial_rtp_grp)
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, sph_grps_IO%theta_rtp_grp)
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, sph_grps_IO%zonal_rtp_grp)
!
      end subroutine gz_mpi_write_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_spectr_rj_data_b                          &
     &         (IO_param, comm_IO, sph_IO, sph_grps_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(sph_group_data), intent(in) :: sph_grps_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, comm_IO)
      call gz_mpi_write_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_write_rank_4_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_rj_gl_1d_table_b(IO_param, sph_IO)
      call gz_mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_import_data_b(IO_param, comm_IO)
!
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, sph_grps_IO%radial_rj_grp)
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, sph_grps_IO%sphere_rj_grp)
!
      end subroutine gz_mpi_write_spectr_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtm_data_b                           &
     &         (IO_param, comm_IO, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, comm_IO)
      call gz_mpi_write_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_write_rank_4_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_rtp_gl_1d_table_b(IO_param, sph_IO)
      call gz_mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_import_data_b(IO_param, comm_IO)
!
      end subroutine gz_mpi_write_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_modes_rlm_data_b                          &
     &         (IO_param, comm_IO, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, comm_IO)
      call gz_mpi_write_gl_reso_sph_b(IO_param, sph_IO)
      call gz_mpi_write_rank_4_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_rj_gl_1d_table_b(IO_param, sph_IO)
      call gz_mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!
      call gz_mpi_write_import_data_b(IO_param, comm_IO)
!
      end subroutine gz_mpi_write_modes_rlm_data_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_sph_modes_data_IO_b
