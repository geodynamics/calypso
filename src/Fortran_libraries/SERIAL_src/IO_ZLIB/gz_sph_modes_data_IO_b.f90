!>@file   gz_sph_modes_data_IO_b.f90
!!@brief  module gz_sph_modes_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped binary spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_data_b                              &
!!     &         (id_rank, bflag, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine gz_read_spectr_modes_rj_data_b                       &
!!     &         (id_rank, bflag, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine gz_read_geom_rtm_data_b                              &
!!     &         (id_rank, bflag, comm_IO, sph_IO)
!!      subroutine gz_read_modes_rlm_data_b                             &
!!     &         (id_rank, bflag, comm_IO, sph_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!
!!      subroutine gz_write_geom_rtp_data_b                             &
!!     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, bflag)
!!      subroutine gz_write_spectr_modes_rj_data_b                      &
!!     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, bflag)
!!      subroutine gz_write_geom_rtm_data_b                             &
!!     &         (id_rank, comm_IO, sph_IO, bflag)
!!      subroutine gz_write_modes_rlm_data_b                            &
!!     &         (id_rank, comm_IO, sph_IO, bflag)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(sph_group_data), intent(in) :: sph_grps_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!!
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_data_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_node_id_spherical_IO
      use t_spheric_group
      use gz_domain_data_IO_b
      use gz_spherical_model_IO_b
      use gz_sph_global_1d_idx_IO_b
      use gz_binary_IO
      use binary_IO
      use skip_gz_comment
!
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtp_data_b                                &
     &         (id_rank, bflag, comm_IO, sph_IO, sph_grps_IO)
!
      use gz_groups_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
!
      sph_IO%numdir_sph =  3
!
!      write(*,*) '! domain and communication'
      call gz_read_domain_info_b(id_rank, bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_read_gl_resolution_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! segment ID for each direction'
      call gz_read_rank_4_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global ID for each direction'
      call gz_read_rtp_gl_1d_table_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global radial ID and grid ID'
      call gz_read_gl_nodes_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! communication table for rtp'
      call gz_read_import_data_b(bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! Group data bc_rtp_grp_IO'
      call gz_read_group_data_b(bflag, sph_grps_IO%bc_rtp_grp)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! Group data radial_rtp_grp_IO'
      call gz_read_group_data_b(bflag, sph_grps_IO%radial_rtp_grp)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! Group data theta_rtp_grp_IO'
      call gz_read_group_data_b(bflag, sph_grps_IO%theta_rtp_grp)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! Group data zonal_rtp_grp_IO'
      call gz_read_group_data_b(bflag, sph_grps_IO%zonal_rtp_grp)
!
      end subroutine gz_read_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_data_b                         &
     &         (id_rank, bflag, comm_IO, sph_IO, sph_grps_IO)
!
      use gz_groups_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
!
      sph_IO%numdir_sph =  2
!
!      write(*,*) '! domain and communication'
      call gz_read_domain_info_b(id_rank, bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_read_gl_resolution_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! segment ID for each direction'
      call gz_read_rank_4_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global ID for each direction'
      call gz_read_rj_gl_1d_table_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global radial ID and spectr ID'
      call gz_read_gl_nodes_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_import_data_b(bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! Group data'
      call gz_read_group_data_b(bflag, sph_grps_IO%radial_rj_grp)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_group_data_b(bflag, sph_grps_IO%sphere_rj_grp)
!
      end subroutine gz_read_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_data_b                                &
     &         (id_rank, bflag, comm_IO, sph_IO)
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph =  3
!
      call gz_read_domain_info_b(id_rank, bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_gl_resolution_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_rank_4_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_rtp_gl_1d_table_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_gl_nodes_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_import_data_b(bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_read_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_data_b                               &
     &         (id_rank, bflag, comm_IO, sph_IO)
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph =  2
!
      call gz_read_domain_info_b(id_rank, bflag, comm_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_gl_resolution_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_rank_4_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_rj_gl_1d_table_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
      call gz_read_gl_nodes_sph_b(bflag, sph_IO)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_read_import_data_b(bflag, comm_IO)
!
      end subroutine gz_read_modes_rlm_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_data_b                               &
     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, bflag)
!
      use gz_groups_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(sph_group_data), intent(in) :: sph_grps_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
!      write(*,*) '! domain and communication'
      call gz_write_domain_info_b(id_rank, comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_write_gl_resolution_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! segment ID for each direction'
      call gz_write_rank_4_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global ID for each direction'
      call gz_write_rtp_gl_1d_table_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global radial ID and grid ID'
      call gz_write_gl_nodes_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! communication table between spectr data'
      call gz_write_import_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! Group data'
      call gz_write_grp_data_b(sph_grps_IO%bc_rtp_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_grp_data_b(sph_grps_IO%radial_rtp_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_grp_data_b(sph_grps_IO%theta_rtp_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_grp_data_b(sph_grps_IO%zonal_rtp_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_data_b                        &
     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, bflag)
!
      use gz_groups_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(sph_group_data), intent(in) :: sph_grps_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
!      write(*,*) '! domain and communication'
      call gz_write_domain_info_b(id_rank, comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_write_gl_resolution_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!      write(*,*) '! segment ID for each direction'
      call gz_write_rank_4_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global ID for each direction'
      call gz_write_rj_gl_1d_table_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! global radial ID and spectr ID'
      call gz_write_gl_nodes_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! communication table between spectr data'
      call gz_write_import_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(*,*) '! Group data'
      call gz_write_grp_data_b(sph_grps_IO%radial_rj_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_grp_data_b(sph_grps_IO%sphere_rj_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_data_b                               &
     &         (id_rank, comm_IO, sph_IO, bflag)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_domain_info_b(id_rank, comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_gl_resolution_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_rank_4_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_rtp_gl_1d_table_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_gl_nodes_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_import_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_data_b                              &
     &         (id_rank, comm_IO, sph_IO, bflag)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_domain_info_b(id_rank, comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_gl_resolution_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_rank_4_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_rj_gl_1d_table_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_gl_nodes_sph_b(sph_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_import_data_b(comm_IO, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_modes_rlm_data_b
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_data_IO_b
