!>@file   gz_sph_modes_grids_data_IO.f90
!!@brief  module gz_sph_modes_grids_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped spectr data IO routines
!!
!!@verbatim
!!      subroutine read_geom_rtp_data_gz                                &
!!     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, ierr)
!!      subroutine read_spectr_modes_rj_data_gz                         &
!!     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, ierr)
!!      subroutine read_geom_rtm_data_gz                                &
!!     &         (id_rank, comm_IO, sph_IO, ierr)
!!      subroutine read_spectr_modes_rlm_data_gz                        &
!!     &         (id_rank, comm_IO, sph_IO, ierr)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!
!!      subroutine write_geom_rtp_data_gz                               &
!!     &         (id_rank, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine write_spectr_modes_rj_data_gz                        &
!!     &         (id_rank, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine write_geom_rtm_data_gz(id_rank, comm_IO, sph_IO)
!!      subroutine write_modes_rlm_data_gz(id_rank, comm_IO, sph_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(sph_group_data), intent(in) :: sph_grps_IO
!!@endverbatim
!!
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_grids_data_IO
!
      use m_precision
      use m_machine_parameter
      use m_sph_modes_grid_labels
!
      use t_comm_table
      use t_node_id_spherical_IO
      use t_spheric_group
      use gz_domain_data_IO
      use gz_spherical_model_IO
      use gz_sph_global_1d_idx_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_data_gz                                  &
     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, ierr)
!
      use gz_groups_IO
!
      integer, intent(in) :: id_rank
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  3
!
!      write(*,*) '! domain and communication'
      call gz_read_domain_info(id_rank, comm_IO, ierr)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_gz(sph_IO)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_gz(sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table_gz(sph_IO)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph_gz(sph_IO)
!
!      write(*,*) '! communication table between spectr data'
      call gz_read_import_data(comm_IO)
!
!      write(*,*) 'read_rtp_node_grp_data_gz'
      call read_group_data_gz(sph_grps_IO%bc_rtp_grp)
!      write(*,*) 'read_rtp_radial_grp_data_gz'
      call read_group_data_gz(sph_grps_IO%radial_rtp_grp)
!      write(*,*) 'read_rtp_theta_grp_data_gz'
      call read_group_data_gz(sph_grps_IO%theta_rtp_grp)
!      write(*,*) 'read_rtp_zonal_grp_data_gz'
      call read_group_data_gz(sph_grps_IO%zonal_rtp_grp)
!
      end subroutine read_geom_rtp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data_gz                           &
     &         (id_rank, comm_IO, sph_IO, sph_grps_IO, ierr)
!
      use gz_groups_IO
!
      integer, intent(in) :: id_rank
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  2
!
!      write(*,*) '! domain and communication'
      call gz_read_domain_info(id_rank, comm_IO, ierr)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_gz(sph_IO)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_gz(sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table_gz(sph_IO)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph_gz(sph_IO)
!
!      write(*,*) '! communication table between spectr data'
      call gz_read_import_data(comm_IO)
!
!      write(*,*) '! Group data'
      call read_group_data_gz(sph_grps_IO%radial_rj_grp)
      call read_group_data_gz(sph_grps_IO%sphere_rj_grp)
!
      end subroutine read_spectr_modes_rj_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data_gz                                  &
     &         (id_rank, comm_IO, sph_IO, ierr)
!
      integer, intent(in) :: id_rank
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  3
!
      call gz_read_domain_info(id_rank, comm_IO, ierr)
      call read_gl_resolution_sph_gz(sph_IO)
      call read_rank_4_sph_gz(sph_IO)
      call read_rtp_gl_1d_table_gz(sph_IO)
      call read_gl_nodes_sph_gz(sph_IO)
!
      call gz_read_import_data(comm_IO)
!
      end subroutine read_geom_rtm_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data_gz                          &
     &         (id_rank, comm_IO, sph_IO, ierr)
!
      integer, intent(in) :: id_rank
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  2
!
      call gz_read_domain_info(id_rank, comm_IO, ierr)
      call read_gl_resolution_sph_gz(sph_IO)
      call read_rank_4_sph_gz(sph_IO)
      call read_rj_gl_1d_table_gz(sph_IO)
      call read_gl_nodes_sph_gz(sph_IO)
!
      call gz_read_import_data(comm_IO)
!
      end subroutine read_spectr_modes_rlm_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data_gz                                 &
     &         (id_rank, comm_IO, sph_IO, sph_grps_IO)
!
      use gz_groups_IO
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(sph_group_data), intent(in) :: sph_grps_IO
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_domain_info(id_rank, comm_IO)
!
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_gz(sph_IO)
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO)
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_import_data(comm_IO)
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      textbuf = hd_ngrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grps_IO%bc_rtp_grp)
!
      textbuf = hd_rgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grps_IO%radial_rtp_grp)
!
      textbuf = hd_tgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grps_IO%theta_rtp_grp)
!
      textbuf = hd_pgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grps_IO%zonal_rtp_grp)
!
      end subroutine write_geom_rtp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data_gz                          &
     &         (id_rank, comm_IO, sph_IO, sph_grps_IO)
!
      use gz_groups_IO
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
      type(sph_group_data), intent(in) :: sph_grps_IO
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_domain_info(id_rank, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_gz(sph_IO)
!
      textbuf = hd_rj_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO)
!
      textbuf = hd_rj_comm() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_import_data(comm_IO)
!
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      textbuf = hd_kgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grps_IO%radial_rj_grp)
!
      textbuf = hd_jgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grps_IO%sphere_rj_grp)
!
      end subroutine write_spectr_modes_rj_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data_gz(id_rank, comm_IO, sph_IO)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_domain_info(id_rank, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_gz(sph_IO)
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO)
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_import_data(comm_IO)
!
      end subroutine write_geom_rtm_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data_gz(id_rank, comm_IO, sph_IO)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_domain_info(id_rank, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_gz(sph_IO)
!
      textbuf = hd_rlm_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO)
!
      textbuf = hd_rj_comm() // char(0)
      call gz_write_textbuf_no_lf
      call gz_write_import_data(comm_IO)
!
      end subroutine write_modes_rlm_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_data_IO
