!>@file  sph_modes_grids_data_IO.f90
!!       module sph_modes_grids_data_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Routines for speherical grid data IO
!!
!!@verbatim
!!      subroutine read_geom_rtp_data(id_file, my_rank_IO,              &
!!     &          comm_IO, sph_IO, sph_grps_IO, ierr)
!!      subroutine read_spectr_modes_rj_data(id_file, my_rank_IO,       &
!!     &          comm_IO, sph_IO, sph_grps_IO, ierr)
!!      subroutine read_geom_rtm_data                                   &
!!     &         (id_file, my_rank_IO, comm_IO, sph_IO, ierr)
!!      subroutine read_spectr_modes_rlm_data                           &
!!     &         (id_file, my_rank_IO, comm_IO, sph_IO, ierr)
!!
!!      subroutine write_geom_rtp_data                                  &
!!     &         (id_file, my_rank_IO, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine write_spectr_modes_rj_data                           &
!!     &         (id_file, my_rank_IO, comm_IO, sph_IO, sph_grps_IO)
!!      subroutine write_geom_rtm_data                                  &
!!     &         (id_file, my_rank_IO, comm_IO, sph_IO)
!!      subroutine write_modes_rlm_data                                 &
!!     &         (id_file, my_rank_IO, comm_IO, sph_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!@endverbatim
!
      module sph_modes_grids_data_IO
!
      use m_precision
      use m_machine_parameter
      use m_sph_modes_grid_labels
!
      use t_comm_table
      use t_node_id_spherical_IO
      use t_spheric_group
      use domain_data_IO
      use spherical_model_IO
      use sph_global_1d_idx_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_data(id_file, my_rank_IO,                &
     &          comm_IO, sph_IO, sph_grps_IO, ierr)
!
      use groups_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  3
!
!      write(*,*) '! domain and communication'
      call read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
      if(ierr .ne. 0) return
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph(id_file, sph_IO)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph(id_file, sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table(id_file, sph_IO)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph(id_file, sph_IO)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data(id_file, comm_IO)
!
!      write(*,*) '! Group data'
      call read_group_data(id_file, sph_grps_IO%bc_rtp_grp)
      call read_group_data(id_file, sph_grps_IO%radial_rtp_grp)
      call read_group_data(id_file, sph_grps_IO%theta_rtp_grp)
      call read_group_data(id_file, sph_grps_IO%zonal_rtp_grp)
!
      end subroutine read_geom_rtp_data
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data(id_file, my_rank_IO,         &
     &          comm_IO, sph_IO, sph_grps_IO, ierr)
!
      use groups_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  2
!
!      write(*,*) '! domain and communication'
      call read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
      if(ierr .ne. 0) return
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph(id_file, sph_IO)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph(id_file, sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table(id_file, sph_IO)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph(id_file, sph_IO)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data(id_file, comm_IO)
!
!      write(*,*) '! Group data'
      call read_group_data(id_file, sph_grps_IO%radial_rj_grp)
      call read_group_data(id_file, sph_grps_IO%sphere_rj_grp)
!
      end subroutine read_spectr_modes_rj_data
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data                                     &
     &         (id_file, my_rank_IO, comm_IO, sph_IO, ierr)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  3
!
      call read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
      if(ierr .ne. 0) return
!
      call read_gl_resolution_sph(id_file, sph_IO)
      call read_rank_4_sph(id_file, sph_IO)
      call read_rtp_gl_1d_table(id_file, sph_IO)
      call read_gl_nodes_sph(id_file, sph_IO)
!
      call read_import_data(id_file, comm_IO)
!
      end subroutine read_geom_rtm_data
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data                             &
     &         (id_file, my_rank_IO, comm_IO, sph_IO, ierr)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      sph_IO%numdir_sph =  2
!
      call read_domain_info(id_file, my_rank_IO, comm_IO, ierr)
      if(ierr .ne. 0) return
!
      call read_gl_resolution_sph(id_file, sph_IO)
      call read_rank_4_sph(id_file, sph_IO)
      call read_rj_gl_1d_table(id_file, sph_IO)
      call read_gl_nodes_sph(id_file, sph_IO)
!
      call read_import_data(id_file, comm_IO)
!
      end subroutine read_spectr_modes_rlm_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data                                    &
     &         (id_file, my_rank_IO, comm_IO, sph_IO, sph_grps_IO)
!
      use groups_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank_IO
!
!
      write(id_file,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_file, my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_file, sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_file, sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rtp_glbl()
      call write_gl_nodes_sph(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rtp_comm()
      call write_import_data(id_file, comm_IO)
!
!      write(*,*) '! Group data'
!
      write(id_file,'(a)', advance='NO') hd_grphd()
!
      write(id_file,'(a)', advance='NO') hd_ngrphd()
      call write_grp_data(id_file, sph_grps_IO%bc_rtp_grp)
!
      write(id_file,'(a)', advance='NO') hd_rgrphd()
      call write_grp_data(id_file, sph_grps_IO%radial_rtp_grp)
!
      write(id_file,'(a)', advance='NO') hd_tgrphd()
      call write_grp_data(id_file, sph_grps_IO%theta_rtp_grp)
!
      write(id_file,'(a)', advance='NO') hd_pgrphd()
      call write_grp_data(id_file, sph_grps_IO%zonal_rtp_grp)
!
!      write(*,*) 'finish!!'
!
      end subroutine write_geom_rtp_data
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data                             &
     &         (id_file, my_rank_IO, comm_IO, sph_IO, sph_grps_IO)
!
      use groups_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank_IO
!
!
      write(id_file,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_file, my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_file, sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_file, sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rj_glbl()
      call write_gl_nodes_sph(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rj_comm()
      call write_import_data(id_file, comm_IO)
!
!      write(*,*) '! Group data'
      write(id_file,'(a)', advance='NO') hd_grphd()
      write(id_file,'(a)', advance='NO') hd_kgrphd()
      call write_grp_data(id_file, sph_grps_IO%radial_rj_grp)
      write(id_file,'(a)', advance='NO') hd_jgrphd()
      call write_grp_data(id_file, sph_grps_IO%sphere_rj_grp)
!
      end subroutine write_spectr_modes_rj_data
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data                                    &
     &         (id_file, my_rank_IO, comm_IO, sph_IO)
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank_IO
!
!
      write(id_file,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_file, my_rank_IO, comm_IO)
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_file, sph_IO)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_file, sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rtp_glbl()
      call write_gl_nodes_sph(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rtp_comm()
      call write_import_data(id_file, comm_IO)
!
      end subroutine write_geom_rtm_data
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data                                   &
     &         (id_file, my_rank_IO, comm_IO, sph_IO)
!
      integer(kind = kint), intent(in) :: id_file, my_rank_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      write(id_file,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_file, my_rank_IO, comm_IO)
!      write(id_file,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_file, sph_IO)
!      write(id_file,*) '! segment ID for each direction'
      call write_rank_4_sph(id_file, sph_IO)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rlm_glbl()
      call write_gl_nodes_sph(id_file, sph_IO)
!
      write(id_file,'(a)', advance='NO') hd_rj_comm()
      call write_import_data(id_file, comm_IO)
!
      end subroutine write_modes_rlm_data
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_data_IO
