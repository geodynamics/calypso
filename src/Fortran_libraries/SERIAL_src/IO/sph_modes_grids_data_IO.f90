!>@file  sph_modes_grids_data_IO.f90
!!       module sph_modes_grids_data_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Routines for speherical grid data IO
!!
!!@verbatim
!!      subroutine read_geom_rtp_data(mesh_file_id)
!!      subroutine read_spectr_modes_rj_data(mesh_file_id)
!!      subroutine read_geom_rtm_data(mesh_file_id)
!!      subroutine read_spectr_modes_rlm_data(mesh_file_id)
!!
!!      subroutine write_geom_rtp_data(id_mesh)
!!      subroutine write_spectr_modes_rj_data(id_mesh)
!!      subroutine write_geom_rtm_data(id_mesh)
!!      subroutine write_modes_rlm_data(id_mesh)
!!@endverbatim
!
      module sph_modes_grids_data_IO
!
      use m_precision
!
      use domain_data_IO
      use spherical_model_IO
      use comm_stack_item_IO
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
      subroutine read_geom_rtp_data(mesh_file_id)
!
      use sph_rtp_groups_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!      write(*,*) '! domain and communication'
      call read_domain_info(mesh_file_id)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph(mesh_file_id)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph(mesh_file_id)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table(mesh_file_id)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph(mesh_file_id)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data(mesh_file_id)
!
!      write(*,*) '! Group data'
      call read_geom_rtp_groups(mesh_file_id)
!
      end subroutine read_geom_rtp_data
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data(mesh_file_id)
!
      use sph_rj_groups_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
!      write(*,*) '! domain and communication'
      call read_domain_info(mesh_file_id)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph(mesh_file_id)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph(mesh_file_id)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table(mesh_file_id)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph(mesh_file_id)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data(mesh_file_id)
!
!      write(*,*) '! Group data'
      call read_modes_rj_groups(mesh_file_id)
!
      end subroutine read_spectr_modes_rj_data
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_domain_info(mesh_file_id)
      call read_gl_resolution_sph(mesh_file_id)
      call read_rank_4_sph(mesh_file_id)
      call read_rtp_gl_1d_table(mesh_file_id)
      call read_gl_nodes_sph(mesh_file_id)
!
      call read_import_data(mesh_file_id)
!
      end subroutine read_geom_rtm_data
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_domain_info(mesh_file_id)
      call read_gl_resolution_sph(mesh_file_id)
      call read_rank_4_sph(mesh_file_id)
      call read_rj_gl_1d_table(mesh_file_id)
      call read_gl_nodes_sph(mesh_file_id)
!
      call read_import_data(mesh_file_id)
!
      end subroutine read_spectr_modes_rlm_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data(id_mesh)
!
      use sph_rtp_groups_IO
!
      integer(kind = kint), intent(in) :: id_mesh
!
      write(id_mesh,'(a)') '! '
      write(id_mesh,'(a)') '! 1.parallel information'
      write(id_mesh,'(a)') '!    domain ID'
      write(id_mesh,'(a)') '!    number of domain for transfer'
      write(id_mesh,'(a)') '!    domain ID for transfer'
      write(id_mesh,'(a)') '! '
!
      call write_domain_info(id_mesh)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! number of stack number for each domain'
      write(id_mesh,'(a)') '! local wavenumber ID'
      write(id_mesh,'(a)') '! global radial ID and grid ID'
      write(id_mesh,'(a)') '!'
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! communication table between spectr data'
      write(id_mesh,'(a)') '!'
      call write_import_data(id_mesh)
!
!      write(*,*) '! Group data'
      call write_geom_rtp_groups(id_mesh)
!
!      write(*,*) 'finish!!'
!
      end subroutine write_geom_rtp_data
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data(id_mesh)
!
      use sph_rj_groups_IO
!
      integer(kind = kint), intent(in) :: id_mesh
!
!
      write(id_mesh,'(a)') '! '
      write(id_mesh,'(a)') '! 1.parallel information'
      write(id_mesh,'(a)') '!    domain ID'
      write(id_mesh,'(a)') '!    number of domain for transfer'
      write(id_mesh,'(a)') '!    domain ID for transfer'
      write(id_mesh,'(a)') '! '
!
      call write_domain_info(id_mesh)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! number of stack number for each domain'
      write(id_mesh,'(a)') '! local wavenumber ID'
      write(id_mesh,'(a)') '! global radial ID and spectr ID'
      write(id_mesh,'(a)') '!'
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! communication table between spectr data'
      write(id_mesh,'(a)') '!'
      call write_import_data(id_mesh)
!
!      write(*,*) '! Group data'
      call write_modes_rj_groups(id_mesh)
!
      end subroutine write_spectr_modes_rj_data
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data(id_mesh)
!
      integer(kind = kint), intent(in) :: id_mesh
!
      write(id_mesh,'(a)') '! '
      write(id_mesh,'(a)') '! 1.parallel information'
      write(id_mesh,'(a)') '!    domain ID'
      write(id_mesh,'(a)') '!    number of domain for transfer'
      write(id_mesh,'(a)') '!    domain ID for transfer'
      write(id_mesh,'(a)') '! '
!
      call write_domain_info(id_mesh)
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! number of stack number for each domain'
      write(id_mesh,'(a)') '! local wavenumber ID'
      write(id_mesh,'(a)') '! global radial ID and grid ID'
      write(id_mesh,'(a)') '!'
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! communication table between grid data'
      write(id_mesh,'(a)') '!'
      call write_import_data(id_mesh)
!
      end subroutine write_geom_rtm_data
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data(id_mesh)
!
      integer(kind = kint), intent(in) :: id_mesh
!
      write(id_mesh,'(a)') '! '
      write(id_mesh,'(a)') '! 1.parallel information'
      write(id_mesh,'(a)') '!    domain ID'
      write(id_mesh,'(a)') '!    number of domain for transfer'
      write(id_mesh,'(a)') '!    domain ID for transfer'
      write(id_mesh,'(a)') '! '
!
      call write_domain_info(id_mesh)
!      write(id_mesh,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(id_mesh,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! number of stack number for each domain'
      write(id_mesh,'(a)') '! local wavenumber ID'
      write(id_mesh,'(a)') '! global radial ID and wavenumber ID'
      write(id_mesh,'(a)') '!'
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! communication table between spectr data'
      write(id_mesh,'(a)') '!'
      call write_import_data(id_mesh)
!
      end subroutine write_modes_rlm_data
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_data_IO
