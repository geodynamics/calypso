!sph_modes_grids_file_IO.f90
!      module sph_modes_grids_file_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_fname_sph_rtp(my_rank)
!      subroutine set_fname_sph_rj(my_rank)
!      subroutine set_fname_sph_rtm(my_rank)
!      subroutine set_fname_sph_rlm(my_rank)
!
!      subroutine set_fname_org_sph_rj(my_rank)
!
!      subroutine read_geom_rtp_file(my_rank)
!      subroutine read_spectr_modes_rj_file(my_rank)
!      subroutine read_geom_rtm_file(my_rank)
!      subroutine read_modes_rlm_file(my_rank)
!
!      subroutine write_geom_rtp_file(my_rank)
!      subroutine write_spectr_modes_rj_file(my_rank)
!      subroutine write_geom_rtm_file(my_rank)
!      subroutine write_modes_rlm_file(my_rank)
!
      module sph_modes_grids_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
      use set_parallel_file_name
      use sph_modes_grids_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_fname_sph_rtp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(my_rank, sph_head, fname_tmp)
      call add_rtp_extension(fname_tmp, sph_file_name)
!
      end subroutine set_fname_sph_rtp
!
!------------------------------------------------------------------
!
      subroutine set_fname_sph_rj(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(my_rank, sph_head, fname_tmp)
      call add_rj_extension(fname_tmp, sph_file_name)
!
      end subroutine set_fname_sph_rj
!
!------------------------------------------------------------------
!
      subroutine set_fname_sph_rtm(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(my_rank, sph_head, fname_tmp)
      call add_rtm_extension(fname_tmp, sph_file_name)
!
      end subroutine set_fname_sph_rtm
!
!------------------------------------------------------------------
!
      subroutine set_fname_sph_rlm(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(my_rank, sph_head, fname_tmp)
      call add_rlm_extension(fname_tmp, sph_file_name)
!
      end subroutine set_fname_sph_rlm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_fname_org_sph_rj(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(my_rank, org_sph_rj_head, fname_tmp)
      call add_rj_extension(fname_tmp, sph_file_name)
!
      end subroutine set_fname_org_sph_rj
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii grid file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call read_geom_rtp_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii spectr modes file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call read_spectr_modes_rj_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii grid file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call read_geom_rtm_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii spectr modes file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call read_spectr_modes_rlm_data(mesh_file_id)
!
      close(mesh_file_id)
!
      end subroutine read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii grid file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call write_geom_rtp_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii spectr modes file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call write_spectr_modes_rj_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii grid file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call write_geom_rtm_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii spectr modes file: ', trim(sph_file_name)
      open (mesh_file_id,file = sph_file_name, form = 'formatted')
      call write_modes_rlm_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_file_IO
