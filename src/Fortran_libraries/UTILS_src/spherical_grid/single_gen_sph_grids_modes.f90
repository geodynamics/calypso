!>@file   single_gen_sph_grids_modes.f90
!!@brief  module single_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Serial version)
!!
!!@verbatim
!!      subroutine gen_sph_rlm_grids
!!      subroutine gen_sph_rtm_grids
!!
!!      subroutine gen_sph_rj_modes
!!      subroutine gen_sph_rtp_grids
!!
!!      subroutine gen_fem_mesh_for_sph
!!@endverbatim
!
      module single_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gen_sph_rlm_grids
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use copy_sph_comm_table_4_type
      use gen_sph_grids_modes
!
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        call const_sph_rlm_modes(ip_rank)
        call copy_comm_rlm_num_to_type(sph_para(ip)%sph_comms%comm_rlm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans(ip_rank)
!
        write(*,'(a,i6,a)') 'Legendre transform table rlm',             &
     &          ip_rank, ' is done.'
      end do
      write(*,*)
!
      end subroutine gen_sph_rlm_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rtm_grids
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use copy_sph_comm_table_4_type
      use gen_sph_grids_modes
!
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        call const_sph_rtm_grids(ip_rank)
        call copy_comm_rtm_num_to_type(sph_para(ip)%sph_comms%comm_rtm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans(ip_rank)
!
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &          ip_rank, ' is done.'
      end do
      write(*,*)
!
      end subroutine gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rj_modes
!
      use m_spheric_parameter
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rj_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        call const_sph_rj_modes(ip_rank)
      end do
      call deallocate_rj_1d_local_idx
!
      end subroutine gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rtp_grids
!
      use m_spheric_parameter
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        call const_sph_rtp_grids(ip_rank)
      end do
      call deallocate_rtp_1d_local_idx
!
      end subroutine gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_fem_mesh_for_sph
!
      use m_gauss_points
      use m_group_data_sph_specr
      use m_sph_mesh_1d_connect
      use gen_sph_grids_modes
      use set_local_index_table_sph
      use set_sph_groups
!
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_excluding_FEM_mesh .gt. 0) return
!
      n_point = nidx_global_rtp(2)
      call allocate_gauss_points
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call set_rj_radial_grp
!
      do ip_rank = 0, ndomain_sph-1
        call const_fem_mesh_for_sph(ip_rank)
      end do
!
      call deallocate_rj_r_grp_item
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module single_gen_sph_grids_modes
