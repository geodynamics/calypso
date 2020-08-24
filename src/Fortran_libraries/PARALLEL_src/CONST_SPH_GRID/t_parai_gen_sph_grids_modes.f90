!>@file   t_parai_gen_sph_grids_modes.f90
!!@brief  module t_parai_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_rj_modes(gen_sph, para_sph)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(parallel_sph_params), intent(inout) :: para_sph
!!      subroutine para_gen_sph_rtm_rtp_grids(gen_sph, para_sph)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(parallel_sph_params), intent(inout) :: para_sph
!!@endverbatim
!
      module t_parai_gen_sph_grids_modes
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_SPH_mesh_field_data
      use t_const_spherical_grid
      use t_sph_local_index
!
      implicit none
!
      type(sph_local_1d_index), save, private :: sph_lcx_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_rj_modes(num_pe, gen_sph, sph_mesh)
!
      use m_elapsed_labels_gen_SPH
      use set_comm_table_rtp_rj
      use gen_sph_grids_modes
      use bcast_comm_stacks_sph
!
      integer(kind = kint), intent(in) :: num_pe
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
      integer :: ip, id_rank
!
!
      allocate(comm_rlm_mul(num_pe))
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rlm table generation for', id_rank, ' on ', my_rank
        call const_sph_rlm_modes                                        &
     &   (id_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph_mesh(ip)%sph%sph_rlm, sph_mesh(ip)%sph_comms%comm_rlm)
        call copy_sph_comm_neib                                         &
     &     (sph_mesh(ip)%sph_comms%comm_rlm, comm_rlm_mul(ip))
      end do
!
      call s_bcast_comm_stacks_sph(num_pe, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      call alloc_rj_1d_local_idx(sph_mesh(1)%sph%sph_rj, sph_lcx_m)
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &     'Construct spherical modes for domain ', id_rank,            &
     &     ' on ', my_rank
        call const_sph_rj_modes(id_rank, num_pe, comm_rlm_mul,          &
     &      gen_sph%added_radial_grp, gen_sph%s3d_ranks,                &
     &      gen_sph%s3d_radius, gen_sph%sph_lcp,                        &
     &      gen_sph%stk_lc1d, gen_sph%sph_gl1d,                         &
     &      sph_mesh(ip)%sph%sph_params, sph_mesh(ip)%sph%sph_rtp,      &
     &      sph_mesh(ip)%sph%sph_rj, sph_mesh(ip)%sph_comms%comm_rj,    &
     &      sph_mesh(ip)%sph_grps, sph_lcx_m)
      end do
      call dealloc_rj_1d_local_idx(sph_lcx_m)
      call dealloc_comm_stacks_sph(num_pe, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
      end subroutine para_gen_sph_rlm_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_rtp_grids(num_pe, gen_sph, sph_mesh)
!
      use m_elapsed_labels_gen_SPH
      use gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use bcast_comm_stacks_sph
!
      integer(kind = kint), intent(in) :: num_pe
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable:: comm_rtm_mul(:)
      integer :: ip, id_rank
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      allocate(comm_rtm_mul(num_pe))
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rtm table generation for', id_rank, ' on ', my_rank
        call const_sph_rtm_grids                                        &
     &     (id_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,             &
     &      gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &      sph_mesh(ip)%sph%sph_rtm, sph_mesh(ip)%sph_comms%comm_rtm)
        call copy_sph_comm_neib                                         &
     &     (sph_mesh(ip)%sph_comms%comm_rtm, comm_rtm_mul(ip))
      end do
      call s_bcast_comm_stacks_sph(num_pe, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      call alloc_rtp_1d_local_idx(sph_mesh(1)%sph%sph_rtp, sph_lcx_m)
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',  id_rank,   &
     &              ' on ',  my_rank
        call const_sph_rtp_grids(id_rank, num_pe, comm_rtm_mul,         &
     &      gen_sph%added_radial_grp, gen_sph%r_layer_grp,              &
     &      gen_sph%med_layer_grp, gen_sph%s3d_ranks,                   &
     &      gen_sph%s3d_radius, gen_sph%sph_lcp,                        &
     &      gen_sph%stk_lc1d, gen_sph%sph_gl1d,                         &
     &      sph_mesh(ip)%sph%sph_params, sph_mesh(ip)%sph%sph_rtp,      &
     &      sph_mesh(ip)%sph_comms%comm_rtp, sph_mesh(ip)%sph_grps,     &
     &      sph_lcx_m)
      end do
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
      call dealloc_comm_stacks_sph(num_pe, comm_rtm_mul)
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
      end subroutine para_gen_sph_rtm_rtp_grids
!
! ----------------------------------------------------------------------
!
      end module t_parai_gen_sph_grids_modes
