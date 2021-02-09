!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed on Sep., 2017
!!
!!@brief  Construct spherical harmonics index and communications
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_rj_modes(gen_sph, sph_array)
!!      subroutine para_gen_sph_rtm_rtp_grids(gen_sph, sph_array)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_mesh_array), intent(inout) :: sph_array
!!@endverbatim
!
!
      module para_gen_sph_grids_modes
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      use calypso_mpi
      use m_work_time
!
      use t_SPH_mesh_field_array
      use t_const_spherical_grid
      use t_sph_local_index
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_rj_modes(gen_sph, sph_array)
!
      use m_elapsed_labels_gen_SPH
      use set_comm_table_rtp_rj
      use gen_sph_grids_modes
      use bcast_comm_stacks_sph
!
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_mesh_array), intent(inout) :: sph_array
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
      integer :: ip, id_rank
!
!
      allocate(comm_rlm_mul(sph_array%num_pe))
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      do ip = 1, sph_array%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rlm table generation for', id_rank, ' on ', my_rank
        call const_sph_rlm_modes(id_rank, gen_sph,                      &
     &      sph_array%sph(ip)%sph_rlm, sph_array%comms(ip)%comm_rlm)
        call copy_sph_comm_neib                                         &
     &     (sph_array%comms(ip)%comm_rlm, comm_rlm_mul(ip))
      end do
!
      call para_bcast_comm_stacks_sph(sph_array%num_pe, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      do ip = 1, sph_array%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &     'Construct spherical modes for domain ', id_rank,            &
     &     ' on ', my_rank
        call const_sph_rj_modes                                         &
     &     (id_rank, sph_array%num_pe, comm_rlm_mul, gen_sph,           &
     &      sph_array%sph(ip)%sph_params, sph_array%sph(ip)%sph_rtp,    &
     &      sph_array%sph(ip)%sph_rj, sph_array%comms(ip)%comm_rj,      &
     &      sph_array%sph_grps(ip))
      end do
      call dealloc_comm_stacks_sph(sph_array%num_pe, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
      end subroutine para_gen_sph_rlm_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_rtp_grids(gen_sph, sph_array)
!
      use m_elapsed_labels_gen_SPH
      use gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use bcast_comm_stacks_sph
!
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_mesh_array), intent(inout) :: sph_array
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable:: comm_rtm_mul(:)
      integer :: ip, id_rank
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      allocate(comm_rtm_mul(sph_array%num_pe))
      do ip = 1, sph_array%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rtm table generation for', id_rank, ' on ', my_rank
        call const_sph_rtm_grids(id_rank, gen_sph,                      &
     &      sph_array%sph(ip)%sph_rtm, sph_array%comms(ip)%comm_rtm)
        call copy_sph_comm_neib                                         &
     &     (sph_array%comms(ip)%comm_rtm, comm_rtm_mul(ip))
      end do
      call para_bcast_comm_stacks_sph(sph_array%num_pe, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      do ip = 1, sph_array%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',  id_rank,   &
     &              ' on ',  my_rank
        call const_sph_rtp_grids                                        &
     &     (id_rank, sph_array%num_pe, comm_rtm_mul, gen_sph,           &
     &      sph_array%sph(ip)%sph_params, sph_array%sph(ip)%sph_rtp,    &
     &      sph_array%comms(ip)%comm_rtp, sph_array%sph_grps(ip))
      end do
      call dealloc_comm_stacks_sph(sph_array%num_pe, comm_rtm_mul)
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
      end subroutine para_gen_sph_rtm_rtp_grids
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
