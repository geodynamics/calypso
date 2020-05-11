!>@file   t_check_and_make_SPH_mesh.f90
!!@brief  module t_check_and_make_SPH_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine check_and_make_SPH_mesh                              &
!!     &         (iflag_make_SPH, sph_file_param, sph_maker)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!      subroutine para_gen_sph_grids(sph_file_param, sph, gen_sph)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_grids), intent(inout) :: sph
!!@endverbatim
!
      module t_check_and_make_SPH_mesh
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_const_spherical_grid
      use t_file_IO_parameter
!
      implicit none
!
      type sph_grid_maker_in_sim
!>        Structure to construct grid
        type(construct_spherical_grid) :: gen_sph
!         Structure for temporal spherical grid
        type(sph_grids) :: sph_tmp
      end type sph_grid_maker_in_sim
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_mesh                                &
     &         (iflag_make_SPH, sph_file_param, sph_maker)
!
      use m_error_IDs
      use parallel_gen_sph_grids
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: iflag_make_SPH
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
      end if
      call MPI_BCAST(iflag_lc, 1, CALYPSO_LOGICAL, 0,                   &
     &               CALYPSO_COMM, ierr_MPI)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(iflag_make_SPH .eq. 0) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        call para_gen_sph_grids                                         &
     &     (sph_file_param, sph_maker%sph_tmp, sph_maker%gen_sph)
        call dealloc_gen_mesh_params(sph_maker%gen_sph)
      end if
!
      end subroutine check_and_make_SPH_mesh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_grids(sph_file_param, sph, gen_sph)
!
      use m_elapsed_labels_gen_SPH
      use set_global_spherical_param
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use bcast_comm_stacks_sph
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
!
!  =========  Set global resolutions ===================================
!
      call set_global_sph_resolution                                    &
     &   (sph%sph_params%l_truncation, sph%sph_params%m_folding,        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter                             &
     &     (sph%sph_params, sph%sph_rtp)
        call output_set_radial_grid                                     &
     &     (sph%sph_params, sph%sph_rtp, gen_sph%s3d_radius)
      end if
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes                               &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rj,         &
     &    gen_sph%s3d_ranks, gen_sph%sph_lcp,                           &
     &    gen_sph%stk_lc1d, gen_sph%sph_gl1d)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rlm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_grids'
      call mpi_gen_sph_rlm_grids(sph_file_param,                        &
     &    gen_sph, sph%sph_params, sph%sph_rlm, comm_rlm_mul)
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call mpi_gen_sph_rj_modes                                         &
     &   (sph_file_param, comm_rlm_mul, sph%sph_params,                 &
     &    gen_sph, sph%sph_rlm, sph%sph_rj)
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rtm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'mpi_gen_sph_rtm_grids'
      call mpi_gen_sph_rtm_grids(sph_file_param,                        &
     &    gen_sph, sph%sph_params, sph%sph_rtm, comm_rtm_mul)
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call mpi_gen_sph_rtp_grids                                        &
     &   (sph_file_param, comm_rtm_mul, sph%sph_params,                 &
     &    gen_sph, sph%sph_rtp, sph%sph_rtm)
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rtm_mul)
!
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      end subroutine para_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      end module t_check_and_make_SPH_mesh
