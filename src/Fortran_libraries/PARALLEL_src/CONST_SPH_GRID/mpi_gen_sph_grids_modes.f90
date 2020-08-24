!>@file   mpi_gen_sph_grids_modes.f90
!!@brief  module mpi_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine const_sph_global_parameters(gen_sph, sph)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_grids), intent(inout) :: sph
!!
!!      subroutine mpi_gen_sph_grids(gen_sph, sph, comms_sph, sph_grp)
!!      subroutine mpi_gen_sph_rj_mode(gen_sph, sph, comms_sph, sph_grp)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grp
!!@endverbatim
!
      module mpi_gen_sph_grids_modes
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_const_spherical_grid
      use t_file_IO_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_global_parameters(gen_sph, sph)
!
      use set_global_spherical_param
      use const_sph_radial_grid
      use const_global_sph_grids_modes
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), intent(inout) :: sph
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
      end subroutine const_sph_global_parameters
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_grids                                      &
     &         (gen_sph, sph_tmp, sph, comms_sph, sph_grp)
!
      use t_sph_local_index
!
      use m_elapsed_labels_gen_SPH
      use bcast_comm_stacks_sph
      use set_comm_table_rtp_rj
      use gen_sph_grids_modes
      use copy_para_sph_global_params
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), intent(inout) :: sph_tmp
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), intent(inout) :: sph
!>       Structure of communication table for spherical spectr method
      type(sph_comm_tables), intent(inout) :: comms_sph
!>       Structure of group data for spherical spectr method
      type(sph_group_data), intent(inout) :: sph_grp
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
      type(sph_local_1d_index) :: sph_lcx_m
!
!  =========  Set global resolutions ===================================
!
      call const_sph_global_parameters(gen_sph, sph_tmp)
      call copy_each_sph_param_from_ctl                                 &
     &   (sph_tmp, sph%sph_params, sph%sph_rtp, sph%sph_rj)
      call copy_each_global_sph_resolution                              &
     &   (sph_tmp, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
!  ========= Generate each spherical harmonics table ===================
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      allocate(comm_rlm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rlm table generation for', my_rank
      call const_sph_rlm_modes                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_rlm, comms_sph%comm_rlm)
      call copy_sph_comm_neib                                           &
     &   (comms_sph%comm_rlm, comm_rlm_mul(my_rank+1))
!
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call alloc_rj_1d_local_idx(sph%sph_rj, sph_lcx_m)
      call const_sph_rj_modes                                           &
     &   (my_rank, nprocs, comm_rlm_mul, gen_sph%added_radial_grp,      &
     &    gen_sph%s3d_ranks, gen_sph%s3d_radius,                        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    comms_sph%comm_rj, sph_grp, sph_lcx_m)
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      allocate(comm_rtm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rtm table generation for',  my_rank
      call const_sph_rtm_grids                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_rtm, comms_sph%comm_rtm)
      call copy_sph_comm_neib                                           &
     &   (comms_sph%comm_rtm, comm_rtm_mul(my_rank+1))
!
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical grids for domain ',  my_rank
      call alloc_rtp_1d_local_idx(sph%sph_rtp, sph_lcx_m)
      call const_sph_rtp_grids(my_rank, nprocs, comm_rtm_mul,           &
     &    gen_sph%added_radial_grp, gen_sph%r_layer_grp,                &
     &    gen_sph%med_layer_grp, gen_sph%s3d_ranks, gen_sph%s3d_radius, &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_params, sph%sph_rtp, comms_sph%comm_rtp,              &
     &    sph_grp, sph_lcx_m)
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
!
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rtm_mul)
!
      deallocate(comm_rtm_mul)
      call dealloc_gen_mesh_params(gen_sph)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
      end subroutine mpi_gen_sph_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rj_mode(gen_sph, sph_tmp,                  &
     &          sph, comms_sph, sph_grp)
!
      use t_sph_local_index
!
      use m_elapsed_labels_gen_SPH
      use bcast_comm_stacks_sph
      use set_comm_table_rtp_rj
      use gen_sph_grids_modes
      use copy_para_sph_global_params
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), intent(inout) :: sph_tmp
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), intent(inout) :: sph
!>       Structure of communication table for spherical spectr method
      type(sph_comm_tables), intent(inout) :: comms_sph
!>       Structure of group data for spherical spectr method
      type(sph_group_data), intent(inout) :: sph_grp
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!
      type(sph_local_1d_index) :: sph_lcx_m
!
      integer :: ip
!
!  =========  Set global resolutions ===================================
!
      write(*,*) 'Tako!'
      call const_sph_global_parameters(gen_sph, sph_tmp)
      call copy_each_sph_param_from_ctl                                 &
     &   (sph_tmp, sph%sph_params, sph%sph_rtp, sph%sph_rj)
      call copy_each_global_sph_resolution                              &
     &   (sph_tmp, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
!  ========= Generate each spherical harmonics table ===================
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
      allocate(comm_rlm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rlm table generation for', my_rank
      call const_sph_rlm_modes                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_rlm, comms_sph%comm_rlm)
      ip = my_rank + 1
      call copy_sph_comm_neib(comms_sph%comm_rlm, comm_rlm_mul(ip))
!
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+7)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call alloc_rj_1d_local_idx(sph%sph_rj, sph_lcx_m)
      call const_sph_rj_modes                                           &
     &   (my_rank, nprocs, comm_rlm_mul, gen_sph%added_radial_grp,      &
     &    gen_sph%s3d_ranks, gen_sph%s3d_radius,                        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    comms_sph%comm_rj, sph_grp, sph_lcx_m)
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      call dealloc_gen_mesh_params(gen_sph)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+7)
!
      end subroutine mpi_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      end module mpi_gen_sph_grids_modes
