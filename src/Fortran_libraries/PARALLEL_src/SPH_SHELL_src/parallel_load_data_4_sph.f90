!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_sph_mesh                                        &
!!     &         (sph_file_param, sph, comms_sph, sph_grps)
!!      subroutine load_sph_rj_mesh                                     &
!!     &         (sph_file_param, sph, comms_sph, sph_grps)
!!        type(field_IO_params), intent(in) ::  sph_file_param
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!
!!      subroutine output_sph_mesh                                      &
!!     &         (sph_file_param, sph, comms_sph, sph_grps)
!!      subroutine output_sph_rj_mesh                                   &
!!     &         (sph_file_param, sph, comms_sph, sph_grps)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grps
!!
!!      subroutine sph_index_flags_and_params(sph_grps, sph, comms_sph)
!!        type(sph_group_data), intent(in) ::  sph_grps
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!      subroutine sph_rj_index_flags_and_params                        &
!!     &         (sph_grps, sph_params, sph_rj, comm_rj)
!!        type(sph_group_data), intent(in) ::  sph_grps
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!
!!      subroutine set_radius_dat_sph_MHD                               &
!!     &         (radial_rj_grp, sph_params, sph_rj)
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!
!!      subroutine dealloc_sph_modes(sph, comms_sph, sph_grps)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grps
!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_spheric_data_IO
      use t_const_spherical_grid
      use t_sph_local_parameter
      use t_file_IO_parameter
      use sph_file_MPI_IO_select
      use set_loaded_data_4_sph
!
      implicit none
!
      type(sph_file_data_type), save, private :: sph_file_l
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_sph_mesh                                          &
     &         (sph_file_param, sph, comms_sph, sph_grps)
!
      use load_data_for_sph_IO
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rtp_from_IO'
      call sel_mpi_read_geom_rtp_file                                   &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      call copy_sph_trans_rtp_from_IO(sph_file_l, sph%sph_rtp,          &
     &    comms_sph%comm_rtp, sph_grps, sph%sph_params)
      call dealloc_rtp_grid_IO(sph_file_l)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rj_from_IO'
      call sel_mpi_read_spectr_rj_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      call copy_sph_trans_rj_from_IO(sph_file_l, sph%sph_rj,            &
     &    comms_sph%comm_rj, sph_grps, sph%sph_params)
      call dealloc_rj_mode_IO(sph_file_l)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rtm_from_IO'
      call sel_mpi_read_geom_rtm_file                                   &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      call copy_sph_trans_rtm_from_IO(sph_file_l,                       &
     &    sph%sph_rtm, comms_sph%comm_rtm, sph%sph_params)
      call dealloc_rtm_grid_IO(sph_file_l)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rlm_from_IO'
      call sel_mpi_read_modes_rlm_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      call copy_sph_trans_rlm_from_IO(sph_file_l,                       &
     &    sph%sph_rlm, comms_sph%comm_rlm, sph%sph_params)
      call dealloc_rlm_mode_IO(sph_file_l)
!
      end subroutine load_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine load_sph_rj_mesh                                       &
     &         (sph_file_param, sph, comms_sph, sph_grps)
!
      use load_data_for_sph_IO
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rj_from_IO'
      call sel_mpi_read_spectr_rj_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      call copy_sph_trans_rj_from_IO(sph_file_l,                        &
     &    sph%sph_rj, comms_sph%comm_rj, sph_grps, sph%sph_params)
      call dealloc_rj_mode_IO(sph_file_l)
!
      end subroutine load_sph_rj_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_sph_mesh                                        &
     &         (sph_file_param, sph, comms_sph, sph_grps)
!
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      call copy_sph_trans_rlm_to_IO(sph%sph_params, sph%sph_rlm,        &
     &                              comms_sph%comm_rlm, sph_file_l)
      call sel_mpi_write_modes_rlm_file(sph_file_param, sph_file_l)
      call dealloc_rlm_mode_IO(sph_file_l)
      write(*,'(a,i6,a)') 'Spherical transform table for domain',       &
     &          my_rank, ' is done.'
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'copy_sph_trans_rj_to_IO', my_rank
      call copy_sph_trans_rj_to_IO(sph%sph_params,                      &
     &    sph%sph_rj, comms_sph%comm_rj, sph_grps, sph_file_l)
      call sel_mpi_write_spectr_rj_file(sph_file_param, sph_file_l)
      call dealloc_rj_mode_IO(sph_file_l)
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
!
!
      call copy_sph_trans_rtm_to_IO(sph%sph_params, sph%sph_rtm,        &
     &    comms_sph%comm_rtm, sph_file_l)
      call sel_mpi_write_geom_rtm_file(sph_file_param, sph_file_l)
      call dealloc_rtm_grid_IO(sph_file_l)
      write(*,'(a,i6,a)') 'Legendre transform table rtm',               &
     &          my_rank, ' is done.'
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'copy_sph_trans_rtp_to_IO', my_rank
      call copy_sph_trans_rtp_to_IO(sph%sph_params,                     &
     &    sph%sph_rtp, comms_sph%comm_rtp, sph_grps, sph_file_l)
      call sel_mpi_write_geom_rtp_file(sph_file_param, sph_file_l)
      call dealloc_rtp_grid_IO(sph_file_l)
      write(*,'(a,i6,a)') 'Spherical grids for domain',                 &
     &          my_rank, ' is done.'
!
      end subroutine output_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine output_sph_rj_mesh                                     &
     &         (sph_file_param, sph, comms_sph, sph_grps)
!
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'copy_sph_trans_rj_to_IO', my_rank
      call copy_sph_trans_rj_to_IO(sph%sph_params,                      &
     &    sph%sph_rj, comms_sph%comm_rj, sph_grps, sph_file_l)
      call sel_mpi_write_spectr_rj_file(sph_file_param, sph_file_l)
      call dealloc_rj_mode_IO(sph_file_l)
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
!
      end subroutine output_sph_rj_mesh
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_index_flags_and_params(sph_grps, sph, comms_sph)
!
      use set_from_recv_buf_rev
      use count_num_sph_smp
      use set_loaded_data_4_sph
      use pole_sph_transform
!
      type(sph_group_data), intent(in) ::  sph_grps
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      integer(kind = kint) :: ierr
!
!
      ierr = 0
      call count_num_rtp_smp(sph%sph_rtp, ierr)
      call count_num_rj_smp(sph%sph_rj, ierr)
      call count_num_rtm_smp(sph%sph_rtm, ierr)
      call count_num_rlm_smp(sph%sph_rlm, ierr)
!
      call set_reverse_import_table(sph%sph_rtp%nnod_rtp,               &
     &    comms_sph%comm_rtp%ntot_item_sr, comms_sph%comm_rtp%item_sr,  &
     &    comms_sph%comm_rtp%irev_sr)
      call set_reverse_import_table(sph%sph_rj%nnod_rj,                 &
     &    comms_sph%comm_rj%ntot_item_sr, comms_sph%comm_rj%item_sr,    &
     &    comms_sph%comm_rj%irev_sr)
      call set_reverse_import_table(sph%sph_rtm%nnod_rtm,               &
     &    comms_sph%comm_rtm%ntot_item_sr, comms_sph%comm_rtm%item_sr,  &
     &    comms_sph%comm_rtm%irev_sr)
      call set_reverse_import_table(sph%sph_rlm%nnod_rlm,               &
     &    comms_sph%comm_rlm%ntot_item_sr, comms_sph%comm_rlm%item_sr,  &
     &    comms_sph%comm_rlm%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'set_index_flags_4_SPH'
      call set_index_flags_4_SPH(sph%sph_params,                        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj,            &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
      call set_radius_dat_sph_MHD                                       &
     &   (sph_grps%radial_rj_grp, sph%sph_params, sph%sph_rj)
      call init_pole_grids(sph%sph_rtp)
!
      end subroutine sph_index_flags_and_params
!
! ----------------------------------------------------------------------
!
      subroutine sph_rj_index_flags_and_params                          &
     &         (sph_grps, sph_params, sph_rj, comm_rj)
!
      use set_from_recv_buf_rev
      use count_num_sph_smp
      use set_loaded_data_4_sph
      use pole_sph_transform
!
      type(sph_group_data), intent(in) ::  sph_grps
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      integer(kind = kint) :: ierr
!
!
      ierr = 0
      call count_num_rj_smp(sph_rj, ierr)
!
      call set_reverse_import_table(sph_rj%nnod_rj,                     &
     &    comm_rj%ntot_item_sr, comm_rj%item_sr, comm_rj%irev_sr)
      call set_index_flags_4_rj(sph_rj, comm_rj)
!
      call set_radius_dat_sph_MHD                                       &
     &   (sph_grps%radial_rj_grp, sph_params, sph_rj)
!
      end subroutine sph_rj_index_flags_and_params
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_radius_dat_sph_MHD                                 &
     &         (radial_rj_grp, sph_params, sph_rj)
!
      use set_radius_4_sph_dynamo
!
      type(group_data), intent(in) :: radial_rj_grp
!
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_shell_parameters), intent(inout) :: sph_params
!
!* --------  radius  --------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_radius_dat_4_sph_dynamo'
      call set_radius_dat_4_sph_dynamo                                  &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, radial_rj_grp,      &
     &    sph_params%iflag_radial_grid, sph_params%nlayer_ICB,          &
     &    sph_params%nlayer_CMB, sph_params%nlayer_2_center,            &
     &    sph_rj%ar_1d_rj, sph_rj%r_ele_rj, sph_rj%ar_ele_rj,           &
     &    sph_params%radius_ICB, sph_params%radius_CMB,                 &
     &    sph_params%R_earth)
!
      end subroutine set_radius_dat_sph_MHD
!
!  -------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_modes(sph, comms_sph, sph_grps)
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      call dealloc_sph_grid_group(sph_grps)
      call dealloc_sph_mode_group(sph_grps)
!
      call dealloc_sph_comms_item(comms_sph)
      call dealloc_spheric_parameter(sph)
      call dealloc_sph_1d_indices(sph)
!
      end subroutine dealloc_sph_modes
!
! ----------------------------------------------------------------------
!
      end module parallel_load_data_4_sph
