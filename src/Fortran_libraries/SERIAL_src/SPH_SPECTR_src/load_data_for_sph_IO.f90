!>@file   load_data_for_sph_IO.f90
!!@brief  module load_data_for_sph_IO
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief load spherical harmonics indexing data
!!
!!@verbatim
!!      subroutine input_geom_rtp_sph_trans(sph_grps_IO,                &
!!     &          sph_rtp, comm_rtp, sph_grps, sph_params, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(sph_group_data), intent(inout) :: sph_grps
!!      subroutine input_modes_rj_sph_trans(sph_grps_IO,                &
!!     &          sph_rj, comm_rj, sph_grps, sph_params, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rj_grid),  intent(inout) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(sph_group_data), intent(inout) :: sph_grps
!!      subroutine input_geom_rtm_sph_trans                             &
!!     &         (sph_file, sph_rtm, comm_rtm, sph_params, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!      subroutine input_modes_rlm_sph_trans                            &
!!     &         (sph_file, sph_rlm, comm_rlm, sph_params, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!
!!      subroutine output_geom_rtp_sph_trans                            &
!!     &         (sph_params, sph_rtp, comm_rtp, sph_grps, sph_file)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(sph_group_data), intent(inout) :: sph_grps
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!      subroutine output_modes_rj_sph_trans(sph_params,                &
!!     &          sph_rj, comm_rj, sph_grps, sph_file)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid),  intent(in) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(sph_group_data), intent(inout) :: sph_grps
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!      subroutine output_geom_rtm_sph_trans                            &
!!     &         (sph_params, sph_rtm, comm_rtm, sph_file)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!      subroutine output_modes_rlm_sph_trans                           &
!!     &         (sph_params, sph_rlm, comm_rlm, sph_file)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!@endverbatim
!
      module load_data_for_sph_IO
!
      use m_precision
!
      use t_spheric_mesh
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
      use t_sph_trans_comm_tbl
      use t_group_data
      use t_spheric_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtp_sph_trans(sph_file,                     &
     &          sph_rtp, comm_rtp, sph_grps, sph_params, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
      use count_num_sph_smp
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_group_data), intent(inout) :: sph_grps
      type(sph_shell_parameters), intent(inout) :: sph_params
      integer(kind = kint), intent(inout) :: ierr
!
!
      call copy_sph_node_4_rtp_from_IO                                  &
     &   (sph_file%sph_IO, sph_rtp, sph_params%l_truncation)
      call copy_comm_sph_from_IO                                        &
     &   (sph_rtp%nnod_rtp, sph_file%comm_IO, comm_rtp)
!
      call set_gruop_stracture                                          &
     &   (sph_file%sph_grp_IO%bc_rtp_grp, sph_grps%bc_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_file%sph_grp_IO%radial_rtp_grp, sph_grps%radial_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_file%sph_grp_IO%theta_rtp_grp, sph_grps%theta_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_file%sph_grp_IO%zonal_rtp_grp, sph_grps%zonal_rtp_grp)
!
      call deallocate_grp_type(sph_file%sph_grp_IO%bc_rtp_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%radial_rtp_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%theta_rtp_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%zonal_rtp_grp)
!
      call count_num_rtp_smp(sph_rtp, ierr)
!
      end subroutine input_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rj_sph_trans(sph_file,                     &
     &          sph_rj, comm_rj, sph_grps, sph_params, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
      use count_num_sph_smp
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
      type(sph_rj_grid),  intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_group_data), intent(inout) :: sph_grps
      type(sph_shell_parameters), intent(inout) :: sph_params
      integer(kind = kint), intent(inout) :: ierr
!
!
      call copy_sph_node_4_rj_from_IO                                   &
     &   (sph_file%sph_IO, sph_rj, sph_params%l_truncation)
      call copy_comm_sph_from_IO                                        &
     &   (sph_rj%nnod_rj, sph_file%comm_IO, comm_rj)
!
      call set_gruop_stracture                                          &
     &   (sph_file%sph_grp_IO%radial_rj_grp, sph_grps%radial_rj_grp)
      call set_gruop_stracture                                          &
     &   (sph_file%sph_grp_IO%sphere_rj_grp, sph_grps%sphere_rj_grp)
!
      call deallocate_grp_type(sph_file%sph_grp_IO%radial_rj_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%sphere_rj_grp)
!
      call count_num_rj_smp(sph_rj, ierr)
!
      end subroutine input_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtm_sph_trans                               &
     &         (sph_file, sph_rtm, comm_rtm, sph_params, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use count_num_sph_smp
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_shell_parameters), intent(inout) :: sph_params
      integer(kind = kint), intent(inout) :: ierr
!
!
      call copy_sph_node_4_rtm_from_IO                                  &
     &   (sph_file%sph_IO, sph_rtm, sph_params%l_truncation)
      call copy_comm_sph_from_IO                                        &
     &   (sph_rtm%nnod_rtm, sph_file%comm_IO, comm_rtm)
!
      call count_num_rtm_smp(sph_rtm, ierr)
!
      end subroutine input_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rlm_sph_trans                              &
     &         (sph_file, sph_rlm, comm_rlm, sph_params, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use count_num_sph_smp
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_shell_parameters), intent(inout) :: sph_params
      integer(kind = kint), intent(inout) :: ierr
!
!
      call copy_sph_node_4_rlm_from_IO                                  &
     &   (sph_file%sph_IO, sph_rlm, sph_params%l_truncation)
      call copy_comm_sph_from_IO                                        &
     &   (sph_rlm%nnod_rlm, sph_file%comm_IO, comm_rlm)
!
      call count_num_rlm_smp(sph_rlm, ierr)
!
      end subroutine input_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtp_sph_trans                              &
     &         (sph_params, sph_rtp, comm_rtp, sph_grps, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_group_data), intent(inout) :: sph_grps
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      call copy_sph_node_4_rtp_to_IO                                    &
     &   (sph_params%l_truncation, sph_rtp, sph_file%sph_IO)
      call copy_comm_sph_to_comm_tbl(comm_rtp, sph_file%comm_IO)
!
      call set_gruop_stracture                                          &
     &   (sph_grps%bc_rtp_grp, sph_file%sph_grp_IO%bc_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%radial_rtp_grp, sph_file%sph_grp_IO%radial_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%theta_rtp_grp, sph_file%sph_grp_IO%theta_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%zonal_rtp_grp, sph_file%sph_grp_IO%zonal_rtp_grp)
!
      call deallocate_grp_type(sph_grps%bc_rtp_grp)
      call deallocate_grp_type(sph_grps%radial_rtp_grp)
      call deallocate_grp_type(sph_grps%theta_rtp_grp)
      call deallocate_grp_type(sph_grps%zonal_rtp_grp)
!
      end subroutine output_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rj_sph_trans(sph_params,                  &
     &          sph_rj, comm_rj, sph_grps, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid),  intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_group_data), intent(inout) :: sph_grps
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      call copy_sph_node_4_rj_to_IO                                     &
     &   (sph_params%l_truncation, sph_rj, sph_file%sph_IO)
      call copy_comm_sph_to_comm_tbl(comm_rj, sph_file%comm_IO)
!
      call set_gruop_stracture                                          &
     &   (sph_grps%radial_rj_grp, sph_file%sph_grp_IO%radial_rj_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%sphere_rj_grp, sph_file%sph_grp_IO%sphere_rj_grp)
!
      call deallocate_grp_type(sph_grps%radial_rj_grp)
      call deallocate_grp_type(sph_grps%sphere_rj_grp)
!
      end subroutine output_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtm_sph_trans                              &
     &         (sph_params, sph_rtm, comm_rtm, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      call copy_sph_node_4_rtm_to_IO                                    &
     &   (sph_params%l_truncation, sph_rtm, sph_file%sph_IO)
      call copy_comm_sph_to_comm_tbl(comm_rtm, sph_file%comm_IO)
!
      end subroutine output_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rlm_sph_trans                             &
     &         (sph_params, sph_rlm, comm_rlm, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      call copy_sph_node_4_rlm_to_IO                                    &
     &   (sph_params%l_truncation, sph_rlm, sph_file%sph_IO)
      call copy_comm_sph_to_comm_tbl(comm_rlm, sph_file%comm_IO)
!
      end subroutine output_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
!
      end module load_data_for_sph_IO
