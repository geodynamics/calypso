!>@file   compare_sph_with_IO.f90
!!@brief  module compare_sph_with_IO
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Compare spherical harmonics index data and file
!!
!!@verbatim
!!      integer(kind = kint) function s_compare_sph_with_IO             &
!!     &                   (sph_file_param, sph, comms_sph, sph_grps)
!!        type(field_IO_params), intent(in) ::  sph_file_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_group_data), intent(in) ::  sph_grps
!@endverbatim
!
      module compare_sph_with_IO
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
      private :: compare_sph_rj_with_IO,  compare_sph_rtp_with_IO
      private :: compare_sph_rlm_with_IO, compare_sph_rtm_with_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function s_compare_sph_with_IO               &
     &                   (sph_file_param, sph, comms_sph, sph_grps)
!
      use load_data_for_sph_IO
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_group_data), intent(in) ::  sph_grps
!
      integer(kind = kint) :: iflag
!
!
      s_compare_sph_with_IO = 1
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rtp_from_IO'
      call sel_mpi_read_geom_rtp_file                                   &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      iflag = compare_sph_rtp_with_IO(sph%sph_params, sph%sph_rtp,      &
     &                        comms_sph%comm_rtp, sph_grps, sph_file_l)
      if(iflag .gt. 0) return
      call dealloc_rtp_grid_IO(sph_file_l)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rj_from_IO'
      call sel_mpi_read_spectr_rj_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      iflag = compare_sph_rj_with_IO(sph%sph_params, sph%sph_rj,        &
     &                         comms_sph%comm_rj, sph_grps, sph_file_l)
      if(iflag .gt. 0) return
      call dealloc_rj_mode_IO(sph_file_l)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rtm_from_IO'
      call sel_mpi_read_geom_rtm_file                                   &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      iflag = compare_sph_rtm_with_IO(sph%sph_params, sph%sph_rtm,      &
     &                                comms_sph%comm_rtm, sph_file_l)
      if(iflag .gt. 0) return
      call dealloc_rtm_grid_IO(sph_file_l)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_trans_rlm_from_IO'
      call sel_mpi_read_modes_rlm_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_l)
      iflag = compare_sph_rlm_with_IO(sph%sph_params, sph%sph_rlm,      &
     &                                comms_sph%comm_rlm, sph_file_l)
      if(iflag .gt. 0) return
      call dealloc_rlm_mode_IO(sph_file_l)
      s_compare_sph_with_IO = 0
!
      end function s_compare_sph_with_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_rtp_with_IO             &
     &         (sph_params, sph_rtp, comm_rtp, sph_grps, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_rtp_node_4_IO
      use set_group_types_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(sph_group_data), intent(in) :: sph_grps
!
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      compare_sph_rtp_with_IO                                           &
     &      = compare_sph_rtp_node_with_IO(sph_params%l_truncation,     &
     &                                     sph_rtp, sph_file%sph_IO)
      if(compare_sph_rtp_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTP indexing'
        return
      end if
      compare_sph_rtp_with_IO                                           &
     &      = compare_comm_sph_with_comm_tbl(comm_rtp,                  &
     &                                       sph_file%comm_IO)
      if(compare_sph_rtp_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTP communication'
        return
      end if
!
      compare_sph_rtp_with_IO                                           &
     &      = compare_gruop_stracture(sph_grps%bc_rtp_grp,              &
     &                                sph_file%sph_grp_IO%bc_rtp_grp)
      if(compare_sph_rtp_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTP grouping'
        return
      end if
      compare_sph_rtp_with_IO                                           &
     &      = compare_gruop_stracture(sph_grps%radial_rtp_grp,          &
     &                              sph_file%sph_grp_IO%radial_rtp_grp)
      if(compare_sph_rtp_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTP radial group'
        return
      end if
      compare_sph_rtp_with_IO                                           &
     &      = compare_gruop_stracture(sph_grps%theta_rtp_grp,           &
     &                               sph_file%sph_grp_IO%theta_rtp_grp)
      if(compare_sph_rtp_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTP theta group'
        return
      end if
      compare_sph_rtp_with_IO                                           &
     &      = compare_gruop_stracture(sph_grps%zonal_rtp_grp,           &
     &                               sph_file%sph_grp_IO%zonal_rtp_grp)
      if(compare_sph_rtp_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTP phi group'
        return
      end if
      compare_sph_rtp_with_IO = 0
!
      end function compare_sph_rtp_with_IO
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_rj_with_IO              &
     &             (sph_params, sph_rj, comm_rj, sph_grps, sph_file)
!
      use copy_sph_comm_table_4_IO
      use set_group_types_4_IO
      use copy_sph_rj_mode_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid),  intent(in) :: sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(sph_group_data), intent(in) :: sph_grps
!
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      compare_sph_rj_with_IO                                            &
     &      = compare_sph_node_rj_with_IO(sph_params%l_truncation,      &
     &                                    sph_rj, sph_file%sph_IO)
      if(compare_sph_rj_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RJ indexing'
        return
      end if
      compare_sph_rj_with_IO                                            &
     &      = compare_comm_sph_with_comm_tbl(comm_rj, sph_file%comm_IO)
      if(compare_sph_rj_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RJ communication'
        return
      end if
!
      compare_sph_rj_with_IO                                            &
     &      = compare_gruop_stracture(sph_grps%radial_rj_grp,           &
     &                               sph_file%sph_grp_IO%radial_rj_grp)
      if(compare_sph_rj_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RJ radial group'
        return
      end if
      compare_sph_rj_with_IO                                            &
     &      = compare_gruop_stracture(sph_grps%sphere_rj_grp,           &
     &                               sph_file%sph_grp_IO%sphere_rj_grp)
      if(compare_sph_rj_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RJ horiz. group'
        return
      end if
      compare_sph_rj_with_IO = 0
!
      end function compare_sph_rj_with_IO
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_rtm_with_IO             &
     &                   (sph_params, sph_rtm, comm_rtm, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_rtm_node_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm
!
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      compare_sph_rtm_with_IO                                           &
     &      = compare_sph_rtm_node_with_IO(sph_params%l_truncation,     &
     &                                     sph_rtm, sph_file%sph_IO)
      if(compare_sph_rtm_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTM indexing'
        return
      end if
      compare_sph_rtm_with_IO                                           &
     &      = compare_comm_sph_with_comm_tbl(comm_rtm,                  &
     &                                       sph_file%comm_IO)
      if(compare_sph_rtm_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RTM communication'
        return
      end if
      compare_sph_rtm_with_IO = 0
!
      end function compare_sph_rtm_with_IO
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_rlm_with_IO             &
     &                   (sph_params, sph_rlm, comm_rlm, sph_file)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_rlm_mode_4_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
!
      type(sph_file_data_type), intent(in) :: sph_file
!
!
      compare_sph_rlm_with_IO                                           &
     &      = compare_sph_mode_rlm_with_IO(sph_params%l_truncation,     &
     &                                     sph_rlm, sph_file%sph_IO)
      if(compare_sph_rlm_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RLM indexing'
        return
      end if
      compare_sph_rlm_with_IO                                           &
     &      = compare_comm_sph_with_comm_tbl(comm_rlm,                  &
     &                                       sph_file%comm_IO)
      if(compare_sph_rlm_with_IO .gt. 0) then
        write(e_message,'(a)') 'Wrong information in RLM communication'
        return
      end if
      compare_sph_rlm_with_IO = 0
!
      end function compare_sph_rlm_with_IO
!
! -----------------------------------------------------------------------
!
      end module compare_sph_with_IO
