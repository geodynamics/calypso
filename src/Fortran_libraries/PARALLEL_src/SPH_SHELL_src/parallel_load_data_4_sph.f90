!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh                           &
!!     &         (iflag_access_FEM, sph, comms_sph, sph_grps,           &
!!     &          mesh, group, ele_mesh, mesh_file, gen_sph)
!!      subroutine load_para_SPH_rj_mesh(sph, comms_sph, sph_grps)
!!      subroutine load_para_sph_mesh(sph, bc_rtp_grp, sph_grps)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!
!!      subroutine load_para_rj_mesh                                    &
!!     &         (sph_params, sph_rj, comm_rj, sph_grps)
!!         type(sph_shell_parameters), intent(inout) :: sph_params
!!         type(sph_rtp_grid), intent(inout) :: sph_rtp
!!         type(sph_rtm_grid), intent(inout) :: sph_rtm
!!         type(sph_rlm_grid), intent(inout) :: sph_rlm
!!         type(sph_rj_grid), intent(inout) :: sph_rj
!!         type(sph_comm_tbl), intent(inout) :: comm_rj
!!         type(sph_group_data), intent(inout) ::  sph_grps
!!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
!
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_mesh
      use t_spheric_data_IO
      use t_const_spherical_grid
      use t_sph_local_parameter
      use sph_file_MPI_IO_select
      use set_loaded_data_4_sph
!
      implicit none
!
      private :: load_FEM_mesh_4_SPH
!
      type(sph_file_data_type), save, private :: sph_file_l
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh                             &
     &         (iflag_access_FEM, sph, comms_sph, sph_grps,             &
     &          mesh, group, ele_mesh, mesh_file, gen_sph)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: iflag_access_FEM
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      call load_FEM_mesh_4_SPH(iflag_access_FEM,                        &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    sph_grps%radial_rtp_grp, sph_grps%radial_rj_grp,              &
     &    mesh, group, ele_mesh, mesh_file, gen_sph)
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_rj_mesh(sph, comms_sph, sph_grps)
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
!
      call load_para_rj_mesh                                            &
     &   (sph%sph_params, sph%sph_rj, comms_sph%comm_rj, sph_grps)
!
      end subroutine load_para_SPH_rj_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH(iflag_access_FEM, sph_params,      &
     &          sph_rtp, sph_rj, radial_rtp_grp, radial_rj_grp,         &
     &          mesh, group, ele_mesh, mesh_file, gen_sph)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use m_spheric_constants
      use mpi_load_mesh_data
      use copy_mesh_structures
      use const_FEM_mesh_sph_mhd
      use gen_sph_grids_modes
      use mesh_IO_select
      use mesh_file_name_by_param
!
      integer(kind = kint), intent(in) :: iflag_access_FEM
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp
      type(group_data), intent(in) :: radial_rj_grp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      type(mesh_data) :: femmesh_s
!
!
!  --  load FEM mesh data
      if(check_exist_mesh(mesh_file, my_rank) .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        call mpi_input_mesh(mesh_file, nprocs, mesh, group,             &
     &      ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
        call set_fem_center_mode_4_SPH                                  &
     &     (mesh%node%internal_node, sph_rtp, sph_params)
        return
      end if
!
!  --  Construct FEM mesh
      if(sph_params%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph_rj%iflag_rj_center .gt. 0) then
          sph_params%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph_params%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call const_FEM_mesh_4_sph_mhd(iflag_access_FEM,                   &
     &    sph_params, sph_rtp, sph_rj, radial_rtp_grp, radial_rj_grp,   &
     &    femmesh_s%mesh, femmesh_s%group, mesh_file, gen_sph)
!      call compare_mesh_type                                           &
!     &   (my_rank, mesh%nod_comm, mesh%node, mesh%ele, femmesh_s%mesh)
!      call compare_mesh_groups(group%nod_grp, femmesh_s%group)
!
      call set_mesh_data_from_type(femmesh_s%mesh, femmesh_s%group,     &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      ele_mesh%surf, ele_mesh%edge,                               &
     &      group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
      use set_from_recv_buf_rev
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtp_sph_trans'
      call sel_mpi_read_geom_rtp_file                                   &
     &   (nprocs, my_rank, sph_file_l)
      call input_geom_rtp_sph_trans(sph_file_l, sph%sph_rtp,            &
     &    comms_sph%comm_rtp, sph_grps, sph%sph_params, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, 'error in RTP mesh')
!
      call set_reverse_import_table(sph%sph_rtp%nnod_rtp,               &
     &    comms_sph%comm_rtp%ntot_item_sr, comms_sph%comm_rtp%item_sr,  &
     &    comms_sph%comm_rtp%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call sel_mpi_read_spectr_rj_file                                  &
     &   (nprocs, my_rank, sph_file_l)
      call input_modes_rj_sph_trans(sph_file_l, sph%sph_rj,             &
     &    comms_sph%comm_rj, sph_grps, sph%sph_params, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, 'error in RJ mesh')
!
      call set_reverse_import_table(sph%sph_rj%nnod_rj,                 &
     &    comms_sph%comm_rj%ntot_item_sr, comms_sph%comm_rj%item_sr,    &
     &    comms_sph%comm_rj%irev_sr)
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtm_sph_trans'
      call sel_mpi_read_geom_rtm_file                                   &
     &   (nprocs, my_rank, sph_file_l)
      call input_geom_rtm_sph_trans(sph_file_l,                         &
     &    sph%sph_rtm, comms_sph%comm_rtm, sph%sph_params, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, 'error in RTM mesh')
!
      call set_reverse_import_table(sph%sph_rtm%nnod_rtm,               &
     &    comms_sph%comm_rtm%ntot_item_sr, comms_sph%comm_rtm%item_sr,  &
     &    comms_sph%comm_rtm%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rlm_sph_trans'
      call sel_mpi_read_modes_rlm_file                                  &
     &   (nprocs, my_rank, sph_file_l)
      call input_modes_rlm_sph_trans(sph_file_l,                        &
     &    sph%sph_rlm, comms_sph%comm_rlm, sph%sph_params, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, 'error in RLM mesh')
!
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
      end subroutine load_para_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine load_para_rj_mesh                                      &
     &         (sph_params, sph_rj, comm_rj, sph_grps)
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_group_data), intent(inout) ::  sph_grps
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call sel_mpi_read_spectr_rj_file(nprocs, my_rank, sph_file_l)
      call input_modes_rj_sph_trans(sph_file_l,                         &
     &    sph_rj, comm_rj, sph_grps, sph_params, ierr)
      call set_reverse_import_table(sph_rj%nnod_rj,                     &
     &    comm_rj%ntot_item_sr, comm_rj%item_sr, comm_rj%irev_sr)
!
      call set_index_flags_4_rj(sph_rj, comm_rj)
!
      end subroutine load_para_rj_mesh
!
! -----------------------------------------------------------------------
!
      end module parallel_load_data_4_sph
