!>@file   t_check_and_make_SPH_mesh.f90
!!@brief  module t_check_and_make_SPH_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh(FEM_mesh_flags,           &
!!     &          sph_file_param, sph, comms_sph, sph_grps, geofem,     &
!!     &          mesh_file, sph_maker)
!!      subroutine const_FEM_mesh_4_SPH(FEM_mesh_flags,                 &
!!     &          sph_file_param, sph, comms_sph, sph_grps, geofem,     &
!!     &          mesh_file, sph_maker)
!!        type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
!!        type(field_IO_params), intent(in) ::  sph_file_param
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_data), intent(inout) :: geofem
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!      subroutine check_and_make_SPH_mesh(sph_file_param, sph_maker,   &
!!     &          sph, comms_sph, sph_grps)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grps
!!      subroutine check_and_make_SPH_rj_mode(sph_file_param, sph_maker,&
!!     &          sph, comms_sph, sph_grps)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grps
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
      use m_elapsed_labels_gen_SPH
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_const_spherical_grid
      use t_file_IO_parameter
!
      implicit none
!
!>      Structure to check and construct spherical shell mesh
      type sph_grid_maker_in_sim
!>        Switch to construct spherical shell grid
        logical :: make_SPH_flag =    .FALSE.
!>        Switch to output constructed spherical shell grid
        logical :: mesh_output_flag = .FALSE.
!
!>        Structure to construct grid
        type(construct_spherical_grid) :: gen_sph
!>        Structure for temporal spherical grid
        type(sph_grids) :: sph_tmp
      end type sph_grid_maker_in_sim
!
      private :: load_FEM_mesh_4_SPH
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh(FEM_mesh_flags,             &
     &          sph_file_param, sph, comms_sph, sph_grps, geofem,       &
     &          mesh_file, sph_maker)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
!  Check and construct spherical shell table
      call check_and_make_SPH_mesh(sph_file_param, sph_maker,           &
     &                             sph, comms_sph, sph_grps)
!
!  --  load geofem mesh data
      if(check_exist_mesh(my_rank, mesh_file)) then
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        call mpi_input_mesh(mesh_file, nprocs, geofem)
        call set_fem_center_mode_4_SPH                                  &
     &     (geofem%mesh%node%internal_node,                             &
     &      sph%sph_rtp, sph%sph_params)
      else
        call copy_sph_radial_groups(sph_grps, sph_maker%gen_sph)
!  --  Construct FEM mesh
        mesh_file%file_prefix = sph_file_param%file_prefix
        call load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,             &
     &      sph%sph_params, sph%sph_rtp, sph%sph_rj,                    &
     &      geofem, sph_maker%gen_sph)
        call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
      end if
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_SPH(FEM_mesh_flags,                   &
     &          sph_file_param, sph, comms_sph, sph_grps, geofem,       &
     &          mesh_file, sph_maker)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_group_data), intent(in) ::  sph_grps
      type(sph_grids), intent(inout) :: sph
!
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
!
      call copy_sph_radial_groups(sph_grps, sph_maker%gen_sph)
!  --  Construct FEM mesh
      mesh_file%file_prefix = sph_file_param%file_prefix
      call load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,               &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    geofem, sph_maker%gen_sph)
      call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
!
      end subroutine const_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_mesh(sph_file_param, sph_maker,     &
     &          sph, comms_sph, sph_grps)
!
      use m_error_IDs
      use calypso_mpi_logical
      use output_gen_sph_grid_modes
      use mpi_gen_sph_grids_modes
      use sph_file_IO_select
      use check_sph_file_access
      use parallel_load_data_4_sph
      use check_sph_mhd_openmp_size
!
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grps
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
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
        call load_sph_mesh(sph_file_param, sph, comms_sph, sph_grps)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
        call mpi_gen_sph_grids(sph_maker%gen_sph,                       &
     &      sph_maker%sph_tmp, sph, comms_sph, sph_grps)
        if(sph_maker%mesh_output_flag) then
          call output_sph_mesh(sph_file_param,                          &
     &                         sph, comms_sph, sph_grps)
        end if
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params(sph_grps, sph, comms_sph)
!
      call s_check_sph_mhd_openmp_size(sph)
!
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_rj_mode(sph_file_param, sph_maker,  &
     &          sph, comms_sph, sph_grps)
!
      use m_error_IDs
      use calypso_mpi_logical
      use mpi_gen_sph_grids_modes
      use output_gen_sph_grid_modes
      use sph_file_IO_select
      use check_sph_file_access
      use parallel_load_data_4_sph
      use set_from_recv_buf_rev
!
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grps
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
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
        call load_sph_rj_mesh                                           &
     &     (sph_file_param, sph%sph_params, sph%sph_rj,                 &
     &      comms_sph%comm_rj, sph_grps)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
        call mpi_gen_sph_grids(sph_maker%gen_sph,                       &
     &      sph_maker%sph_tmp, sph, comms_sph, sph_grps)
!
        if(sph_maker%mesh_output_flag) then
          call output_sph_mesh(sph_file_param,                          &
     &                         sph, comms_sph, sph_grps)
        end if
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
      end if
!
      call sph_rj_index_flags_and_params                                &
     &   (sph_grps, sph%sph_params, sph%sph_rj, comms_sph%comm_rj)
!
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_rj_mode
!
! ----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH                                    &
     &         (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj, &
     &          geofem, gen_sph)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use m_spheric_constants
      use copy_mesh_structures
      use const_FEM_mesh_sph_mhd
      use gen_sph_grids_modes
      use mesh_IO_select
      use set_nnod_4_ele_by_type
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  mesh_file
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(mesh_data), intent(inout) :: geofem
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      type(mesh_data) :: femmesh_s
!
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
      call const_FEM_mesh_4_sph_mhd                                     &
     &   (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj,       &
     &    femmesh_s%mesh, femmesh_s%group, gen_sph)
!      call compare_mesh_type                                           &
!     &   (my_rank, geofem%mesh%nod_comm, mesh%node, mesh%ele,          &
!     &    femmesh_s%mesh)
!      call compare_mesh_groups(geofem%group%nod_grp, femmesh_s%group)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_mesh_and_group'
      femmesh_s%mesh%ele%first_ele_type                                 &
     &   = set_cube_eletype_from_num(femmesh_s%mesh%ele%nnod_4_ele)
      call copy_mesh_and_group                                          &
     &   (femmesh_s%mesh, femmesh_s%group, geofem%mesh, geofem%group)
      call dealloc_groups_data(femmesh_s%group)
      call dealloc_mesh_geometry_base(femmesh_s%mesh)
!
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      end module t_check_and_make_SPH_mesh
