!>@file   t_SPH_mesh_field_data.f90
!!@brief  module t_SPH_mesh_field_data
!!
!!@author H. Matsui
!!@date Programmed on Sep., 2017
!!
!!@brief  iSpherical harmonics index and field data
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh(FEM_mesh_flags,           &
!!     &          sph_file_param, SPH_MHD, geofem, mesh_file)
!!      subroutine check_and_make_SPH_mesh(sph_file_param, SPH_MHD)
!!        type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_data), intent(inout) :: geofem
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!      subroutine check_and_make_SPH_rj_mode(sph_file_param, SPH_MHD)
!!        type(field_IO_params), intent(in) ::  sph_file_param
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!@endverbatim
!
!
      module t_SPH_mesh_field_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
      use m_spheric_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_phys_address
      use t_phys_data
!
      use t_sph_grid_maker_in_sim
      use t_file_IO_parameter
!
      implicit none
!
!
!>      Structure of spectr grid and data
      type SPH_mesh_field_data
!>         Structure of grid and spectr data for spherical spectr method
        type(sph_grids) :: sph
!>        Structure for communication table for spherical transform
        type(sph_comm_tables) :: comms
!>        Structure for grid and comm table for spherical transform
        type(sph_group_data) :: groups
!
!
!>        address for spectr data (poloidal component for vector)
        type(phys_address) :: ipol
!>        Structure for field data
        type(phys_data) :: fld
!
!>        Structure to check and construct spherical shell mesh
        type(sph_grid_maker_in_sim) :: sph_maker
      end type SPH_mesh_field_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh(FEM_mesh_flags,             &
     &          sph_file_param, SPH_MHD, geofem, mesh_file)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
      use const_FEM_mesh_sph_mhd
      use m_work_time
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
!  Check and construct spherical shell table
      call check_and_make_SPH_mesh(sph_file_param, SPH_MHD)
!
!  --  load geofem mesh data
      if(check_exist_mesh(my_rank, mesh_file)) then
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        call mpi_input_mesh(mesh_file, nprocs, geofem)
        call set_fem_center_mode_4_SPH                                  &
     &     (geofem%mesh%node%internal_node,                             &
     &      SPH_MHD%sph%sph_rtp, SPH_MHD%sph%sph_params)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
      else
!    --  Construct FEM mesh
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+3)
        mesh_file%file_prefix = sph_file_param%file_prefix
        call load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,             &
     &      SPH_MHD%groups, SPH_MHD%sph, geofem, SPH_MHD%sph_maker)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+3)
      end if
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_mesh(sph_file_param, SPH_MHD)
!
      use m_error_IDs
      use calypso_mpi_logical
      use output_gen_sph_grid_modes
      use mpi_gen_sph_grids_modes
      use sph_file_IO_select
      use check_sph_file_access
      use parallel_load_data_4_sph
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
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
        call load_sph_mesh(sph_file_param,                              &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      else if(SPH_MHD%sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
!
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
        call mpi_gen_sph_grids(SPH_MHD%sph_maker,                       &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        if(SPH_MHD%sph_maker%mesh_output_flag) then
          call output_sph_mesh(sph_file_param,                          &
     &        SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        end if
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_MHD%groups, SPH_MHD%sph, SPH_MHD%comms)
!
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_rj_mode(sph_file_param, SPH_MHD)
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
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
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
        call load_sph_rj_mesh(sph_file_param,                           &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
      else if(SPH_MHD%sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
        call mpi_gen_sph_grids(SPH_MHD%sph_maker,                       &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
!
        if(SPH_MHD%sph_maker%mesh_output_flag) then
          call output_sph_mesh(sph_file_param,                          &
     &        SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        end if
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
      end if
!
      call sph_rj_index_flags_and_params                                &
     &   (SPH_MHD%groups, SPH_MHD%sph%sph_params,                       &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%comms%comm_rj)
!
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_rj_mode
!
! ----------------------------------------------------------------------
!
      end module t_SPH_mesh_field_data
