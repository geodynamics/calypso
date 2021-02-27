!>@file   const_FEM_mesh_sph_mhd.f90
!!@brief  module const_FEM_mesh_sph_mhd
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Construct FEM mesh from spherical harmonics transform data
!!
!!@verbatim
!!      subroutine load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,       &
!!     &                               sph_grps, sph, geofem, sph_maker)
!!        type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(sph_grids), intent(inout) :: sph
!!        type(mesh_data), intent(inout) :: geofem
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!
!!      subroutine const_FEM_mesh_4_sph_mhd                             &
!!     &        (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj,&
!!     &         mesh, group, mesh_file, gen_sph)
!!      subroutine base_FEM_mesh_sph_mhd                                &
!!     &         (FEM_mesh_flags, sph_params, sph_rtp, sph_rj,          &
!!     &          mesh, group, gen_sph)
!!        type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::  group
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module const_FEM_mesh_sph_mhd
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_mesh_data
      use t_group_data
      use t_gauss_points
      use t_sph_grid_maker_in_sim
      use t_const_spherical_grid
      use t_sph_local_parameter
      use t_sph_mesh_1d_connect
!
      implicit none
!
      type(gauss_points), save, private :: gauss_SF
      type(comm_table_make_sph), save, private :: stbl_SF
!
      private :: const_FEM_mesh_4_sph_mhd
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,         &
     &                               sph_grps, sph, geofem, sph_maker)
!
      use m_spheric_constants
      use copy_mesh_structures
      use gen_sph_grids_modes
      use mesh_IO_select
      use set_nnod_4_ele_by_type
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  mesh_file
      type(sph_group_data), intent(in) :: sph_grps
!
      type(sph_grids), intent(inout) :: sph
      type(mesh_data), intent(inout) :: geofem
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      type(mesh_data) :: femmesh_s
!
!
      call copy_sph_radial_groups(sph_grps, sph_maker%gen_sph)
!
!  --  Construct FEM mesh
      if(sph%sph_params%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph%sph_rj%iflag_rj_center .gt. 0) then
          sph%sph_params%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph%sph_params%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call const_FEM_mesh_4_sph_mhd(FEM_mesh_flags, mesh_file,          &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    femmesh_s%mesh, femmesh_s%group, sph_maker%gen_sph)
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
      call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_mhd                               &
     &        (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj,  &
     &         mesh, group, gen_sph)
!
      use m_elapsed_labels_gen_SPH
      use m_work_time
      use mpi_load_mesh_data
      use sph_file_IO_select
!      use para_const_kemoview_mesh
!      use parallel_sleeve_extension
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  mesh_file
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!      type(parallel_make_vierwer_mesh) :: par_view
!      integer(kind = kint) :: i_level
!
!
      if(iflag_GSP_time) call start_elapsed_time(ied_elapsed_GSP+9)
      call base_FEM_mesh_sph_mhd(sph_params, sph_rtp, sph_rj,           &
     &    mesh, group, gen_sph)
      if(iflag_GSP_time) call end_elapsed_time(ied_elapsed_GSP+9)
!
!! Increase sleeve size
!      if(iflag_GSP_time) call end_elapsed_time(ied_elapsed_GSP+10)
!      call sleeve_extension_loop(gen_sph%num_FEM_sleeve, mesh, group)
!      if(iflag_GSP_time) call start_elapsed_time(ied_elapsed_GSP+10)
!
! Output mesh data
      if(FEM_mesh_flags%iflag_access_FEM .gt. 0) then
        call mpi_output_mesh(mesh_file, mesh, group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
!!
!        if(FEM_mesh_flags%iflag_output_VMESH .gt. 0) then
!          call pickup_surface_mesh(mesh_file, par_view)
!        end if
      end if
!
      end subroutine const_FEM_mesh_4_sph_mhd
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine base_FEM_mesh_sph_mhd(sph_params, sph_rtp, sph_rj,     &
     &          mesh, group, gen_sph)
!
      use set_FEM_mesh_4_sph
      use const_1d_ele_connect_4_sph
      use set_nnod_4_ele_by_type
      use const_global_sph_FEM_mesh
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      call const_global_sph_FEM(sph_rtp, sph_rj, gen_sph)
!
      call const_gauss_colatitude(sph_rtp%nidx_global_rtp(2), gauss_SF)
!
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp,   &
     &    gen_sph%s3d_ranks, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &    stbl_SF)
!
!      write(*,*) 's_const_FEM_mesh_for_sph',                           &
!     &          sph_params%iflag_shell_mode, iflag_MESH_w_center
      call s_const_FEM_mesh_for_sph(my_rank, gauss_SF,                  &
     &    sph_params, sph_rtp, sph_rj, gen_sph, mesh, group, stbl_SF)
!
      call dealloc_nnod_nele_sph_mesh(stbl_SF)
      call dealloc_gauss_colatitude(gauss_SF)
!
      end subroutine base_FEM_mesh_sph_mhd
!
!-----------------------------------------------------------------------
!
      end module const_FEM_mesh_sph_mhd
