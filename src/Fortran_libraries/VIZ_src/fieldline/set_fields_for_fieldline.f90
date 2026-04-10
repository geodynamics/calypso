!>@file   set_fields_for_fieldline.f90
!!@brief  module set_fields_for_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine s_set_fields_for_fieldline(mesh, group, para_surf,   &
!!     &          nod_fld, num_line_local, fln_prm, fln_tce)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(inout) :: num_line_local
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module set_fields_for_fieldline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_parallel_surface_indices
      use t_control_params_4_fline
      use t_tracing_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fields_for_fieldline(mesh, group, para_surf,     &
     &          nod_fld, num_line_local, fln_prm, fln_tce)
!
      use t_mesh_data
      use t_phys_data
      use start_surface_by_gl_table
      use start_surface_by_flux
      use start_surface_by_volume
      use start_surface_4_fline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(paralell_surface_indices), intent(in) :: para_surf
!
      integer(kind = kint), intent(inout) :: num_line_local
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      if(fln_prm%id_fline_seed_type .eq. iflag_surface_group) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_flux'
        call s_start_surface_by_flux                                    &
     &     (mesh%ele, mesh%surf, group%surf_grp, nod_fld,               &
     &      fln_prm, fln_tce, num_line_local)
      else if(fln_prm%id_fline_seed_type                                &
     &                           .eq. iflag_spray_in_domain) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_volume'
        call s_start_surface_by_volume                                  &
     &     (mesh%node, mesh%ele, group%ele_grp, nod_fld,                &
     &      fln_prm, fln_tce, num_line_local)
      else if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_gl_table'
        call s_start_surface_by_gl_table                                &
     &     (mesh%ele, group%ele_grp, fln_prm, num_line_local)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 's_start_surface_4_fline'
      call s_start_surface_4_fline                                      &
     &   (mesh%node, mesh%ele, mesh%surf, nod_fld,                      &
     &    para_surf%isf_4_ele_dbl, num_line_local, fln_prm, fln_tce)
!
      end subroutine s_set_fields_for_fieldline
!
!  ---------------------------------------------------------------------
!
      end module set_fields_for_fieldline
