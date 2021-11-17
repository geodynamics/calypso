!const_bc_infty_surf_type.f90
!     module const_bc_infty_surf_type
!
!     written by H. Matsui on Dec., 2008
!
!
!!      subroutine const_bc_infinity_surf_grp                            &
!!     &         (infty_BC, surf_grp, infty_grp)
!!       type(boundary_condition_list), intent(in) :: infty_BC
!!       type(surface_group_data), intent(in) :: surf_grp
!!       type(scalar_surf_BC_list), intent(inout) :: infty_grp
!!      subroutine empty_infty_surf_type(group)
!!        type(mesh_groups), intent(inout) :: group
!
      module const_bc_infty_surf_type
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_bc_infinity_surf_grp                             &
     &         (infty_BC, surf_grp, infty_grp)
!
      use t_bc_data_list
      use t_group_data
      use t_surface_boundary
      use const_bc_infinity_surf
!
      type(boundary_condition_list), intent(in) :: infty_BC
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(inout) :: infty_grp
!
!
      call count_num_bc_infinity(infty_BC,                               &
     &    surf_grp%num_grp, surf_grp%grp_name, infty_grp%ngrp_sf)
!
      call alloc_scalar_surf_BC(infty_grp)
!
      call set_bc_infty_id(infty_BC, surf_grp%num_grp,                   &
     &    surf_grp%grp_name, infty_grp%ngrp_sf, infty_grp%igrp_sf)
!
      end subroutine const_bc_infinity_surf_grp
!
!-----------------------------------------------------------------------
!
      subroutine empty_infty_surf_type(infty_grp)
!
      use t_mesh_data
      use const_bc_infinity_surf
!
      type(scalar_surf_BC_list), intent(inout) :: infty_grp
!
!
      infty_grp%ngrp_sf = 0
      call alloc_scalar_surf_BC(infty_grp)
!
      end subroutine empty_infty_surf_type
!
!-----------------------------------------------------------------------
!
      end module const_bc_infty_surf_type
