!>@file   set_surface_group_types.f90
!!@brief  module set_surface_group_types
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui in Sep. 2005
!
!> @brief set surface boundary condition flags from conterol input
!!
!!@verbatim
!!      subroutine set_surf_group_types_scalar(bc_type_ctl, ibc_type)
!!      subroutine set_surf_group_types_vector(bc_type_ctl, ibc_type)
!!      subroutine set_stress_free_group_types(bc_type_ctl, ibc_type)
!!      subroutine set_pseudo_vacuum_group_types(bc_type_ctl, ibc_type)
!!      subroutine set_surf_wall_group_types(bc_type_ctl, ibc_type)
!!      subroutine set_surf_infty_group_types(bc_type_ctl, ibc_type)
!!@endverbatim
!
      module set_surface_group_types
!
      use m_precision
      use m_boundary_condition_IDs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_group_types_scalar(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
      if      ( bc_type_ctl .eq. 'fixed_ctl'                            &
     &    .or.  bc_type_ctl .eq. 'fixed') then
        ibc_type =  iflag_surf_fix_s
      else if ( bc_type_ctl .eq. 'fixed_dat' ) then
        ibc_type = -iflag_surf_fix_s
      else if ( bc_type_ctl .eq. 'sgs_correct' ) then
        ibc_type = iflag_bc_sgs_commute_s
!
      else if ( bc_type_ctl .eq. 'grad_ctl'                             &
     &    .or.  bc_type_ctl .eq. 'grad'                                 &
     &    .or.  bc_type_ctl .eq. 'gradient' ) then
        ibc_type =  iflag_fixed_grad_s
      else if ( bc_type_ctl .eq. 'grad_dat' ) then
        ibc_type = -iflag_fixed_grad_s
!
      else if ( bc_type_ctl .eq. 'lead_grad' ) then
        ibc_type = iflag_lead_grad_s
      end if
!
      end subroutine set_surf_group_types_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_group_types_vector(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
         if      ( bc_type_ctl .eq. 'fix_ctl_x'                         &
     &        .or. bc_type_ctl .eq. 'fix_x') then
          ibc_type = iflag_surf_fix_x
         else if ( bc_type_ctl .eq. 'fix_ctl_y'                         &
     &        .or. bc_type_ctl .eq. 'fix_y' ) then
          ibc_type = iflag_surf_fix_y
         else if ( bc_type_ctl .eq. 'fix_ctl_z'                         &
     &        .or. bc_type_ctl .eq. 'fix_z' ) then
          ibc_type = iflag_surf_fix_z
         else if ( bc_type_ctl .eq. 'fix_dat_x' ) then
          ibc_type = -iflag_surf_fix_x
         else if ( bc_type_ctl .eq. 'fix_dat_y' ) then
          ibc_type = -iflag_surf_fix_y
         else if ( bc_type_ctl .eq. 'fix_dat_z' ) then
          ibc_type = -iflag_surf_fix_z
         else if ( bc_type_ctl .eq. 'sgs_x' ) then
          ibc_type = iflag_bc_sgs_commute_x
         else if ( bc_type_ctl .eq. 'sgs_y' ) then
          ibc_type = iflag_bc_sgs_commute_y
         else if ( bc_type_ctl .eq. 'sgs_z' ) then
          ibc_type = iflag_bc_sgs_commute_z
!
         else if ( bc_type_ctl .eq. 'fix_norm_ctl' ) then
          ibc_type = iflag_fixed_norm
         else if ( bc_type_ctl .eq. 'fix_norm_dat' ) then
          ibc_type = -iflag_fixed_norm
!
         else if ( bc_type_ctl .eq. 'grad_ctl_x' ) then
          ibc_type = iflag_fixed_grad_x
         else if ( bc_type_ctl .eq. 'grad_ctl_y' ) then
          ibc_type = iflag_fixed_grad_y
         else if ( bc_type_ctl .eq. 'grad_ctl_z' ) then
          ibc_type = iflag_fixed_grad_z
         else if ( bc_type_ctl .eq. 'grad_dat_x' ) then
          ibc_type = -iflag_fixed_grad_x
         else if ( bc_type_ctl .eq. 'grad_dat_y' ) then
          ibc_type = -iflag_fixed_grad_z
         else if ( bc_type_ctl .eq. 'grad_dat_z' ) then
          ibc_type = -iflag_fixed_grad_z
!
         else if ( bc_type_ctl .eq. 'lead_grad_x' ) then
          ibc_type = iflag_lead_grad_x
         else if ( bc_type_ctl .eq. 'lead_grad_y' ) then
          ibc_type = iflag_lead_grad_y
         else if ( bc_type_ctl .eq. 'lead_grad_z' ) then
          ibc_type = iflag_lead_grad_z
         end if
!
      end subroutine set_surf_group_types_vector
!
!-----------------------------------------------------------------------
!
      subroutine set_stress_free_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
         if ( bc_type_ctl .eq. 'free_sph_in' ) then
          ibc_type = iflag_surf_free_sph_in
         else if ( bc_type_ctl .eq. 'free_sph_out' ) then
          ibc_type = iflag_surf_free_sph_out
         end if
!
      end subroutine set_stress_free_group_types
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_wall_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
         if ( bc_type_ctl .eq. 'wall' ) then
          ibc_type = iflag_surf_wall
         else if ( bc_type_ctl .eq. 'sph_in' ) then
          ibc_type = iflag_surf_sph_in
         else if ( bc_type_ctl .eq. 'sph_out' ) then
          ibc_type = iflag_surf_sph_out
         end if
!
      end subroutine set_surf_wall_group_types
!
!-----------------------------------------------------------------------
!
      subroutine set_pseudo_vacuum_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
         if ( bc_type_ctl .eq. 'pseudo_vacuum_in' ) then
          ibc_type = iflag_surf_qvc_sph_in
         else if ( bc_type_ctl .eq. 'pseudo_vacuum_out' ) then
          ibc_type = iflag_surf_qvc_sph_out
         end if
!
      end subroutine set_pseudo_vacuum_group_types
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_infty_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
      if ( bc_type_ctl .eq. 'infinity' ) then
       ibc_type = iflag_surf_infty
      end if
!
      end subroutine set_surf_infty_group_types
!
!-----------------------------------------------------------------------
!
      end module set_surface_group_types
