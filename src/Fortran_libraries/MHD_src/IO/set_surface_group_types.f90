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
      use set_node_group_types
      use skip_comment_f
!
      implicit none
!
!>      control name for fixed normal gradient by control
      character(len = kchara), parameter :: grad_sf =     'grad'
!>      control name for fixed normal gradient by control
      character(len = kchara), parameter :: gradient_sf = 'gradient'
!>      control name for fixed normal gradient by control
      character(len = kchara), parameter :: grad_ctl =    'grad_ctl'
!>      control name for fixed normal gradient by external file
      character(len = kchara), parameter :: grad_file =    'grad_file'
!>      control name for fixed normal gradient by program
      character(len = kchara), parameter :: grad_lead = 'lead_grad'
!
!>      control name for fixed normal gradient by control
      character(len = kchara), parameter :: fixed_norm = 'fix_norm'
!>      control name for fixed normal gradient  by control
      character(len = kchara), parameter :: norm_ctl = 'fix_norm_ctl'
!>      control name for fixed normal gradient by external file
      character(len = kchara), parameter :: norm_file = 'fix_norm_dat'
!
!>      control name for fixed gradient of x-componenet by control
      character(len = kchara), parameter :: fixed_grad_x = 'grad_x'
!>      control name for fixed gradient of y-componenet by control
      character(len = kchara), parameter :: fixed_grad_y = 'grad_y'
!>      control name for fixed gradient of z-componenet by control
      character(len = kchara), parameter :: fixed_grad_z = 'grad_z'
!>      control name for fixed gradient of x-componenet by control
      character(len = kchara), parameter :: grad_ctl_x = 'grad_ctl_x'
!>      control name for fixed gradient of y-componenet by control
      character(len = kchara), parameter :: grad_ctl_y = 'grad_ctl_y'
!>      control name for fixed gradient of z-componenet by control
      character(len = kchara), parameter :: grad_ctl_z = 'grad_ctl_z'
!
!>      control name for fixed gradient of x-componenet by external file
      character(len = kchara), parameter :: grad_file_x = 'grad_file_x'
!>      control name for fixed gradient of y-componenet by external file
      character(len = kchara), parameter :: grad_file_y = 'grad_file_y'
!>      control name for fixed gradient of z-componenet by external file
      character(len = kchara), parameter :: grad_file_z = 'grad_file_z'
!
!>      control name for fixed gradient of x-componenet by program
      character(len = kchara), parameter :: grad_lead_x = 'lead_grad_x'
!>      control name for fixed gradient of y-componenet by program
      character(len = kchara), parameter :: grad_lead_y = 'lead_grad_y'
!>      control name for fixed gradient of z-componenet by program
      character(len = kchara), parameter :: grad_lead_z = 'lead_grad_z'
!
!>      control name for inner sphere boundary
      character(len = kchara), parameter                                &
     &      :: free_sph_in = 'free_sph_in'
!>      control name for outer sphere boundary
      character(len = kchara), parameter                                &
     &     :: free_sph_out = 'free_sph_out'
!
!>      control name for wall boundary
      character(len = kchara), parameter :: wall_surf = 'wall'
!>      control name for inner sphere boundary
      character(len = kchara), parameter :: sphere_in = 'sph_in'
!>      control name for outer sphere boundary
      character(len = kchara), parameter :: sphere_out = 'sph_out'
!
!>      control name for inner pseduo vacuum boundary
      character(len = kchara), parameter :: pseudo_in                   &
     &                        = 'pseudo_vacuum_in'
!>      control name for outer pseduo vacuum boundary
      character(len = kchara), parameter :: pseudo_out                  &
     &                        = 'pseudo_vacuum_out'
!
!>      control name for infinity surface
      character(len = kchara), parameter :: infty_surf = 'infinity'
!
      private :: grad_sf, gradient_sf, grad_file, grad_lead
      private :: fixed_grad_x, grad_ctl_x, grad_file_x, grad_lead_x
      private :: fixed_grad_y, grad_ctl_y, grad_file_y, grad_lead_y
      private :: fixed_grad_z, grad_ctl_z, grad_file_z, grad_lead_z
      private :: free_sph_in,  sphere_in,  pseudo_in,  wall_surf
      private :: free_sph_out, sphere_out, pseudo_out, infty_surf
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
!
      call set_bc_group_types_scalar(bc_type_ctl, ibc_type)
      call set_bc_group_types_sgs_scalar(bc_type_ctl, ibc_type)
!
      if      ( cmp_no_case(bc_type_ctl, grad_sf)                       &
     &    .or.  cmp_no_case(bc_type_ctl, gradient_sf)                   &
     &    .or.  cmp_no_case(bc_type_ctl, grad_ctl)   ) then
        ibc_type =  iflag_fixed_grad_s
      else if ( cmp_no_case(bc_type_ctl, grad_file)  ) then
        ibc_type = -iflag_fixed_grad_s
!
      else if ( cmp_no_case(bc_type_ctl, grad_lead)  ) then
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
!
      call set_bc_group_types_vector(bc_type_ctl, ibc_type)
      call set_bc_group_types_sgs_vect(bc_type_ctl, ibc_type)
!
      if      (cmp_no_case(bc_type_ctl, fixed_norm)                     &
     &    .or. cmp_no_case(bc_type_ctl, norm_ctl)  ) then
        ibc_type = iflag_fixed_norm
      else if (cmp_no_case(bc_type_ctl, norm_file) ) then
        ibc_type = -iflag_fixed_norm
!
      else if (cmp_no_case(bc_type_ctl, fixed_grad_x)                   &
     &    .or. cmp_no_case(bc_type_ctl, grad_ctl_x)  ) then
        ibc_type = iflag_fixed_grad_x
      else if (cmp_no_case(bc_type_ctl, fixed_grad_y)                   &
     &    .or. cmp_no_case(bc_type_ctl, grad_ctl_y)  ) then
        ibc_type = iflag_fixed_grad_y
      else if (cmp_no_case(bc_type_ctl, fixed_grad_z)                   &
     &    .or. cmp_no_case(bc_type_ctl, grad_ctl_z)  ) then
        ibc_type = iflag_fixed_grad_z
      else if (cmp_no_case(bc_type_ctl, grad_file_x) ) then
        ibc_type = -iflag_fixed_grad_x
      else if (cmp_no_case(bc_type_ctl, grad_file_y) ) then
        ibc_type = -iflag_fixed_grad_z
      else if (cmp_no_case(bc_type_ctl, grad_file_z) ) then
        ibc_type = -iflag_fixed_grad_z
!
      else if (cmp_no_case(bc_type_ctl, grad_lead_x) ) then
        ibc_type = iflag_lead_grad_x
      else if (cmp_no_case(bc_type_ctl, grad_lead_y) ) then
        ibc_type = iflag_lead_grad_y
      else if (cmp_no_case(bc_type_ctl, grad_lead_z) ) then
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
!
      if (cmp_no_case(bc_type_ctl, free_sph_in)) then
        ibc_type = iflag_surf_free_sph_in
      else if (cmp_no_case(bc_type_ctl, free_sph_out)) then
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
      if      (cmp_no_case(bc_type_ctl, wall_surf) ) then
        ibc_type = iflag_surf_wall
      else if (cmp_no_case(bc_type_ctl, sphere_in) ) then
        ibc_type = iflag_surf_sph_in
      else if (cmp_no_case(bc_type_ctl, sphere_out)) then
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
!
      if      (cmp_no_case(bc_type_ctl, pseudo_in) ) then
        ibc_type = iflag_surf_qvc_sph_in
      else if (cmp_no_case(bc_type_ctl, pseudo_out)) then
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
!
      if(cmp_no_case(bc_type_ctl, infty_surf)) then
        ibc_type = iflag_surf_infty
      end if
!
      end subroutine set_surf_infty_group_types
!
!-----------------------------------------------------------------------
!
      end module set_surface_group_types
