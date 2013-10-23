!>@file   set_node_group_types.f90
!!@brief  module set_node_group_types
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui in Sep. 2005
!
!> @brief set surface boundary condition flags from conterol input
!!
!!@verbatim
!!      subroutine set_nod_group_types_scalar(bc_type_ctl, ibc_type)
!!      subroutine set_nod_group_types_vector(bc_type_ctl, ibc_type)
!!      subroutine set_nod_group_types_sgs_scalar(bc_type_ctl, ibc_type)
!!      subroutine set_nod_group_types_sgs_vect(bc_type_ctl, ibc_type)
!!      subroutine set_nod_group_types_rotatin(bc_type_ctl, ibc_type)
!!@endverbatim
!
      module set_node_group_types
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
      subroutine set_nod_group_types_scalar(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if ( bc_type_ctl .eq. 'fixed' ) then
        ibc_type =  iflag_bc_fix_s
      else if ( bc_type_ctl .eq. 'file' ) then
        ibc_type = -iflag_bc_fix_s
      end if
!
      end subroutine set_nod_group_types_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_group_types_vector(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      ( bc_type_ctl .eq. 'fix_x' ) then
        ibc_type = iflag_bc_fixed + 1
      else if ( bc_type_ctl .eq. 'fix_y' ) then
        ibc_type = iflag_bc_fixed + 2
      else if ( bc_type_ctl .eq. 'fix_z' ) then
        ibc_type = iflag_bc_fixed + 3
      else if ( bc_type_ctl .eq. 'file_x' ) then
        ibc_type = iflag_bc_fixed - 1
      else if ( bc_type_ctl .eq. 'file_y' ) then
        ibc_type = iflag_bc_fixed - 2
      else if ( bc_type_ctl .eq. 'file_z' ) then
        ibc_type = iflag_bc_fixed - 3
      end if
!
      end subroutine set_nod_group_types_vector
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_group_types_sgs_scalar(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if( bc_type_ctl .eq. 'sgs' )  ibc_type = iflag_bc_sgs_s
!
      end subroutine set_nod_group_types_sgs_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_group_types_sgs_vect(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      ( bc_type_ctl .eq. 'sgs_x' ) then
        ibc_type = iflag_bc_sgs + 1
      else if ( bc_type_ctl .eq. 'sgs_y' ) then
        ibc_type = iflag_bc_sgs + 2
      else if ( bc_type_ctl .eq. 'sgs_z' ) then
        ibc_type = iflag_bc_sgs + 3
      end if
!
      end subroutine set_nod_group_types_sgs_vect
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_group_types_rotatin(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      ( bc_type_ctl .eq. 'rot_x' ) then
        ibc_type = iflag_bc_rot + 1
      else if ( bc_type_ctl .eq. 'rot_y' ) then
        ibc_type = iflag_bc_rot + 2
      else if ( bc_type_ctl .eq. 'rot_z' ) then
        ibc_type = iflag_bc_rot + 3
      end if
!
      end subroutine set_nod_group_types_rotatin
!
!-----------------------------------------------------------------------
!
      end module set_node_group_types
