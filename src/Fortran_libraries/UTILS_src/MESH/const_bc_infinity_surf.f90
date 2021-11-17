!>@file   const_bc_infinity_surf.f90
!!@brief  module const_bc_infinity_surf
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Set group for infinity elements
!!
!!@verbatim
!!      subroutine count_num_bc_infinity                                &
!!     &         (infty_BC, num_surf, surf_name, ngrp_sf_infty)
!!      subroutine set_bc_infty_id(infty_BC, num_surf, surf_name,       &
!!     &                           ngrp_sf_infty, id_grp_sf_infty)
!!        type(boundary_condition_list), intent(in) :: infty_BC
!!@endverbatim
!
      module const_bc_infinity_surf
!
      use m_precision
      use t_bc_data_list
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_infinity                                  &
     &         (infty_BC, num_surf, surf_name, ngrp_sf_infty)
!
      use m_boundary_condition_IDs
!
      type(boundary_condition_list), intent(in) :: infty_BC
      integer(kind=kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(inout) :: ngrp_sf_infty
!
      integer (kind = kint) :: igrp, jgrp
!
!
      ngrp_sf_infty = 0
      do igrp = 1, num_surf
!
!  ---  for infinity element
        do jgrp = 1, infty_BC%num_bc
          if (surf_name(igrp) .eq. infty_BC%bc_name(jgrp)               &
     &      .and. infty_BC%ibc_type(jgrp) .eq. iflag_surf_infty) then
              ngrp_sf_infty = ngrp_sf_infty + 1
          end if
        end do
      end do
!
      end subroutine count_num_bc_infinity
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_infty_id(infty_BC, num_surf, surf_name,         &
     &                           ngrp_sf_infty, id_grp_sf_infty)
!
      use m_boundary_condition_IDs
!
      type(boundary_condition_list), intent(in) :: infty_BC
      integer(kind=kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
      integer (kind=kint), intent(in) :: ngrp_sf_infty
!
      integer (kind=kint), intent(inout)                                &
     &      :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer (kind=kint) :: igrp, jgrp
      integer (kind=kint) :: icou
!
! ---------  boundary condition for temperature
      icou = 0
      do igrp = 1, num_surf
!
! ----------- loop for boundary conditions
        do jgrp = 1, infty_BC%num_bc
!
! ----------- check surface group
          if (surf_name(igrp) .eq. infty_BC%bc_name(jgrp)               &
     &       .and. infty_BC%ibc_type(jgrp) .eq. iflag_surf_infty) then
            icou = icou + 1
            id_grp_sf_infty(icou) = igrp
          end if
!
        end do
      end do
!
      end subroutine set_bc_infty_id
!
!-----------------------------------------------------------------------
!
      end module const_bc_infinity_surf
