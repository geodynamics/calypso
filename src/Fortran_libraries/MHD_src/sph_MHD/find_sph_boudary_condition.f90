!>@file   find_sph_boudary_condition.f90
!!        module find_sph_boudary_condition
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!>@brief control date for volume averaged spectr data
!!
!!@verbatim
!!      logical function find_fill_to_centre_bc(nod_bc_list, sf_bc_list)
!!        type(boundary_condition_list), intent(in) :: nod_bc_list
!!        type(boundary_condition_list), intent(in) :: sf_bc_list
!!      logical function find_rotatable_inner_core_bc(velocity_nod,     &
!!     &                                              torque_surf)
!!        type(boundary_condition_list), intent(in) :: velocity_nod
!!        type(boundary_condition_list), intent(in) :: torque_surf
!!
!!      logical function find_boudary_condition(iflag_target, bc_list)
!!        integer(kind = kint), intent(in) :: iflag_target
!!        type(boundary_condition_list), intent(in) :: bc_list
!!@endverbatim
      module find_sph_boudary_condition
!
      use m_precision
!
      use t_bc_data_list
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      logical function find_fill_to_centre_bc(nod_bc_list, sf_bc_list)
!
      use m_boundary_condition_IDs
!
      type(boundary_condition_list), intent(in) :: nod_bc_list
      type(boundary_condition_list), intent(in) :: sf_bc_list
!
      find_fill_to_centre_bc                                            &
     &     = find_boudary_condition(iflag_sph_2_center, nod_bc_list)
      if(find_fill_to_centre_bc) return
!
      find_fill_to_centre_bc                                            &
     &     = find_boudary_condition(iflag_sph_2_center, sf_bc_list)
!
      end function find_fill_to_centre_bc
!
! -----------------------------------------------------------------------
!
      logical function find_rotatable_inner_core_bc(velocity_nod,       &
     &                                              torque_surf)
!
      use m_boundary_condition_IDs
!
      type(boundary_condition_list), intent(in) :: velocity_nod
      type(boundary_condition_list), intent(in) :: torque_surf
!
      find_rotatable_inner_core_bc                                      &
     &    = find_boudary_condition(iflag_rotatable_icore, velocity_nod)
      if(find_rotatable_inner_core_bc) return
!
      find_rotatable_inner_core_bc                                      &
     &    = find_boudary_condition(iflag_rotatable_icore, torque_surf)
!
      end function find_rotatable_inner_core_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function find_boudary_condition(iflag_target, bc_list)
!
      integer(kind = kint), intent(in) :: iflag_target
      type(boundary_condition_list), intent(in) :: bc_list
!
      integer(kind = kint) :: i
!
      find_boudary_condition = .FALSE.
      do i = 1, bc_list%num_bc
        if(bc_list%ibc_type(i) .eq. iflag_target) then
          find_boudary_condition = .TRUE.
          return
        end if
      end do
!
      end function find_boudary_condition
!
! -----------------------------------------------------------------------
!
      end module find_sph_boudary_condition
