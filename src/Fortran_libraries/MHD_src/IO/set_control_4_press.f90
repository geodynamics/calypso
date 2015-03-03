!>@file   set_control_4_press.f90
!!@brief  module set_control_4_press
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for pressure from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_press
!!@endverbatim
!
      module set_control_4_press
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_press
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_node_group_types
      use set_surface_group_types
!
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        press_nod%num_bc = 0
        wall_surf%num_bc = 0
      else
        press_nod%num_bc = node_bc_P_ctl%num
        wall_surf%num_bc = surf_bc_PN_ctl%num
      end if
!
!  set boundary conditions for pressure
!
      if(iflag_debug.eq.iflag_full_msg)                                 &
     &    write(*,*) 'press_nod%num_bc ', press_nod%num_bc
      if(press_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_press
!
        press_nod%bc_name(1:press_nod%num_bc)                           &
     &      = node_bc_P_ctl%c2_tbl(1:press_nod%num_bc)
        press_nod%bc_magnitude(1:press_nod%num_bc)                      &
     &      = node_bc_P_ctl%vect(1:press_nod%num_bc)
!
        do i = 1, press_nod%num_bc
          call set_bc_group_types_scalar(node_bc_P_ctl%c1_tbl(i),       &
     &        press_nod%ibc_type(i))
          call set_bc_group_types_sgs_scalar(node_bc_P_ctl%c1_tbl(i),   &
     &        press_nod%ibc_type(i))
        end do
!
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, press_nod'
          do i = 1, press_nod%num_bc
            write(*,*)  i, press_nod%ibc_type(i),                       &
     &        press_nod%bc_magnitude(i), trim(press_nod%bc_name(i))
          end do
        end if
!
        call deallocate_bc_press_ctl
      end if
!
!
!
      if (wall_surf%num_bc .gt. 0) then
!
        call allocate_press_surf_ctl
!
        wall_surf%bc_magnitude(1:wall_surf%num_bc)                      &
     &        =  surf_bc_PN_ctl%vect(1:wall_surf%num_bc)
        wall_surf%bc_name(1:wall_surf%num_bc)                           &
     &        = surf_bc_PN_ctl%c2_tbl(1:wall_surf%num_bc)
!
        do i = 1, wall_surf%num_bc
          call set_surf_group_types_scalar(surf_bc_PN_ctl%c1_tbl(i),    &
     &       wall_surf%ibc_type(i) )
          call set_surf_wall_group_types(surf_bc_PN_ctl%c1_tbl(i),      &
     &       wall_surf%ibc_type(i) )
        end do
!
        call deallocate_bc_press_sf_ctl
      end if
!
      end subroutine s_set_control_4_press
!
! -----------------------------------------------------------------------
!
      end module set_control_4_press
