!>@file   set_control_4_composition.f90
!!@brief  module set_control_4_composition
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for composition variation
!!        from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_composition                          &
!!     &         (cp_prop, node_bc_C_ctl, surf_bc_CF_ctl,               &
!!     &          light_nod, light_surf)
!!        type(scalar_property), intent(in) :: cp_prop
!!        type(ctl_array_c2r), intent(inout) :: node_bc_C_ctl
!!        type(ctl_array_c2r), intent(inout) :: surf_bc_CF_ctl
!!        type(boundary_condition_list), intent(inout) :: light_nod
!!        type(boundary_condition_list), intent(inout) :: light_surf
!!@endverbatim
!
      module set_control_4_composition
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
      subroutine s_set_control_4_composition                            &
     &         (cp_prop, node_bc_C_ctl, surf_bc_CF_ctl,                 &
     &          light_nod, light_surf)
!
      use calypso_mpi
      use m_machine_parameter
      use t_physical_property
      use t_read_control_arrays
      use t_bc_data_list
      use set_node_group_types
      use set_surface_group_types
!
      type(scalar_property), intent(in) :: cp_prop
      type(ctl_array_c2r), intent(inout) :: node_bc_C_ctl
      type(ctl_array_c2r), intent(inout) :: surf_bc_CF_ctl
      type(boundary_condition_list), intent(inout) :: light_nod
      type(boundary_condition_list), intent(inout) :: light_surf
!
      integer (kind = kint) :: i
!
!
      if (cp_prop%iflag_scheme .eq. id_no_evolution) then
        light_nod%num_bc =  0
        light_surf%num_bc = 0
      else
        light_nod%num_bc =  node_bc_C_ctl%num
        light_surf%num_bc = surf_bc_CF_ctl%num
      end if
!
!   set boundary conditions for composition
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &   write(*,*) 'light_nod%num_bc ',light_nod%num_bc
!
      if (light_nod%num_bc .gt. 0) then
!
        call alloc_bc_type_ctl(light_nod)
!
        light_nod%bc_name(1:light_nod%num_bc)                           &
     &      = node_bc_C_ctl%c2_tbl(1:light_nod%num_bc)
        light_nod%bc_magnitude(1:light_nod%num_bc)                      &
     &      = node_bc_C_ctl%vect(1:light_nod%num_bc)
!
        do i = 1, light_nod%num_bc
          call set_bc_group_types_scalar(node_bc_C_ctl%c1_tbl(i),       &
     &        light_nod%ibc_type(i))
          call set_bc_group_types_sph_center(node_bc_C_ctl%c1_tbl(i),   &
     &        light_nod%ibc_type(i))
          call set_bc_group_types_fluxes(node_bc_C_ctl%c1_tbl(i),       &
     &        light_nod%ibc_type(i))
        end do
!
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*)  'i, bc_c_type, bc_c_magnitude,  bc_c_name'
          do i = 1, light_nod%num_bc
            write(*,*)  i, light_nod%ibc_type(i),                       &
     &         light_nod%bc_magnitude(i), trim(light_nod%bc_name(i))
          end do
        end if
!
        call dealloc_control_array_c2_r(node_bc_C_ctl)
      end if
!
!
!   set boundary conditions for composition flux
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &       write(*,*) 'light_surf%num_bc ',light_surf%num_bc
      if (light_surf%num_bc .gt. 0) then
!
        call alloc_bc_type_ctl(light_surf)
!
        light_surf%bc_name(1:light_surf%num_bc)                         &
     &          = surf_bc_CF_ctl%c2_tbl(1:light_surf%num_bc)
        light_surf%bc_magnitude(1:light_surf%num_bc)                    &
     &          = surf_bc_CF_ctl%vect(1:light_surf%num_bc)
        light_surf%ibc_type(1:light_surf%num_bc) = 0
!
        do i = 1, light_surf%num_bc
          call set_surf_group_types_scalar(surf_bc_CF_ctl%c1_tbl(i),    &
     &        light_surf%ibc_type(i) )
          call set_bc_group_types_sph_center(surf_bc_CF_ctl%c1_tbl(i),  &
     &        light_surf%ibc_type(i) )
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*)  'i, isurf_c_type, surf_c_magnitude, surf_c_name'
          do i = 1, light_surf%num_bc
            write(*,*)  i, light_surf%ibc_type(i),                      &
     &         light_surf%bc_magnitude(i), trim(light_surf%bc_name(i))
          end do
        end if
!
        call dealloc_control_array_c2_r(surf_bc_CF_ctl)
      end if
!
      end subroutine s_set_control_4_composition
!
! -----------------------------------------------------------------------
!
      end module set_control_4_composition
