!>@file   set_control_4_temp.f90
!!@brief  module set_control_4_temp
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for temperature from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_temp
!!@endverbatim
!
      module set_control_4_temp
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
      subroutine s_set_control_4_temp
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_node_group_types
      use set_surface_group_types
!
      integer(kind = kint) :: i
!
!
      if (iflag_t_evo_4_temp .eq. id_no_evolution) then
        temp_nod%num_bc =    0
        h_flux_surf%num_bc = 0
      else
        temp_nod%num_bc =    num_bc_e_ctl
        h_flux_surf%num_bc = num_bc_h_flux_ctl
      end if
!
!   set boundary conditions for temperature
!
      if(iflag_debug .eq. iflag_full_msg)                               &
     &          write(*,*)  'temp_nod%num_bc ',temp_nod%num_bc
      if(temp_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_temp
!
        temp_nod%bc_name =      bc_e_name_ctl
        temp_nod%bc_magnitude = bc_e_magnitude_ctl
!
        do i = 1, temp_nod%num_bc
          call set_bc_group_types_scalar(bc_e_type_ctl(i),              &
     &        temp_nod%ibc_type(i))
          call set_bc_group_types_sgs_scalar(bc_e_type_ctl(i),          &
     &        temp_nod%ibc_type(i))
          call set_bc_group_types_sph_center(bc_e_type_ctl(i),          &
     &        temp_nod%ibc_type(i))
          call set_bc_group_types_fluxes(bc_e_type_ctl(i),              &
     &        temp_nod%ibc_type(i))
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, temp_nod'
          do i = 1, temp_nod%num_bc
            write(*,*)  i, temp_nod%ibc_type(i),                        &
     &         temp_nod%bc_magnitude(i), trim(temp_nod%bc_name(i))
          end do
        end if
      end if
!
!   set boundary conditions for heat flux
!
      if (h_flux_surf%num_bc .gt. 0) then
!
        call allocate_temp_surf_ctl
!
        h_flux_surf%bc_magnitude = bc_h_flux_magnitude_ctl
        h_flux_surf%bc_name =      bc_h_flux_name_ctl
!
        do i = 1, h_flux_surf%num_bc
          call set_surf_group_types_scalar(bc_h_flux_type_ctl(i),       &
     &            h_flux_surf%ibc_type(i))
          call set_bc_group_types_sph_center(bc_h_flux_type_ctl(i),     &
     &            h_flux_surf%ibc_type(i))
        end do
      end if
!
      end subroutine s_set_control_4_temp
!
! -----------------------------------------------------------------------
!
      end module set_control_4_temp
