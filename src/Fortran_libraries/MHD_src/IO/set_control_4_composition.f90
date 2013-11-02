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
!!     subroutine s_set_control_4_composition
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
      subroutine s_set_control_4_composition
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
      if (iflag_t_evo_4_composit .eq. id_no_evolution) then
        light_nod%num_bc =  0
        light_surf%num_bc = 0
      else
        light_nod%num_bc =  num_bc_composit_ctl
        light_surf%num_bc = num_bc_grad_ds_ctl
      end if
!
!   set boundary conditions for composition
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &   write(*,*) 'light_nod%num_bc ',light_nod%num_bc
!
      if (light_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_composit
!
        light_nod%bc_name =      bc_composit_name_ctl
        light_nod%bc_magnitude = bc_composit_magnitude_ctl
!
        do i = 1, light_nod%num_bc
          call set_bc_group_types_scalar(bc_composit_type_ctl(i),       &
     &        light_nod%ibc_type(i))
          call set_bc_group_types_sph_center(bc_composit_type_ctl(i),   &
     &        light_nod%ibc_type(i))
!
          if(bc_composit_type_ctl(i) .eq. 'fixed_flux') then
            light_nod%ibc_type(i) =  iflag_bc_fix_flux
          else if(bc_composit_type_ctl(i) .eq. 'fixed_flux_file') then
            light_nod%ibc_type(i) =  iflag_bc_file_flux
          end if
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
      end if
!
!
!   set boundary conditions for composition flux
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &       write(*,*) 'light_surf%num_bc ',light_surf%num_bc
      if (light_surf%num_bc .gt. 0) then
!
        call allocate_d_scalar_surf_ctl
!
        light_surf%bc_name      = bc_grad_ds_name_ctl
        light_surf%bc_magnitude = bc_grad_ds_magnitude_ctl
        light_surf%ibc_type = 0
!
        do i = 1, light_surf%num_bc
          call set_surf_group_types_scalar(bc_grad_ds_type_ctl(i),      &
     &        light_surf%ibc_type(i) )
          call set_bc_group_types_sph_center(bc_grad_ds_type_ctl(i),    &
     &        light_surf%ibc_type(i) )
        end do
!
        call deallocate_sf_dscalar_ctl
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*)  'i, isurf_c_type, surf_c_magnitude, surf_c_name'
          do i = 1, light_surf%num_bc
            write(*,*)  i, light_surf%ibc_type(i),                      &
     &         light_surf%bc_magnitude(i), trim(light_surf%bc_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_composition
!
! -----------------------------------------------------------------------
!
      end module set_control_4_composition
