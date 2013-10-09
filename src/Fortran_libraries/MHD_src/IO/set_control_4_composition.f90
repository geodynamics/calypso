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
      use set_surface_group_types
!
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_composit .eq. id_no_evolution) then
        num_surf_composition = 0
        num_bc_composit =   0
      else
        num_bc_composit = num_bc_composit_ctl
        num_surf_composition = num_bc_grad_ds_ctl
      end if
!
!   set boundary conditions for composition
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &   write(*,*) 'num_bc_composit ',num_bc_composit
!
      if (num_bc_composit .gt. 0) then
!
        call allocate_nod_bc_list_composit
!
        bc_composit_name      = bc_composit_name_ctl
        bc_composit_magnitude = bc_composit_magnitude_ctl
!
        do i = 1, num_bc_composit
          if(bc_composit_type_ctl(i) .eq. 'fixed') then
            ibc_composit_type(i) =  iflag_bc_fix_s
          else if(bc_composit_type_ctl(i) .eq. 'file') then
            ibc_composit_type(i) = -iflag_bc_fix_s
          else if(bc_composit_type_ctl(i) .eq. 'fixed_flux') then
            ibc_composit_type(i) =  iflag_bc_fix_flux
          end if
        end do
!
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*)  'i, bc_c_type, bc_c_magnitude,  bc_c_name'
          do i = 1, num_bc_composit
            write(*,*)  i, ibc_composit_type(i),                        &
     &         bc_composit_magnitude(i), trim(bc_composit_name(i))
          end do
        end if
      end if
!
!
!   set boundary conditions for composition flux
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &       write(*,*) 'num_surf_composition ',num_surf_composition
      if (num_surf_composition .gt. 0) then
!
        call allocate_d_scalar_surf_ctl
!
        surf_composit_name      = bc_grad_ds_name_ctl
        surf_composit_magnitude = bc_grad_ds_magnitude_ctl
        isurf_composit_type = 0
!
        do i = 1, num_surf_composition
          call set_surf_group_types_scalar(bc_grad_ds_type_ctl(i),      &
     &        isurf_composit_type(i) )
        end do
!
        call deallocate_sf_dscalar_ctl
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*)  'i, isurf_c_type, surf_c_magnitude, surf_c_name'
          do i = 1, num_surf_composition
            write(*,*)  i, isurf_composit_type(i),                      &
     &         surf_composit_magnitude(i), trim(surf_composit_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_composition
!
! -----------------------------------------------------------------------
!
      end module set_control_4_composition
