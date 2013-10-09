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
      use set_surface_group_types
!
      integer(kind = kint) :: i
!
!
      if (iflag_t_evo_4_temp .eq. id_no_evolution) then
        num_bc_e = 0
        num_bc_h_flux = 0
      else
        num_bc_e =      num_bc_e_ctl
        num_bc_h_flux = num_bc_h_flux_ctl
      end if
!
!   set boundary conditions for temperature
!
      if(iflag_debug .eq. iflag_full_msg)                               &
     &          write(*,*)  'num_bc_e ',num_bc_e
      if(num_bc_e .gt. 0) then
!
        call allocate_nod_bc_list_temp
!
        bc_e_name      =  bc_e_name_ctl
        bc_e_magnitude = bc_e_magnitude_ctl
!
        do i = 1, num_bc_e
          if ( bc_e_type_ctl(i) .eq. 'fixed' ) then
            ibc_e_type(i) =  iflag_bc_fix_s
          else if ( bc_e_type_ctl(i) .eq. 'file' ) then
            ibc_e_type(i) = -iflag_bc_fix_s
          else if ( bc_e_type_ctl(i) .eq. 'fixed_flux' ) then
            ibc_e_type(i) =  iflag_bc_fix_flux
          else if ( bc_e_type_ctl(i) .eq. 'sgs' ) then
            ibc_e_type(i) =  iflag_bc_sgs_s
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, ibc_e_type, bc_e_magnitude, bc_e_name'
          do i = 1, num_bc_e
            write(*,*)  i, ibc_e_type(i), bc_e_magnitude(i),            &
     &                 trim(bc_e_name(i))
          end do
        end if
      end if
!
!   set boundary conditions for heat flux
!
      if (num_bc_h_flux .gt. 0) then
!
        call allocate_temp_surf_ctl
!
        bc_h_flux_magnitude = bc_h_flux_magnitude_ctl
        bc_h_flux_name     =  bc_h_flux_name_ctl
!
        do i = 1, num_bc_h_flux
          call set_surf_group_types_scalar(bc_h_flux_type_ctl(i),       &
     &            ibc_h_flux_type(i))
        end do
      end if
!
      end subroutine s_set_control_4_temp
!
! -----------------------------------------------------------------------
!
      end module set_control_4_temp
