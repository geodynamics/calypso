!>@file   set_control_4_velo.f90
!!@brief  module set_control_4_velo
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for velocity from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_velo
!!@endverbatim
!
      module set_control_4_velo
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
      subroutine s_set_control_4_velo
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
      integer (kind = kint) :: i, iflag_4_hemi
!
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        velo_nod%num_bc =    0
        torque_surf%num_bc = 0
      else
        velo_nod%num_bc =    num_bc_v_ctl
        torque_surf%num_bc = num_bc_torque_ctl
      end if
!
!  set boundary conditions for velocity
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &      write(*,*) 'velo_nod%num_bc ',velo_nod%num_bc
      if (velo_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_velo
!
        velo_nod%bc_name = bc_v_name_ctl
        velo_nod%bc_magnitude = bc_v_magnitude_ctl
!
        iflag_4_hemi = 0
        do i = 1, velo_nod%num_bc
          if ( velo_nod%bc_name(i)  .eq. 'equator') then
            iflag_4_hemi = 1
          end if
        end do
!
        do i = 1, velo_nod%num_bc
         call set_bc_group_types_vector(bc_v_type_ctl(i),               &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_sgs_vect(bc_v_type_ctl(i),             &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_rotation(bc_v_type_ctl(i),             &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_sph_center(bc_v_type_ctl(i),           &
     &       velo_nod%ibc_type(i))
!
          if (bc_v_type_ctl(i) .eq. 'vr_0' ) then
            velo_nod%ibc_type(i) = iflag_no_vr
          else if ( bc_v_type_ctl(i) .eq. 'free_slip_sph' ) then
            velo_nod%ibc_type(i) = iflag_free_sph
          else if ( bc_v_type_ctl(i) .eq. 'non_slip_sph' ) then
            velo_nod%ibc_type(i) = iflag_non_slip_sph
          else if ( bc_v_type_ctl(i) .eq. 'rot_inner_core' ) then
            velo_nod%ibc_type(i) = iflag_rotatable_icore
          else if ( bc_v_type_ctl(i) .eq. 'special' ) then
            velo_nod%ibc_type(i) = iflag_bc_special
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, velo_nod'
          do i = 1, velo_nod%num_bc
            write(*,*)  i, velo_nod%ibc_type(i),                        &
     &         velo_nod%bc_magnitude(i), trim(velo_nod%bc_name(i))
          end do
        end if
      end if
!
!
!
      if(iflag_debug .eq. iflag_full_msg)                               &
     &            write(*,*) 'torque_surf%num_bc', torque_surf%num_bc
      if(torque_surf%num_bc .gt. 0) then
!
        call allocate_velo_surf_ctl
!
        torque_surf%bc_name =      bc_torque_name_ctl
        torque_surf%bc_magnitude = bc_torque_magnitude_ctl
!
        do i = 1, torque_surf%num_bc
          call set_surf_group_types_vector(bc_torque_type_ctl(i),       &
     &       torque_surf%ibc_type(i) )
          call set_stress_free_group_types(bc_torque_type_ctl(i),       &
     &       torque_surf%ibc_type(i) )
          call set_bc_group_types_sph_center(bc_torque_type_ctl(i),     &
     &       torque_surf%ibc_type(i) )
!
          if      (bc_torque_type_ctl(i) .eq. 'free_slip_sph' ) then
            torque_surf%ibc_type(i) = iflag_free_sph
          else if (bc_torque_type_ctl(i) .eq. 'non_slip_sph' ) then
            torque_surf%ibc_type(i) = iflag_non_slip_sph
          else if (bc_torque_type_ctl(i) .eq. 'rot_inner_core' ) then
            torque_surf%ibc_type(i) = iflag_rotatable_icore
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, torque_surf'
          do i = 1, torque_surf%num_bc
            write(*,*)  i, torque_surf%ibc_type(i),                     &
     &                 torque_surf%bc_magnitude(i),                     &
     &                 trim(torque_surf%bc_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_velo
!
! -----------------------------------------------------------------------
!
      end module set_control_4_velo
