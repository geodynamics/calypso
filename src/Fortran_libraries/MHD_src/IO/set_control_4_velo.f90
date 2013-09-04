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
      use m_parallel_var_dof
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_surface_group_types
!
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i, iflag_4_hemi
!
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        num_bc_v = 0
        num_bc_tq = 0
      else
        num_bc_v =  num_bc_v_ctl
        num_bc_tq = num_bc_torque_ctl
      end if
!
!  set boundary conditions for velocity
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &      write(*,*) 'num_bc_v ',num_bc_v
      if (num_bc_v .gt. 0) then
!
        call allocate_nod_bc_list_velo
!
        bc_v_name      = bc_v_name_ctl
        bc_v_magnitude = bc_v_magnitude_ctl
!
        iflag_4_hemi = 0
        do i = 1, num_bc_v
          if ( bc_v_name(i)  .eq. 'equator') then
            iflag_4_hemi = 1
          end if
        end do
!
        do i = 1, num_bc_v
          tmpchara = bc_v_type_ctl(i)
          if ( tmpchara .eq. 'fix_x' ) then
            ibc_v_type(i) = iflag_bc_fixed + 1
          else if ( tmpchara .eq. 'fix_y' ) then
            ibc_v_type(i) = iflag_bc_fixed + 2
          else if ( tmpchara .eq. 'fix_z' ) then
            ibc_v_type(i) = iflag_bc_fixed + 3
          else if ( tmpchara .eq. 'file_x' ) then
            ibc_v_type(i) = iflag_bc_fixed - 1
          else if ( tmpchara .eq. 'file_y' ) then
            ibc_v_type(i) = iflag_bc_fixed - 2
          else if ( tmpchara .eq. 'file_z' ) then
            ibc_v_type(i) = iflag_bc_fixed - 3
          else if ( tmpchara .eq. 'rot_x' ) then
            ibc_v_type(i) = iflag_bc_rot + 1
          else if ( tmpchara .eq. 'rot_y' ) then
            ibc_v_type(i) = iflag_bc_rot + 2
          else if ( tmpchara .eq. 'rot_z' ) then
            ibc_v_type(i) = iflag_bc_rot + 3
          else if ( tmpchara .eq. 'vr_0' ) then
            ibc_v_type(i) = iflag_no_vr
          else if ( tmpchara .eq. 'free_slip_sph' ) then
            ibc_v_type(i) = iflag_free_sph
          else if ( tmpchara .eq. 'non_slip_sph' ) then
            ibc_v_type(i) = iflag_non_slip_sph
          else if ( tmpchara .eq. 'rot_inner_core' ) then
            ibc_v_type(i) = iflag_rotatable_icore
          else if ( tmpchara .eq. 'special' ) then
            ibc_v_type(i) = iflag_bc_special
          else if ( tmpchara .eq. 'sgs_x' ) then
            ibc_v_type(i) =  iflag_bc_sgs + 1
          else if ( tmpchara .eq. 'sgs_y' ) then
            ibc_v_type(i) =  iflag_bc_sgs + 2
          else if ( tmpchara .eq. 'sgs_z' ) then
            ibc_v_type(i) =  iflag_bc_sgs + 3
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, ibc_v_type, bc_v_magnitude, bc_v_name'
          do i = 1, num_bc_v
            write(*,*)  i, ibc_v_type(i), bc_v_magnitude(i),            &
     &                 trim(bc_v_name(i))
          end do
        end if
      end if
!
!
!
      if(iflag_debug .eq. iflag_full_msg)                               &
     &            write(*,*) 'num_bc_tq', num_bc_tq
      if(num_bc_tq .gt. 0) then
!
        call allocate_velo_surf_ctl
!
        bc_tq_name      =  bc_torque_name_ctl
        bc_tq_magnitude =  bc_torque_magnitude_ctl
!
        do i = 1, num_bc_tq
          call set_surf_group_types_vector(bc_torque_type_ctl(i),       &
     &       ibc_tq_type(i) )
          call set_stress_free_group_types(bc_torque_type_ctl(i),       &
     &       ibc_tq_type(i) )
!
          if      (bc_torque_type_ctl(i) .eq. 'free_slip_sph' ) then
            ibc_tq_type(i) = iflag_free_sph
          else if (bc_torque_type_ctl(i) .eq. 'non_slip_sph' ) then
            ibc_tq_type(i) = iflag_non_slip_sph
          else if (bc_torque_type_ctl(i) .eq. 'rot_inner_core' ) then
            ibc_tq_type(i) = iflag_rotatable_icore
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, ibc_tq_type, bc_tq_magnitude, bc_tq_name'
          do i = 1, num_bc_tq
            write(*,*)  i, ibc_tq_type(i), bc_tq_magnitude(i),          &
     &                 trim(bc_tq_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_velo
!
! -----------------------------------------------------------------------
!
      end module set_control_4_velo
