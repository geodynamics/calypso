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
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        num_bc_p = 0
        num_bc_wall = 0
      else
        num_bc_p = num_bc_p_ctl
        num_bc_wall = num_bc_grad_p_ctl
      end if
!
!  set boundary conditions for pressure
!
      if(iflag_debug.eq.iflag_full_msg)                                 &
     &    write(*,*) 'num_bc_p ', num_bc_p
      if(num_bc_p .gt. 0) then
!
        call allocate_nod_bc_list_press
!
        bc_p_name     = bc_p_name_ctl
        bc_p_magnitude = bc_p_magnitude_ctl
!
        do i = 1, num_bc_p
          tmpchara = bc_p_type_ctl(i)
          if ( tmpchara .eq. 'fixed' ) then
            ibc_p_type(i) =  iflag_bc_fix_s
          else if ( tmpchara .eq. 'file' ) then
            ibc_p_type(i) = -iflag_bc_fix_s
          else if ( tmpchara .eq. 'sgs' ) then
            ibc_p_type(i) =  iflag_bc_sgs_s
          end if
        end do
!
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, ibc_p_type, bc_p_magnitude, bc_p_name'
          do i = 1, num_bc_p
            write(*,*)  i, ibc_p_type(i), bc_p_magnitude(i),            &
     &                 trim(bc_p_name(i))
          end do
        end if
      end if
!
!
!
      if (num_bc_wall .gt. 0) then
!
        call allocate_press_surf_ctl
!
        bc_wall_magnitude =  bc_grad_p_magnitude_ctl
        bc_wall_name      =  bc_grad_p_name_ctl
!
        do i = 1, num_bc_wall
          tmpchara = bc_grad_p_type_ctl(i)
          call set_surf_group_types_scalar(bc_grad_p_type_ctl(i),       &
     &       ibc_wall_type(i) )
          call set_surf_wall_group_types(bc_grad_p_type_ctl(i),         &
     &       ibc_wall_type(i) )
        end do
      end if
!
      end subroutine s_set_control_4_press
!
! -----------------------------------------------------------------------
!
      end module set_control_4_press
