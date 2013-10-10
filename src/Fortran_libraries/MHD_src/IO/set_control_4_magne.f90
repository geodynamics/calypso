!>@file   set_control_4_magne.f90
!!@brief  module set_control_4_magne
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for magnetic field from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_magne
!!@endverbatim
!
      module set_control_4_magne
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
      subroutine s_set_control_4_magne
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
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &       .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        num_bc_b = 0
        magne_surf%num_bc = 0
      else
        num_bc_b = num_bc_b_ctl
        magne_surf%num_bc = num_bc_grad_b_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &       write(*,*) 'num_bc_b ',num_bc_b
      if (num_bc_b .gt. 0) then
!
        call allocate_nod_bc_list_magne
!
        bc_b_name      = bc_b_name_ctl
        bc_b_magnitude = bc_b_magnitude_ctl
!
        do i = 1, num_bc_b
          tmpchara = bc_b_type_ctl(i)
          if ( tmpchara .eq. 'fix_x' ) then
            ibc_b_type(i) = iflag_bc_fixed + 1
          else if ( tmpchara .eq. 'fix_y' ) then
            ibc_b_type(i) = iflag_bc_fixed + 2
          else if ( tmpchara .eq. 'fix_z' ) then
            ibc_b_type(i) = iflag_bc_fixed + 3
          else if ( tmpchara .eq. 'file_x' ) then
            ibc_b_type(i) = iflag_bc_fixed - 1
          else if ( tmpchara .eq. 'file_y' ) then
            ibc_b_type(i) = iflag_bc_fixed - 2
          else if ( tmpchara .eq. 'file_z' ) then
            ibc_b_type(i) = iflag_bc_fixed - 3
          else if ( tmpchara .eq. 'insulator' ) then
            ibc_b_type(i) = iflag_insulator
          else if ( tmpchara .eq. 'sph_to_center' ) then
            ibc_b_type(i) = iflag_sph_2_center
          else if ( tmpchara .eq. 'pseudo_vacuum' ) then
            ibc_b_type(i) = iflag_pseudo_vacuum
!          else if ( tmpchara .eq. 'sph' ) then
!            ibc_b_type(i) = 999
          else if ( tmpchara .eq. 'sgs_x' ) then
            ibc_b_type(i) = iflag_bc_sgs + 1
          else if ( tmpchara .eq. 'sgs_y' ) then
            ibc_b_type(i) = iflag_bc_sgs + 2
          else if ( tmpchara .eq. 'sgs_z' ) then
            ibc_b_type(i) = iflag_bc_sgs + 3
          end if
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*)'i, ibc_b_type, bc_b_magnitude, bc_b_name'
          do i = 1, num_bc_b
            write(*,*) i, ibc_b_type(i), bc_b_magnitude(i),             &
     &                 trim(bc_b_name(i))
          end do
        end if
!
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &           write(*,*) 'magne_surf%num_bc ',magne_surf%num_bc
      if (magne_surf%num_bc .gt. 0) then
!
        call allocate_magne_surf_ctl
!
        magne_surf%bc_name =       bc_grad_b_name_ctl
        magne_surf%bc_magnitude =  bc_grad_b_magnitude_ctl
!
        do i = 1, magne_surf%num_bc
          call set_surf_group_types_vector(bc_grad_b_type_ctl(i),       &
     &        magne_surf%ibc_type(i))
!
          if (bc_grad_b_type_ctl(i) .eq. 'insulator' ) then
            magne_surf%ibc_type(i) = iflag_insulator
          else if (bc_grad_b_type_ctl(i) .eq. 'sph_to_center' ) then
            magne_surf%ibc_type(i) = iflag_sph_2_center
          else if (bc_grad_b_type_ctl(i) .eq. 'pseudo_vacuum' ) then
            magne_surf%ibc_type(i) = iflag_pseudo_vacuum
          end if
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i, magne_surf'
          do i = 1, magne_surf%num_bc
            write(*,*) i, magne_surf%ibc_type(i),                       &
     &         magne_surf%bc_magnitude(i), trim(magne_surf%bc_name(i))
          end do
        end if
      end if
!
!
      end subroutine s_set_control_4_magne
!
! -----------------------------------------------------------------------
!
      end module set_control_4_magne
