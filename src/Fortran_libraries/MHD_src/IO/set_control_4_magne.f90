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
      use set_node_group_types
      use set_surface_group_types
!
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &       .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        magne_nod%num_bc =  0
        magne_surf%num_bc = 0
      else
        magne_nod%num_bc =  num_bc_b_ctl
        magne_surf%num_bc = num_bc_grad_b_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &       write(*,*) 'magne_nod%num_bc ',magne_nod%num_bc
      if (magne_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_magne
!
        magne_nod%bc_name =      bc_b_name_ctl
        magne_nod%bc_magnitude = bc_b_magnitude_ctl
!
        do i = 1, magne_nod%num_bc
         call set_bc_group_types_vector(bc_b_type_ctl(i),               &
     &       magne_nod%ibc_type(i))
         call set_bc_group_types_sgs_vect(bc_b_type_ctl(i),             &
     &       magne_nod%ibc_type(i))
         call set_bc_group_types_sph_center(bc_b_type_ctl(i),           &
     &       magne_nod%ibc_type(i))
!
          tmpchara = bc_b_type_ctl(i)
          if ( tmpchara .eq. 'insulator' ) then
            magne_nod%ibc_type(i) = iflag_insulator
          else if ( tmpchara .eq. 'pseudo_vacuum' ) then
            magne_nod%ibc_type(i) = iflag_pseudo_vacuum
!          else if ( tmpchara .eq. 'sph' ) then
!            magne_nod%ibc_type(i) = 999
          end if
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*)'i, magne_nod'
          do i = 1, magne_nod%num_bc
            write(*,*) i, magne_nod%ibc_type(i),                        &
     &         magne_nod%bc_magnitude(i), trim(magne_nod%bc_name(i))
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
          call set_bc_group_types_sph_center(bc_grad_b_type_ctl(i),     &
     &        magne_surf%ibc_type(i))
!
          if (bc_grad_b_type_ctl(i) .eq. 'insulator' ) then
            magne_surf%ibc_type(i) = iflag_insulator
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
