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
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &       .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        magne_nod%num_bc =  0
        magne_surf%num_bc = 0
      else
        magne_nod%num_bc =  node_bc_B_ctl%num
        magne_surf%num_bc = surf_bc_BN_ctl%num
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
        magne_nod%bc_name(1:magne_nod%num_bc)                           &
     &      = node_bc_B_ctl%c2_tbl(1:magne_nod%num_bc)
        magne_nod%bc_magnitude(1:magne_nod%num_bc)                      &
     &      = node_bc_B_ctl%vect(1:magne_nod%num_bc)
!
        do i = 1, magne_nod%num_bc
          call set_bc_group_types_vector(node_bc_B_ctl%c1_tbl(i),       &
     &       magne_nod%ibc_type(i))
          call set_bc_group_types_sgs_vect(node_bc_B_ctl%c1_tbl(i),     &
     &       magne_nod%ibc_type(i))
          call set_bc_group_types_sph_center(node_bc_B_ctl%c1_tbl(i),   &
     &       magne_nod%ibc_type(i))
          call set_bc_group_types_sph_magne(node_bc_B_ctl%c1_tbl(i),    &
     &       magne_nod%ibc_type(i))
!
!          if(cmp_no_case(node_bc_B_ctl%c1_tbl(i),'sph')                &
!     &       ) magne_nod%ibc_type(i) = 999
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
        call deallocate_bc_magne_ctl
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &           write(*,*) 'magne_surf%num_bc ',magne_surf%num_bc
      if (magne_surf%num_bc .gt. 0) then
!
        call allocate_magne_surf_ctl
!
        magne_surf%bc_name(1:magne_surf%num_bc)                         &
     &        = surf_bc_BN_ctl%c2_tbl(1:magne_surf%num_bc)
        magne_surf%bc_magnitude(1:magne_surf%num_bc)                    &
     &        = surf_bc_BN_ctl%vect(1:magne_surf%num_bc)
!
        do i = 1, magne_surf%num_bc
          call set_surf_group_types_vector(surf_bc_BN_ctl%c1_tbl(i),    &
     &        magne_surf%ibc_type(i))
          call set_bc_group_types_sph_center(surf_bc_BN_ctl%c1_tbl(i),  &
     &        magne_surf%ibc_type(i))
          call set_bc_group_types_sph_magne(surf_bc_BN_ctl%c1_tbl(i),   &
     &        magne_surf%ibc_type(i))
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i, magne_surf'
          do i = 1, magne_surf%num_bc
            write(*,*) i, magne_surf%ibc_type(i),                       &
     &         magne_surf%bc_magnitude(i), trim(magne_surf%bc_name(i))
          end do
        end if
!
        call deallocate_bc_magne_sf_ctl
      end if
!
!
      end subroutine s_set_control_4_magne
!
! -----------------------------------------------------------------------
!
      end module set_control_4_magne
