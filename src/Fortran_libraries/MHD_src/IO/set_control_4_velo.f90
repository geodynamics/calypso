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
!!      subroutine s_set_control_4_velo                                 &
!!     &         (fl_prop, node_bc_U_ctl, surf_bc_ST_ctl,               &
!!     &          velo_nod, torque_surf)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(ctl_array_c2r), intent(in) :: node_bc_U_ctl
!!        type(ctl_array_c2r), intent(in) :: surf_bc_ST_ctl
!!        type(boundary_condition_list), intent(inout) :: velo_nod
!!        type(boundary_condition_list), intent(inout) :: torque_surf
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
      subroutine s_set_control_4_velo                                   &
     &         (fl_prop, node_bc_U_ctl, surf_bc_ST_ctl,                 &
     &          velo_nod, torque_surf)
!
      use m_machine_parameter
      use calypso_mpi
      use t_physical_property
      use t_read_control_arrays
      use t_bc_data_list
      use set_node_group_types
      use set_surface_group_types
      use skip_comment_f
!
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c2r), intent(in) :: node_bc_U_ctl
      type(ctl_array_c2r), intent(in) :: surf_bc_ST_ctl
      type(boundary_condition_list), intent(inout) :: velo_nod
      type(boundary_condition_list), intent(inout) :: torque_surf
!
      integer (kind = kint) :: i, iflag_4_hemi
!
!
      if (fl_prop%iflag_scheme .eq. id_no_evolution) then
        velo_nod%num_bc =    0
        torque_surf%num_bc = 0
      else
        velo_nod%num_bc =    node_bc_U_ctl%num
        torque_surf%num_bc = surf_bc_ST_ctl%num
      end if
!
!  set boundary conditions for velocity
!
      if (iflag_debug .eq. iflag_full_msg)                              &
     &      write(*,*) 'velo_nod%num_bc ',velo_nod%num_bc
      if (velo_nod%num_bc .gt. 0) then
!
        call alloc_bc_type_ctl(velo_nod)
!
        velo_nod%bc_name(1:velo_nod%num_bc)                             &
     &      = node_bc_U_ctl%c2_tbl(1:velo_nod%num_bc)
        velo_nod%bc_magnitude(1:velo_nod%num_bc)                        &
     &      = node_bc_U_ctl%vect(1:velo_nod%num_bc)
!
        iflag_4_hemi = 0
        do i = 1, velo_nod%num_bc
          if ( velo_nod%bc_name(i)  .eq. 'equator') then
            iflag_4_hemi = 1
          end if
        end do
!
        do i = 1, velo_nod%num_bc
         call set_bc_group_types_vector(node_bc_U_ctl%c1_tbl(i),        &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_sgs_vect(node_bc_U_ctl%c1_tbl(i),      &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_rotation(node_bc_U_ctl%c1_tbl(i),      &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_sph_center(node_bc_U_ctl%c1_tbl(i),    &
     &       velo_nod%ibc_type(i))
         call set_bc_group_types_sph_velo(node_bc_U_ctl%c1_tbl(i),      &
     &       velo_nod%ibc_type(i))
!
          if(cmp_no_case(node_bc_U_ctl%c1_tbl(i), 'vr_0')               &
     &       ) velo_nod%ibc_type(i) = iflag_no_vr
          if(cmp_no_case(node_bc_U_ctl%c1_tbl(i), 'special')            &
     &       ) velo_nod%ibc_type(i) = iflag_bc_special
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
        call alloc_bc_type_ctl(torque_surf)
!
        torque_surf%bc_name(1:torque_surf%num_bc)                       &
     &       = surf_bc_ST_ctl%c2_tbl(1:torque_surf%num_bc)
        torque_surf%bc_magnitude(1:torque_surf%num_bc)                  &
     &       = surf_bc_ST_ctl%vect(1:torque_surf%num_bc)
!
        do i = 1, torque_surf%num_bc
          call set_surf_group_types_vector(surf_bc_ST_ctl%c1_tbl(i),    &
     &       torque_surf%ibc_type(i) )
          call set_stress_free_group_types(surf_bc_ST_ctl%c1_tbl(i),    &
     &       torque_surf%ibc_type(i) )
         call set_bc_group_types_sph_velo(surf_bc_ST_ctl%c1_tbl(i),     &
     &       torque_surf%ibc_type(i))
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
