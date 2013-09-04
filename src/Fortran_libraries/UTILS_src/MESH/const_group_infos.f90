!
!      module const_group_infos
!
!      Written by H. Matsui on July, 2006
!
!
!      subroutine const_group_informations
!
      module const_group_infos
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_group_informations
!
      use m_machine_parameter
      use set_mesh_data_4_grp
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_group'
      call set_surf_4_ele_group
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_ele_group'
      call set_edge_4_ele_group
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_ele_group'
      call set_node_4_ele_group
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_group'
      call set_surf_id_4_surf_group
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_group'
      call set_edge_4_surf_group
!
      end subroutine const_group_informations
!
! ----------------------------------------------------------------------
!
      end module const_group_infos
