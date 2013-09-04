!set_smp_4_groups.f90
!      module set_smp_4_groups
!
!     Written by H. Matsui on Sep., 2005
!
!      subroutine count_num_groups_4_smp
!      subroutine count_surf_nod_4_sheard_para
!
      module set_smp_4_groups
!
      use m_precision
!
      implicit none
!
      private :: count_bc_4_sheard_para, count_mat_4_sheard_para
      private :: count_surf_4_sheard_para
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_groups_4_smp
!
!
      call count_bc_4_sheard_para
      call count_mat_4_sheard_para
      call count_surf_4_sheard_para
!
      end subroutine count_num_groups_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_4_sheard_para
!
      use m_machine_parameter
      use m_node_group
      use cal_minmax_and_stacks
!
!
      num_bc_smp = np_smp*num_bc
!
      call allocate_boundary_param_smp
!
      call set_group_size_4_smp(np_smp, num_bc, bc_istack,              &
     &    ibc_smp_stack, max_bc_4_smp)
!
      end subroutine count_bc_4_sheard_para
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_mat_4_sheard_para
!
      use m_machine_parameter
      use m_element_group
      use cal_minmax_and_stacks
!
!
      num_mat_smp = np_smp*num_mat
!
      call allocate_material_param_smp
!
      call set_group_size_4_smp(np_smp, num_mat, mat_istack,            &
     &    imat_smp_stack, max_mat_4_smp)
!
      end subroutine count_mat_4_sheard_para
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_4_sheard_para
!
      use m_machine_parameter
      use m_surface_group
      use cal_minmax_and_stacks
!
!
      num_surf_smp = np_smp*num_surf
!
      call allocate_surface_param_smp
!
      call set_group_size_4_smp(np_smp, num_surf, surf_istack,          &
     &    isurf_grp_smp_stack, max_sf_grp_4_smp)
!
      end subroutine count_surf_4_sheard_para
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_nod_4_sheard_para
!
      use m_machine_parameter
      use m_surface_group
      use m_surface_group_connect
      use cal_minmax_and_stacks
!
      call allocate_surf_nod_param_smp
!
      call set_group_size_4_smp(np_smp, num_surf, inod_stack_sf_grp,    &
     &    isurf_nod_smp_stack, max_sf_nod_4_smp)
!
      end subroutine count_surf_nod_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module set_smp_4_groups
