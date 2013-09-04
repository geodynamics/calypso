!> @file  set_size_4_smp.f90
!!      module set_size_4_smp
!!
!! @author  H. Matsui
!! @date Programmed on Sep. 2002
!
!      subroutine count_size_4_sheard_para
!      subroutine count_surf_size_4_smp
!      subroutine count_edge_size_4_smp
!
!      subroutine count_overlap_element
!      subroutine count_overlap_surface
!      subroutine count_overlap_edge
!
!> @brief set numbers for SMP parallelization
!
      module set_size_4_smp
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_size_4_sheard_para
!
      use m_geometry_parameter
      use cal_minmax_and_stacks
!
!
       call allocate_geometry_param_smp
!
!
       call count_number_4_smp( np_smp, ione, numnod,                   &
     &       inod_smp_stack, maxnod_4_smp )
!
       call count_number_4_smp( np_smp, ione, internal_node,            &
     &       inter_smp_stack, max_in_nod_4_smp )
!
       call count_number_4_smp( np_smp, ione, numele,                   &
     &       iele_smp_stack, maxele_4_smp )
!
      end subroutine count_size_4_sheard_para
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_size_4_smp
!
      use m_geometry_parameter
      use cal_minmax_and_stacks
!
!
      call allocate_surf_param_smp
!
      call count_number_4_smp( np_smp, ione, numsurf,                   &
     &       isurf_smp_stack, maxsurf_4_smp )
!
      end subroutine count_surf_size_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_edge_size_4_smp
!
      use m_geometry_parameter
      use cal_minmax_and_stacks
!
!
      call allocate_edge_param_smp
!
      call count_number_4_smp( np_smp, ione, numedge,                   &
     &       iedge_smp_stack, maxedge_4_smp )
!
      end subroutine count_edge_size_4_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_overlap_element
!
      use m_geometry_parameter
      use m_geometry_data
      use count_overlap
!
!
      call set_overlap_flag(np_smp, iele_smp_stack, internal_node,      &
     &    numele, ie(1,1), internal_ele, interior_ele)
!
      call copy_real_overlap_flag(np_smp, iele_smp_stack,               &
     &          numele, interior_ele, e_multi)
!
      end subroutine count_overlap_element
!
! ----------------------------------------------------------------------
!
      subroutine count_overlap_surface
!
      use m_geometry_parameter
      use m_geometry_data
      use count_overlap
!
      call set_overlap_flag(np_smp, isurf_smp_stack, internal_node,     &
     &    numsurf, ie_surf(1,1), internal_surf, interior_surf)
!
      end subroutine count_overlap_surface
!
! ----------------------------------------------------------------------
!
      subroutine count_overlap_edge
!
      use m_geometry_parameter
      use m_geometry_data
      use count_overlap
!
      call set_overlap_flag(np_smp, iedge_smp_stack, internal_node,     &
     &    numedge, ie_edge(1,1), internal_edge, interior_edge)
!
      end subroutine count_overlap_edge
!
! ----------------------------------------------------------------------
!
      end module set_size_4_smp
