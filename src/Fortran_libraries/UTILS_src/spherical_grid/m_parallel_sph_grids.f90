!m_parallel_sph_grids.f90
!      module m_parallel_sph_grids
!
!
!      subroutine alloc_parallel_sph_grids(ndomain_sph)
!      subroutine dealloc_parallel_sph_grids
!
!      subroutine  copy_sph_rlm_grid_from_mem(ip)
!      subroutine  copy_sph_rtm_grid_from_mem(ip)
!
!      subroutine  copy_sph_rlm_grid_to_mem(ip)
!      subroutine  copy_sph_rtm_grid_to_mem(ip)
!
      module m_parallel_sph_grids
!
      use m_precision
!
      use t_spheric_mesh
!
      implicit none
!
      integer(kind = kint) :: iflag_memory_conserve_sph = 0
      type(sph_mesh_data), allocatable :: sph_para(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_parallel_sph_grids(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
!
      allocate(sph_para(ndomain_sph))
!
      end subroutine alloc_parallel_sph_grids
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_parallel_sph_grids
!
      deallocate(sph_para)
!
      end subroutine dealloc_parallel_sph_grids
!
! -----------------------------------------------------------------------
!
      subroutine  copy_sph_rlm_grid_to_mem(ip)
!
      use m_spheric_parameter
      use copy_sph_node_4_type
      use copy_sph_comm_table_4_type
!
      integer(kind = kint), intent(in) :: ip
!
!
      call copy_sph_node_rlm_to_type(l_truncation,                      &
     &    sph_para(ip)%sph_mesh%l_truncation,                           &
     &    sph_para(ip)%sph_mesh%sph_rlm)
      call copy_comm_rlm_to_type(sph_para(ip)%sph_mesh%sph_rlm,         &
     &    sph_para(ip)%sph_comms%comm_rlm)
!
!
      end subroutine  copy_sph_rlm_grid_to_mem
!
! -----------------------------------------------------------------------
!
      subroutine  copy_sph_rtm_grid_to_mem(ip)
!
      use m_spheric_parameter
      use copy_sph_node_4_type
      use copy_sph_comm_table_4_type
!
      integer(kind = kint), intent(in) :: ip
!
!
      call copy_sph_node_rtm_to_type(l_truncation,                      &
     &    sph_para(ip)%sph_mesh%l_truncation,                           &
     &    sph_para(ip)%sph_mesh%sph_rtm)
      call copy_comm_rtm_to_type(sph_para(ip)%sph_mesh%sph_rtm,         &
     &    sph_para(ip)%sph_comms%comm_rtm)
!
      end subroutine  copy_sph_rtm_grid_to_mem
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine  copy_sph_rlm_grid_from_mem(ip)
!
      use m_spheric_parameter
      use copy_sph_node_4_type
      use copy_sph_comm_table_4_type
!
      integer(kind = kint), intent(in) :: ip
!
!
      call copy_sph_node_rlm_from_type(l_truncation,                    &
     &    sph_para(ip)%sph_mesh%l_truncation,                           &
     &    sph_para(ip)%sph_mesh%sph_rlm)
      call copy_comm_rlm_from_type(sph_para(ip)%sph_comms%comm_rlm,     &
     &    sph_para(ip)%sph_mesh%sph_rlm%nnod_rlm)
!
!
      end subroutine  copy_sph_rlm_grid_from_mem
!
! -----------------------------------------------------------------------
!
      subroutine  copy_sph_rtm_grid_from_mem(ip)
!
      use m_spheric_parameter
      use copy_sph_node_4_type
      use copy_sph_comm_table_4_type
!
      integer(kind = kint), intent(in) :: ip
!
!
      call copy_sph_node_rtm_from_type(l_truncation,                    &
     &    sph_para(ip)%sph_mesh%l_truncation,                           &
     &    sph_para(ip)%sph_mesh%sph_rtm)
      call copy_comm_rtm_from_type(sph_para(ip)%sph_comms%comm_rtm,     &
     &    sph_para(ip)%sph_mesh%sph_rtm%nnod_rtm)
!
      end subroutine  copy_sph_rtm_grid_from_mem
!
! -----------------------------------------------------------------------
!
      end module m_parallel_sph_grids
