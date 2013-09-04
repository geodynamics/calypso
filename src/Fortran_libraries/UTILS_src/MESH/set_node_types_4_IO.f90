!set_node_types_4_IO.f90
!      module set_node_types_4_IO
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine copy_node_type_to_IO(node)
!      subroutine copy_node_type_sph_to_IO(node)
!      subroutine copy_node_type_cyl_to_IO(node)
!
!      subroutine copy_node_type_from_IO(node)
!      subroutine copy_sph_type_from_IO(node)
!      subroutine copy_cyl_type_from_IO(node)
!        type(node_data), intent(inout) :: node
!
      module set_node_types_4_IO
!
      use m_precision
!
      use t_geometry_data
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_type_to_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      numnod_dummy =        node%numnod
      internal_node_dummy = node%internal_node
!
      call allocate_node_data_dummy
!
!$omp parallel do
      do inod = 1, node%numnod
        globalnodid_dummy(inod) = node%inod_global(inod)
        xx_dummy(inod,1) = node%xx(inod,1)
        xx_dummy(inod,2) = node%xx(inod,2)
        xx_dummy(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_type_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_node_type_sph_to_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      numnod_dummy =        node%numnod
      internal_node_dummy = node%internal_node
!
      call allocate_node_data_dummy
!
!$omp parallel do
      do inod = 1, node%numnod
        globalnodid_dummy(inod) = node%inod_global(inod)
        xx_dummy(inod,1) = node%rr(inod)
        xx_dummy(inod,2) = node%theta(inod)
        xx_dummy(inod,3) = node%phi(inod)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_type_sph_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_node_type_cyl_to_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      numnod_dummy =        node%numnod
      internal_node_dummy = node%internal_node
!
      call allocate_node_data_dummy
!
!$omp parallel do
      do inod = 1, node%numnod
        globalnodid_dummy(inod) = node%inod_global(inod)
        xx_dummy(inod,1) = node%ss(inod)
        xx_dummy(inod,2) = node%phi(inod)
        xx_dummy(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_type_cyl_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_type_from_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      node%numnod =        numnod_dummy
      node%internal_node = internal_node_dummy
!
      call allocate_node_geometry_type(node)
!
!$omp parallel do
      do inod = 1, node%numnod
        node%inod_global(inod) = globalnodid_dummy(inod)
        node%xx(inod,1) = xx_dummy(inod,1)
        node%xx(inod,2) = xx_dummy(inod,2)
        node%xx(inod,3) = xx_dummy(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_data_dummy
!
      end subroutine copy_node_type_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_sph_type_from_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
!$omp parallel do
      do inod = 1, node%numnod
!        node%inod_global(inod) = globalnodid_dummy(inod)
        node%rr(inod) =    xx_dummy(inod,1)
        node%theta(inod) = xx_dummy(inod,2)
        node%phi(inod) =   xx_dummy(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_data_dummy
!
      end subroutine copy_sph_type_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_cyl_type_from_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
!$omp parallel do
      do inod = 1, node%numnod
!        node%inod_global(inod) = globalnodid_dummy(inod)
        node%ss(inod) =   xx_dummy(inod,1)
        node%phi(inod) =  xx_dummy(inod,2)
        node%xx(inod,3) = xx_dummy(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_data_dummy
!
      end subroutine copy_cyl_type_from_IO
!
!------------------------------------------------------------------
!
      end module set_node_types_4_IO
