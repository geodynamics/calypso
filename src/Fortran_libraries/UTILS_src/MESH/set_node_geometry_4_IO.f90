!
!      module set_node_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_node_geometry_to_IO
!      subroutine copy_node_geom_sph_to_IO
!      subroutine copy_node_geom_cyl_to_IO
!
!      subroutine copy_node_geometry_from_IO
!      subroutine copy_sph_geometry_from_IO
!      subroutine copy_cyl_geometry_from_IO
!
      module set_node_geometry_4_IO
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
!
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
      subroutine copy_node_geometry_to_IO
!
      integer(kind = kint) :: inod
!
!
      numnod_dummy = numnod
      internal_node_dummy = internal_node
!
      call allocate_node_data_dummy
!
!$omp parallel do
      do inod = 1, numnod
        globalnodid_dummy(inod) = globalnodid(inod)
        xx_dummy(inod,1) = xx(inod,1)
        xx_dummy(inod,2) = xx(inod,2)
        xx_dummy(inod,3) = xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_node_geom_sph_to_IO
!
      integer(kind = kint) :: inod
!
!
      numnod_dummy = numnod
      internal_node_dummy = internal_node
!
      call allocate_node_data_dummy
!
!$omp parallel do
      do inod = 1, numnod
        globalnodid_dummy(inod) = globalnodid(inod)
        xx_dummy(inod,1) = radius(inod)
        xx_dummy(inod,2) = colatitude(inod)
        xx_dummy(inod,3) = longitude(inod)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_geom_sph_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_node_geom_cyl_to_IO
!
      integer(kind = kint) :: inod
!
!
      numnod_dummy = numnod
      internal_node_dummy = internal_node
!
      call allocate_node_data_dummy
!
!$omp parallel do
      do inod = 1, numnod
        globalnodid_dummy(inod) = globalnodid(inod)
        xx_dummy(inod,1) = s_cylinder(inod)
        xx_dummy(inod,2) = longitude(inod)
        xx_dummy(inod,3) = xx(inod,3)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_geom_cyl_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_IO
!
      integer(kind = kint) :: inod
!
!
      numnod = numnod_dummy
      internal_node = internal_node_dummy
!
      call allocate_node_geometry
!
!$omp parallel do
      do inod = 1, numnod
        globalnodid(inod) = globalnodid_dummy(inod)
        xx(inod,1) = xx_dummy(inod,1)
        xx(inod,2) = xx_dummy(inod,2)
        xx(inod,3) = xx_dummy(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_data_dummy
!
      end subroutine copy_node_geometry_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_sph_geometry_from_IO
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, numnod
!        globalnodid(inod) = globalnodid_dummy(inod)
        radius(inod) =     xx_dummy(inod,1)
        colatitude(inod) = xx_dummy(inod,2)
        longitude(inod) =  xx_dummy(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_data_dummy
!
      end subroutine copy_sph_geometry_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_cyl_geometry_from_IO
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, numnod
!        globalnodid(inod) = globalnodid_dummy(inod)
        s_cylinder(inod) = xx_dummy(inod,1)
        longitude(inod) =  xx_dummy(inod,2)
        xx(inod,3) =       xx_dummy(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_data_dummy
!
      end subroutine copy_cyl_geometry_from_IO
!
!------------------------------------------------------------------
!
      end module set_node_geometry_4_IO
