!
!      module set_element_data_4_IO
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine copy_ele_connect_to_IO(ele, ele_IO)
!!        type(element_data), intent(in) :: ele
!!        type(element_data), intent(inout) :: ele_IO
!!      subroutine copy_ele_connect_from_IO(ele_IO, ele)
!!        type(element_data), intent(inout) :: ele_IO
!!        type(element_data), intent(in) :: ele
!!
!!      subroutine copy_ele_geometry_to_IO(ele, nod_IO, sfed_IO)
!!      subroutine copy_ele_sph_geom_to_IO(ele, nod_IO, sfed_IO)
!!      subroutine copy_ele_cyl_geom_to_IO(ele, nod_IO, sfed_IO)
!!        type(element_data), intent(in) :: ele
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine copy_ele_geometry_from_IO(nod_IO, sfed_IO, ele)
!!@endverbatim
!
      module set_element_data_4_IO
!
      use m_precision
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_ele_connect_to_IO(ele, ele_IO)
!
      type(element_data), intent(in) :: ele
      type(element_data), intent(inout) :: ele_IO
      integer(kind = kint) :: iele, k1
!
!
      ele_IO%numele =     ele%numele
      ele_IO%nnod_4_ele = ele%nnod_4_ele
!
      call allocate_ele_connect_type(ele_IO)
!
!$omp parallel private(k1)
      do k1 = 1, ele%nnod_4_ele
!$omp do
        do iele = 1, ele%numele
          ele_IO%ie(iele,k1) = ele%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, ele%numele
        ele_IO%iele_global(iele) = ele%iele_global(iele)
        ele_IO%elmtyp(iele) =      ele%elmtyp(iele)
        ele_IO%nodelm(iele) =      ele%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_ele_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_connect_from_IO(ele_IO, ele)
!
      use m_geometry_constants
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele_IO
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: iele, k1
!
!
      if (ele_IO%numele .eq. 0) then
        call deallocate_ele_connect_type(ele_IO)
        return
      end if
!
      ele%first_ele_type = ele_IO%elmtyp(1)
      ele%numele = ele_IO%numele
!
      call set_nnod_4_ele_by_eletype                                    &
     &   (ele%first_ele_type, ele%nnod_4_ele)
!
      call allocate_ele_connect_type(ele)
!
!$omp parallel private(k1)
      do k1 = 1, ele%nnod_4_ele
!$omp do
        do iele = 1, ele%numele
          ele%ie(iele,k1) = ele_IO%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, ele%numele
        ele%iele_global(iele) = ele_IO%iele_global(iele)
        ele%elmtyp(iele) =      ele_IO%elmtyp(iele)
        ele%nodelm(iele) =      ele_IO%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_ele_connect_from_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_ele_geometry_to_IO(ele, nod_IO, sfed_IO)
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: iele
!
!
      nod_IO%numnod =        ele%numele
      nod_IO%internal_node = ele%internal_ele
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!$omp parallel do
      do iele = 1, ele%numele
        nod_IO%inod_global(iele) = ele%iele_global(iele)
        nod_IO%xx(iele,1) = ele%x_ele(iele,1)
        nod_IO%xx(iele,2) = ele%x_ele(iele,2)
        nod_IO%xx(iele,3) = ele%x_ele(iele,3)
        sfed_IO%ele_scalar(iele) = ele%volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_sph_geom_to_IO(ele, nod_IO, sfed_IO)
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: iele
!
!
      nod_IO%numnod =        ele%numele
      nod_IO%internal_node = ele%internal_ele
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!$omp parallel do
      do iele = 1, ele%numele
        nod_IO%inod_global(iele) = ele%iele_global(iele)
!
        nod_IO%xx(iele,1) =    ele%r_ele(iele)
        nod_IO%xx(iele,2) =    ele%theta_ele(iele)
        nod_IO%xx(iele,3) =    ele%phi_ele(iele)
        sfed_IO%ele_scalar(iele) = ele%volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_sph_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_cyl_geom_to_IO(ele, nod_IO, sfed_IO)
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: iele
!
!
      nod_IO%numnod =        ele%numele
      nod_IO%internal_node = ele%internal_ele
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!$omp parallel do
      do iele = 1, ele%numele
        nod_IO%inod_global(iele) = ele%iele_global(iele)
!
        nod_IO%xx(iele,1) =    ele%s_ele(iele)
        nod_IO%xx(iele,2) =    ele%phi_ele(iele)
        nod_IO%xx(iele,3) =    ele%x_ele(iele,3)
        sfed_IO%ele_scalar(iele) = ele%volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_cyl_geom_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_ele_geometry_from_IO(nod_IO, sfed_IO, ele)
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: iele
!
!
      ele%internal_ele = nod_IO%internal_node
      call alloc_ele_geometry(ele)
!
!$omp parallel do
      do iele = 1, ele%numele
         ele%x_ele(iele,1) = nod_IO%xx(iele,1)
         ele%x_ele(iele,2) = nod_IO%xx(iele,2)
         ele%x_ele(iele,3) = nod_IO%xx(iele,3)
         ele%volume_ele(iele) =  sfed_IO%ele_scalar(iele)
      end do
!$omp end parallel do
!
      call dealloc_node_geometry_base(nod_IO)
      call dealloc_ele_scalar_IO(sfed_IO)
!
      end subroutine copy_ele_geometry_from_IO
!
!------------------------------------------------------------------
!
      end module set_element_data_4_IO
