!
!      module set_element_data_4_IO
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine copy_ele_connect_to_IO(ele)
!      subroutine copy_ele_connect_from_IO(ele)
!
!      subroutine copy_ele_geometry_to_IO(ele)
!      subroutine copy_ele_sph_geom_to_IO(ele)
!      subroutine copy_ele_cyl_geom_to_IO(ele)
!
      module set_element_data_4_IO
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
      subroutine copy_ele_connect_to_IO(ele)
!
      type(element_data), intent(inout) :: ele
      integer(kind = kint) :: iele, k1
!
!
      numele_dummy =     ele%numele
      nnod_4_ele_dummy = ele%nnod_4_ele
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
!$omp parallel private(k1)
      do k1 = 1, ele%nnod_4_ele
!$omp do
        do iele = 1, ele%numele
          ie_dummy(iele,k1) = ele%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, ele%numele
        globalelmid_dummy(iele) = ele%iele_global(iele)
        i_ele_dummy(iele) =       ele%elmtyp(iele)
        nodelm_dummy(iele) =      ele%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_ele_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_connect_from_IO(ele)
!
      use m_geometry_constants
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele
      integer(kind = kint) :: iele, k1
!
!
      if (numele_dummy .eq. 0) then
        call deallocate_ele_info_dummy
        return
      end if
!
      ele%numele = numele_dummy
      ele%first_ele_type = i_ele_dummy(1)
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
          ele%ie(iele,k1) = ie_dummy(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, ele%numele
        ele%iele_global(iele) = globalelmid_dummy(iele)
        ele%elmtyp(iele) =      i_ele_dummy(iele)
        ele%nodelm(iele) =      nodelm_dummy(iele)
      end do
!$omp end do
!$omp end parallel
!
      call deallocate_ele_info_dummy
!
      end subroutine copy_ele_connect_from_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_ele_geometry_to_IO(ele)
!
      type(element_data), intent(inout) :: ele
      integer(kind = kint) :: iele
!
!
      numnod_dummy =        ele%numele
      internal_node_dummy = ele%internal_ele
!
      call allocate_node_data_dummy
      call allocate_ele_scalar_IO
!
!$omp parallel do
      do iele = 1, ele%numele
        globalnodid_dummy(iele) = ele%iele_global(iele)
        xx_dummy(iele,1) = ele%x_ele(iele,1)
        xx_dummy(iele,2) = ele%x_ele(iele,2)
        xx_dummy(iele,3) = ele%x_ele(iele,3)
        ele_scalar_IO(iele) = ele%volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_sph_geom_to_IO(ele)
!
      type(element_data), intent(inout) :: ele
      integer(kind = kint) :: iele
!
!
      numnod_dummy =        ele%numele
      internal_node_dummy = ele%internal_ele
!
      call allocate_node_data_dummy
      call allocate_ele_scalar_IO
!
!$omp parallel do
      do iele = 1, ele%numele
        globalnodid_dummy(iele) = ele%iele_global(iele)
!
        xx_dummy(iele,1) =    ele%r_ele(iele)
        xx_dummy(iele,2) =    ele%theta_ele(iele)
        xx_dummy(iele,3) =    ele%phi_ele(iele)
        ele_scalar_IO(iele) = ele%volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_sph_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_cyl_geom_to_IO(ele)
!
      type(element_data), intent(inout) :: ele
      integer(kind = kint) :: iele
!
!
      numnod_dummy =        ele%numele
      internal_node_dummy = ele%internal_ele
!
      call allocate_node_data_dummy
      call allocate_ele_scalar_IO
!
!$omp parallel do
      do iele = 1, ele%numele
        globalnodid_dummy(iele) = ele%iele_global(iele)
!
        xx_dummy(iele,1) =    ele%s_ele(iele)
        xx_dummy(iele,2) =    ele%phi_ele(iele)
        xx_dummy(iele,3) =    ele%x_ele(iele,3)
        ele_scalar_IO(iele) = ele%volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_cyl_geom_to_IO
!
!------------------------------------------------------------------
!
      end module set_element_data_4_IO
