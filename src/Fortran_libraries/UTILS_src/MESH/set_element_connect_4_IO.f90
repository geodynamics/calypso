!
!      module set_element_connect_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_element_connect_to_IO
!      subroutine copy_element_connect_from_IO
!
      module set_element_connect_4_IO
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
      subroutine copy_element_connect_to_IO
!
      integer(kind = kint) :: iele, k1
!
!
      numele_dummy = numele
      nnod_4_ele_dummy = nnod_4_ele
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp do
        do iele = 1, numele
          ie_dummy(iele,k1) = ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, numele
        globalelmid_dummy(iele) = iele_global(iele)
        i_ele_dummy(iele) =       elmtyp(iele)
        nodelm_dummy(iele) =      nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      call deallocate_element_connection
!
      end subroutine copy_element_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_element_connect_from_IO
!
      use m_geometry_constants
!
      integer(kind = kint) :: iele, k1
!
!
      if (numele_dummy .eq. 0) then
        call deallocate_ele_info_dummy
        return
      end if
!
      numele = numele_dummy
      first_ele_type = i_ele_dummy(1)
!
      if (first_ele_type .eq. 332) then
        nnod_4_ele =  num_t_quad
        nnod_4_surf = num_quad_sf
        nnod_4_edge = num_quad_edge
      else if (first_ele_type .eq. 331) then
        nnod_4_ele =  num_t_linear
        nnod_4_surf = num_linear_sf
        nnod_4_edge = num_linear_edge
      else if (first_ele_type .eq. 333) then
        nnod_4_ele =  num_t_lag
        nnod_4_surf = num_lag_sf
        nnod_4_edge = num_quad_edge
      end if
!
      call allocate_element_connection
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp do
        do iele = 1, numele
          ie(iele,k1) = ie_dummy(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, numele
        iele_global(iele) = globalelmid_dummy(iele)
        elmtyp(iele) =      i_ele_dummy(iele)
        nodelm(iele) =      nodelm_dummy(iele)
      end do
!$omp end do
!$omp end parallel
!
      call deallocate_ele_info_dummy
!
      end subroutine copy_element_connect_from_IO
!
!------------------------------------------------------------------
!
      end module set_element_connect_4_IO
