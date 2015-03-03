!cal_sph_ele_connects.f90
!      module cal_sph_ele_connects
!
!     Written by H. Matsui on March, 2012
!
!!      subroutine set_spherical_shell_element(ip_r, ip_t, ele)
!!      subroutine set_south_pole_element(ip_r, ip_t, ele)
!!      subroutine set_north_pole_element(ip_r, ip_t, ele)
!!      subroutine set_inter_center_shell_ele(ip_r, ip_t, ele)
!!      subroutine set_exter_center_shell_ele(ip_r, ip_t, ele)
!!      subroutine set_inter_center_s_pole_ele(ip_r, ip_t, ele)
!!      subroutine set_inter_center_n_pole_ele(ip_r, ip_t, ele)
!!      subroutine set_exter_center_n_pole_ele(ip_r, ip_t, ele)
!
      module cal_sph_ele_connects
!
      use m_precision
      use m_constants
!
      use m_gauss_points
      use t_geometry_data
!
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
      use cal_sph_ele_addresses
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_spherical_shell_element(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k, k1, k2, k_gl
      integer(kind = kint) :: l, l1, l2, l_gl
      integer(kind = kint) :: m, m1, m2
      integer(kind = kint) :: iele
!
!
      do m = 1, nidx_global_fem(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
        do l = 1, nele_sph_t(ip_t)
          l1 = ie_sph_t(l,1,ip_t)
          l2 = ie_sph_t(l,2,ip_t)
          l_gl = inod_sph_t(l1,ip_t)
          do k = 1, nele_sph_r(ip_r)
            k1 = ie_sph_r(k,1,ip_r)
            k2 = ie_sph_r(k,2,ip_r)
            k_gl = inod_sph_r(k1,ip_r)
!
            iele = sph_shell_ele_id(ip_r, ip_t, k, l, m)
            ele%iele_global(iele)                                       &
     &           = global_sph_shell_ele_id(k_gl, l_gl, m)
!
            ele%ie(iele,1) = sph_shell_node_id(ip_r, ip_t, k1, l1, m1)
            ele%ie(iele,2) = sph_shell_node_id(ip_r, ip_t, k1, l1, m2)
            ele%ie(iele,3) = sph_shell_node_id(ip_r, ip_t, k1, l2, m2)
            ele%ie(iele,4) = sph_shell_node_id(ip_r, ip_t, k1, l2, m1)
            ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
            ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
            ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l2, m2)
            ele%ie(iele,8) = sph_shell_node_id(ip_r, ip_t, k2, l2, m1)
          end do
        end do
      end do
!
      end subroutine set_spherical_shell_element
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_south_pole_element(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k, k1, k2, k_gl
      integer(kind = kint) :: l2, m, m1, m2, m3
      integer(kind = kint) :: iele
!
!
      l2 = ie_Spole_t(2,ip_t)
      do m = 1, nele_around_pole
        m1 = ie_sph_p(2*m-1,1)
        m2 = ie_sph_p(2*m,  1)
        m3 = ie_sph_p(2*m,  2)
        do k = 1, nele_sph_r(ip_r)
          k1 = ie_sph_r(k,1,ip_r)
          k2 = ie_sph_r(k,2,ip_r)
          k_gl = inod_sph_r(k1,ip_r)
!
          iele = sph_s_pole_ele_id(ip_r, k, m)
          ele%iele_global(iele) = global_sph_s_pole_ele_id(k_gl, m)
!
          ele%ie(iele,1) = sph_s_pole_node_id(k1)
          ele%ie(iele,2) = sph_shell_node_id(ip_r, ip_t, k1, l2, m3)
          ele%ie(iele,3) = sph_shell_node_id(ip_r, ip_t, k1, l2, m2)
          ele%ie(iele,4) = sph_shell_node_id(ip_r, ip_t, k1, l2, m1)
          ele%ie(iele,5) = sph_s_pole_node_id(k2)
          ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l2, m3)
          ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l2, m2)
          ele%ie(iele,8) = sph_shell_node_id(ip_r, ip_t, k2, l2, m1)
        end do
      end do
!
      end subroutine set_south_pole_element
!
! -----------------------------------------------------------------------
!
      subroutine set_north_pole_element(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k, k1, k2, k_gl
      integer(kind = kint) :: l1, m, m1, m2, m3
      integer(kind = kint) :: iele
!
!
      l1 = ie_Npole_t(1,ip_t)
      do m = 1, nele_around_pole
        m1 = ie_sph_p(2*m-1,1)
        m2 = ie_sph_p(2*m,  1)
        m3 = ie_sph_p(2*m,  2)
        do k = 1, nele_sph_r(ip_r)
          k1 = ie_sph_r(k,1,ip_r)
          k2 = ie_sph_r(k,2,ip_r)
          k_gl = inod_sph_r(k1,ip_r)
!
          iele = sph_n_pole_ele_id(ip_r, k, m)
          ele%iele_global(iele) = global_sph_n_pole_ele_id(k_gl, m)
!
          ele%ie(iele,1) = sph_shell_node_id(ip_r, ip_t, k1, l1, m1)
          ele%ie(iele,2) = sph_shell_node_id(ip_r, ip_t, k1, l1, m2)
          ele%ie(iele,3) = sph_shell_node_id(ip_r, ip_t, k1, l1, m3)
          ele%ie(iele,4) = sph_n_pole_node_id(k1)
          ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
          ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
          ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l1, m3)
          ele%ie(iele,8) = sph_n_pole_node_id(k2)
        end do
      end do
!
      end subroutine set_north_pole_element
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_inter_center_shell_ele(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, m, m1, m2
      integer(kind = kint) :: l, l1, l2
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      do m = 1, nidx_global_fem(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
        do l = 1, nidx_global_fem(2)-1
          l1 = ie_center_t(l,1)
          l2 = ie_center_t(l,2)
!
          iele = sph_inter_ctr_shell_ele_id(l, m)
          ele%iele_global(iele) = global_ctr_shell_ele_id(l, m)
!
          ele%ie(iele,1) = sph_center_node_id()
          ele%ie(iele,2) = sph_center_node_id()
          ele%ie(iele,3) = sph_center_node_id()
          ele%ie(iele,4) = sph_center_node_id()
          if(l1 .gt. 0) then
            ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
            ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
          else
            ele%ie(iele,5) = sph_ctr_shell_node_id(nnod_sph_ct,         &
     &                      (-l1), m1)
            ele%ie(iele,6) = sph_ctr_shell_node_id(nnod_sph_ct,         &
     &                      (-l1), m2)
          end if
          if(l2 .gt. 0) then
            ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l2, m2)
            ele%ie(iele,8) = sph_shell_node_id(ip_r, ip_t, k2, l2, m1)
          else
            ele%ie(iele,7) = sph_ctr_shell_node_id(nnod_sph_ct,         &
     &                      (-l2), m2)
            ele%ie(iele,8) = sph_ctr_shell_node_id(nnod_sph_ct,         &
     &                      (-l2), m1)
          end if
        end do
      end do
!
      end subroutine set_inter_center_shell_ele
!
! -----------------------------------------------------------------------
!
      subroutine set_exter_center_shell_ele(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, m, m1, m2
      integer(kind = kint) :: l, l1, l2, l_gl
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      do m = 1, nidx_global_fem(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
        do l = 1, nele_sph_t(ip_t)
          l1 = ie_sph_t(l,1,ip_t)
          l2 = ie_sph_t(l,2,ip_t)
          l_gl = inod_sph_t(l1,ip_t)
!
          iele = sph_exter_ctr_shell_ele_id(ip_t, l, m)
          ele%iele_global(iele) = global_ctr_shell_ele_id(l_gl, m)
!
          ele%ie(iele,1) = sph_center_node_id()
          ele%ie(iele,2) = sph_center_node_id()
          ele%ie(iele,3) = sph_center_node_id()
          ele%ie(iele,4) = sph_center_node_id()
          ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
          ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
          ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l2, m2)
          ele%ie(iele,8) = sph_shell_node_id(ip_r, ip_t, k2, l2, m1)
        end do
      end do
!
      end subroutine set_exter_center_shell_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_inter_center_s_pole_ele(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, l2, m, m1, m2, m3
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      l2 = ie_center_Sp(2)
      do m = 1, nele_around_pole
        m1 = ie_sph_p(2*m-1,1)
        m2 = ie_sph_p(2*m,  1)
        m3 = ie_sph_p(2*m,  2)
!
        iele = sph_inter_ctr_spole_ele_id(m)
        ele%iele_global(iele) = global_ctr_spole_ele_id(m)
!
        ele%ie(iele,1) = sph_center_node_id()
        ele%ie(iele,2) = sph_center_node_id()
        ele%ie(iele,3) = sph_center_node_id()
        ele%ie(iele,4) = sph_center_node_id()
        ele%ie(iele,5) = sph_s_pole_node_id(k2)
        ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l2, m3)
        ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l2, m2)
        ele%ie(iele,8) = sph_shell_node_id(ip_r, ip_t, k2, l2, m1)
      end do
!
      end subroutine set_inter_center_s_pole_ele
!
! -----------------------------------------------------------------------
!
      subroutine set_inter_center_n_pole_ele(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, l1, m, m1, m2, m3
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      l1 = ie_center_Np(1)
      do m = 1, nele_around_pole
        m1 = ie_sph_p(2*m-1,1)
        m2 = ie_sph_p(2*m,  1)
        m3 = ie_sph_p(2*m,  2)
        iele = sph_inter_ctr_npole_ele_id(m)
        ele%iele_global(iele) = global_ctr_npole_ele_id(m)
!
        ele%ie(iele,1) = sph_center_node_id()
        ele%ie(iele,2) = sph_center_node_id()
        ele%ie(iele,3) = sph_center_node_id()
        ele%ie(iele,4) = sph_center_node_id()
        if(l1 .gt. 0) then
          ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
          ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
          ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l1, m3)
          ele%ie(iele,8) = sph_n_pole_node_id(k2)
        else
          ele%ie(iele,5) = sph_ctr_shell_node_id(nnod_sph_ct,           &
     &                    (-l1), m1)
          ele%ie(iele,6) = sph_ctr_shell_node_id(nnod_sph_ct,           &
     &                    (-l1), m2)
          ele%ie(iele,7) = sph_ctr_shell_node_id(nnod_sph_ct,           &
     &                    (-l1), m3)
          ele%ie(iele,8) = sph_center_np_node_id()
        end if
      end do
!
      end subroutine set_inter_center_n_pole_ele
!
! -----------------------------------------------------------------------
!
      subroutine set_exter_center_n_pole_ele(ip_r, ip_t, ele)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, l1, m, m1, m2, m3
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      l1 = ie_Npole_t(1,ip_t)
      do m = 1, nele_around_pole
        m1 = ie_sph_p(2*m-1,1)
        m2 = ie_sph_p(2*m,  1)
        m3 = ie_sph_p(2*m,  2)
        iele = sph_exter_ctr_npole_ele_id(m)
        ele%iele_global(iele) = global_ctr_npole_ele_id(m)
!
        ele%ie(iele,1) = sph_center_node_id()
        ele%ie(iele,2) = sph_center_node_id()
        ele%ie(iele,3) = sph_center_node_id()
        ele%ie(iele,4) = sph_center_node_id()
        ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
        ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
        ele%ie(iele,7) = sph_shell_node_id(ip_r, ip_t, k2, l1, m3)
        ele%ie(iele,8) = sph_n_pole_node_id(k2)
      end do
!
      end subroutine set_exter_center_n_pole_ele
!
! -----------------------------------------------------------------------
!
      end module cal_sph_ele_connects
