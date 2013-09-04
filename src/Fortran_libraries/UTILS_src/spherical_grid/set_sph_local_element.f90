!set_sph_local_element.f90
!      module set_sph_local_element
!
!     Written by H. Matsui on March, 2012
!
!      subroutine count_local_elements_sph_mesh(ip_r, ip_t, ele)
!      subroutine set_local_elements_sph_mesh(ip_r, ip_t, ele)
!        type(element_data), intent(inout) :: ele
!
      module set_sph_local_element
!
      use m_precision
      use m_constants
!
      use m_gauss_points
      use t_geometry_data
!
      implicit none
!
      integer(kind = kint) :: nele_lc_shell =   0
      integer(kind = kint) :: nele_lc_Spole =   0
      integer(kind = kint) :: nele_lc_Npole =   0
      integer(kind = kint) :: nele_lc_ctr_sph = 0
!
      integer(kind = kint) :: nele_gl_shell =   0
      integer(kind = kint) :: nele_gl_Spole =   0
      integer(kind = kint) :: nele_gl_Npole =   0
      integer(kind = kint) :: nele_gl_ctr_sph = 0
!
      private :: set_south_pole_element, set_north_pole_element
      private :: set_inter_center_s_pole_ele
      private :: set_inter_center_n_pole_ele
      private :: set_exter_center_n_pole_ele
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_local_elements_sph_mesh(ip_r, ip_t, ele)
!
      use m_geometry_constants
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
!
      ele%nnod_4_ele = num_t_linear
!
      nele_lc_shell = nele_sph_r(ip_r) * nele_sph_t(ip_t)               &
     &               * nidx_global_rtp(3)
      nele_gl_shell = (nidx_global_rtp(1)-1) * (nidx_global_rtp(2)-1)   &
     &                                       *  nidx_global_rtp(3)
      ele%numele = nele_lc_shell
!
      nele_lc_Spole =   0
      nele_lc_Npole =   0
      nele_lc_ctr_sph = 0
!
      nele_gl_Spole =   0
      nele_gl_Npole =   0
      nele_gl_ctr_sph = 0
!
!    Set elements for poles
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for north pole
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          nele_lc_Spole = nele_sph_r(ip_r) * nidx_global_rtp(3)
        end if
        nele_gl_Spole = (nidx_global_rtp(1)-1) * nidx_global_rtp(3)
!
!    Set elements for South pole
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          nele_lc_Npole = nele_sph_r(ip_r) * nidx_global_rtp(3)
        end if
        nele_gl_Npole = (nidx_global_rtp(1)-1) * nidx_global_rtp(3)
      end if
      ele%numele = nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
!    Set elements for Center elements
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!     Mesh with center
        if     (iflag_center_r(ip_r) .gt. 0)  then
          if(iflag_Spole_t(ip_t) .gt. 0)  then
            nele_lc_ctr_sph = (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
          else if(iflag_Npole_t(ip_t) .gt. 0)  then
            nele_lc_ctr_sph = (nele_sph_t(ip_t)+1) * nidx_global_rtp(3)
          else
            nele_lc_ctr_sph = nele_sph_t(ip_t) * nidx_global_rtp(3)
          end if
        end if
        nele_gl_ctr_sph = (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
      end if
      ele%numele = nele_lc_shell + nele_lc_Spole + nele_lc_Npole        &
     &            + nele_lc_ctr_sph
!
      end subroutine count_local_elements_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_local_elements_sph_mesh(ip_r, ip_t, ele)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
!
      ele%elmtyp(1:ele%numele) = 331
      ele%nodelm(1:ele%numele) = ele%nnod_4_ele
!
      call set_spherical_shell_element(ip_r, ip_t, ele)
!
!    Set elements for poles
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for south pole
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          call set_south_pole_element(ip_r, ip_t, ele)
        end if
!
!    Set elements for north pole
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          call set_north_pole_element(ip_r, ip_t, ele)
        end if
      end if
!
!    Set elements for Center elements
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        if     (iflag_center_r(ip_r) .gt. 0)  then
          if(iflag_Spole_t(ip_t) .gt. 0)  then
            call set_inter_center_shell_ele(ip_r, ip_t, ele)
!
!    Set element for south pole
            call set_inter_center_s_pole_ele(ip_r, ip_t, ele)
!
!    Set element for north pole
            call set_inter_center_n_pole_ele(ip_r, ip_t, ele)
!
          else
            call set_exter_center_shell_ele(ip_r, ip_t, ele)
!
!    Set element for north pole
            if(iflag_Npole_t(ip_t) .gt. 0)  then
              call set_exter_center_n_pole_ele(ip_r, ip_t, ele)
            end if
          end if
        end if
!
      end if
!
      end subroutine set_local_elements_sph_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_spherical_shell_element(ip_r, ip_t, ele)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
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
      do m = 1, nidx_global_rtp(3)
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k, k1, k2, k_gl
      integer(kind = kint) :: l2, m, m1, m2
      integer(kind = kint) :: iele
!
!
      l2 = ie_Spole_t(2,ip_t)
      do m = 1, nidx_global_rtp(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
        do k = 1, nele_sph_r(ip_r)
          k1 = ie_sph_r(k,1,ip_r)
          k2 = ie_sph_r(k,2,ip_r)
          k_gl = inod_sph_r(k1,ip_r)
!
          iele = sph_s_pole_ele_id(ip_r, k, m)
          ele%iele_global(iele) = global_sph_s_pole_ele_id(k_gl, m)
!
          ele%ie(iele,1) = sph_s_pole_node_id(k1)
          ele%ie(iele,2) = sph_s_pole_node_id(k1)
          ele%ie(iele,3) = sph_shell_node_id(ip_r, ip_t, k1, l2, m2)
          ele%ie(iele,4) = sph_shell_node_id(ip_r, ip_t, k1, l2, m1)
          ele%ie(iele,5) = sph_s_pole_node_id(k2)
          ele%ie(iele,6) = sph_s_pole_node_id(k2)
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k, k1, k2, k_gl
      integer(kind = kint) :: l1, m, m1, m2
      integer(kind = kint) :: iele
!
!
      l1 = ie_Npole_t(1,ip_t)
      do m = 1, nidx_global_rtp(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
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
          ele%ie(iele,3) = sph_n_pole_node_id(k1)
          ele%ie(iele,4) = sph_n_pole_node_id(k1)
          ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
          ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
          ele%ie(iele,7) = sph_n_pole_node_id(k2)
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
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
      do m = 1, nidx_global_rtp(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
        do l = 1, nidx_global_rtp(2)-1
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
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
      do m = 1, nidx_global_rtp(3)
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, l2, m, m1, m2
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      l2 = ie_center_Sp(2)
      do m = 1, nidx_global_rtp(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
!
        iele = sph_inter_ctr_spole_ele_id(m)
        ele%iele_global(iele) = global_ctr_spole_ele_id(m)
!
        ele%ie(iele,1) = sph_center_node_id()
        ele%ie(iele,2) = sph_center_node_id()
        ele%ie(iele,3) = sph_center_node_id()
        ele%ie(iele,4) = sph_center_node_id()
        ele%ie(iele,5) = sph_s_pole_node_id(k2)
        ele%ie(iele,6) = sph_s_pole_node_id(k2)
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, l1, m, m1, m2
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      l1 = ie_center_Np(1)
      do m = 1, nidx_global_rtp(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
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
          ele%ie(iele,7) = sph_n_pole_node_id(k2)
          ele%ie(iele,8) = sph_n_pole_node_id(k2)
        else
          ele%ie(iele,5) = sph_ctr_shell_node_id(nnod_sph_ct,           &
     &                    (-l1), m1)
          ele%ie(iele,6) = sph_ctr_shell_node_id(nnod_sph_ct,           &
     &                    (-l1), m2)
          ele%ie(iele,7) = sph_center_np_node_id()
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
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: k2, l1, m, m1, m2
      integer(kind = kint) :: iele
!
!
      k2 = ie_center_r(2,ip_r)
      l1 = ie_Npole_t(1,ip_t)
      do m = 1, nidx_global_rtp(3)
        m1 = ie_sph_p(m,1)
        m2 = ie_sph_p(m,2)
        iele = sph_exter_ctr_npole_ele_id(ip_t, m)
        ele%iele_global(iele) = global_ctr_npole_ele_id(m)
!
        ele%ie(iele,1) = sph_center_node_id()
        ele%ie(iele,2) = sph_center_node_id()
        ele%ie(iele,3) = sph_center_node_id()
        ele%ie(iele,4) = sph_center_node_id()
        ele%ie(iele,5) = sph_shell_node_id(ip_r, ip_t, k2, l1, m1)
        ele%ie(iele,6) = sph_shell_node_id(ip_r, ip_t, k2, l1, m2)
        ele%ie(iele,7) = sph_n_pole_node_id(k2)
        ele%ie(iele,8) = sph_n_pole_node_id(k2)
      end do
!
      end subroutine set_exter_center_n_pole_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function sph_shell_ele_id(ip_r, ip_t, kr, lt, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      sph_shell_ele_id =  kr + (lt-1) * nele_sph_r(ip_r)                &
     &                  + (mp-1) * nele_sph_r(ip_r)*nele_sph_t(ip_t)
!
      end function sph_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_s_pole_ele_id(ip_r, kr, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r
      integer(kind = kint), intent(in) :: kr, mp
!
!
      sph_s_pole_ele_id = kr + (mp-1) * nele_sph_r(ip_r)                &
     &                        + nele_lc_shell
!
      end function sph_s_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_n_pole_ele_id(ip_r, kr, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r
      integer(kind = kint), intent(in) :: kr, mp
!
!
      sph_n_pole_ele_id = kr + (mp-1) * nele_sph_r(ip_r)                &
     &                        + nele_lc_shell + nele_lc_Spole
!
      end function sph_n_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_inter_ctr_shell_ele_id(lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: lt, mp
!
!
      sph_inter_ctr_shell_ele_id = lt + (mp-1) * (nidx_global_rtp(2)-1) &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_inter_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_exter_ctr_shell_ele_id(ip_t, lt, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_t, lt, mp
!
!
      sph_exter_ctr_shell_ele_id = lt + (mp-1) * nele_sph_t(ip_t)       &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_exter_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_inter_ctr_spole_ele_id(mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: mp
!
!
      sph_inter_ctr_spole_ele_id                                        &
     &         = mp + nidx_global_rtp(3) * (nidx_global_rtp(2)-1)       &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_inter_ctr_spole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_inter_ctr_npole_ele_id(mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: mp
!
!
      sph_inter_ctr_npole_ele_id                                        &
     &         = mp + nidx_global_rtp(3) * nidx_global_rtp(2)           &
     &           + nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_inter_ctr_npole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function sph_exter_ctr_npole_ele_id(ip_t, mp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_t, mp
!
!
      sph_exter_ctr_npole_ele_id                                        &
     &         = mp + nele_sph_t(ip_t) * nidx_global_rtp(3)             &
     &           +  nele_lc_shell + nele_lc_Spole + nele_lc_Npole
!
      end function sph_exter_ctr_npole_ele_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function global_sph_shell_ele_id(kr, lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      global_sph_shell_ele_id =  kr + (lt-1) * (nidx_global_rtp(1)-1)   &
     &        + (mp-1) * (nidx_global_rtp(1)-1)*(nidx_global_rtp(2)-1)
!
      end function global_sph_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_s_pole_ele_id(kr, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: kr, mp
!
!
      global_sph_s_pole_ele_id = kr + (mp-1) * (nidx_global_rtp(1)-1)   &
     &                          + nele_gl_shell
!
      end function global_sph_s_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_n_pole_ele_id(kr, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: kr, mp
!
!
      global_sph_n_pole_ele_id = kr + (mp-1) * (nidx_global_rtp(1)-1)   &
     &                          + nele_gl_shell
!
      end function global_sph_n_pole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_ctr_shell_ele_id(lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: lt, mp
!
!
      global_ctr_shell_ele_id = lt + (mp-1) * (nidx_global_rtp(2)-1)    &
     &           + nele_gl_shell + nele_gl_Spole + nele_gl_Npole
!
      end function global_ctr_shell_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_ctr_spole_ele_id(mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: mp
!
!
      global_ctr_spole_ele_id                                           &
     &      = mp + nidx_global_rtp(3) * (nidx_global_rtp(2)-1)          &
     &           + nele_gl_shell + nele_gl_Spole + nele_gl_Npole
!
      end function global_ctr_spole_ele_id
!
! -----------------------------------------------------------------------
!
      integer function global_ctr_npole_ele_id(mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: mp
!
!
      global_ctr_npole_ele_id                                           &
     &      = mp + nidx_global_rtp(3)*nidx_global_rtp(2)                &
     &           + nele_gl_shell + nele_gl_Spole + nele_gl_Npole
!
      end function global_ctr_npole_ele_id
!
! -----------------------------------------------------------------------
!
      end module set_sph_local_element
