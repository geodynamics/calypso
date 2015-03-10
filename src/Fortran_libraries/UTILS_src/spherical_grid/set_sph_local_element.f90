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
      use cal_sph_node_addresses
      use cal_sph_ele_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(element_data), intent(inout) :: ele
!
!
      ele%nnod_4_ele = num_t_linear
!
      call reset_local_sph_ele_constants
      call set_nele_lc_shell(ip_r, ip_t)
      call set_nele_gl_shell
!
!    Set elements for poles
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for South pole
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          call set_nele_lc_Spole(ip_r)
        end if
        call set_nele_gl_Spole
!
!    Set elements for North pole
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          call set_nele_lc_Npole(ip_r)
        end if
        call set_nele_gl_Npole
      end if
!
!    Set elements for Center elements
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!     Mesh with center
        if     (iflag_center_r(ip_r) .gt. 0)  then
          if(iflag_Spole_t(ip_t) .gt. 0)  then
            call set_nele_lc_ctr_sph
            call set_nele_center_Spole
            call set_nele_center_Npole
          else
            call set_nele_ext_ctr_sph(ip_t)
            if(iflag_Npole_t(ip_t) .gt. 0)  then
              call set_nele_center_Npole
            end if
          end if
        end if
        call set_nele_gl_ctr_sph
      end if
!
      call cal_sph_local_numele(ele%numele)
!
      end subroutine count_local_elements_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_local_elements_sph_mesh(ip_r, ip_t, ele)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
      use cal_sph_ele_connects
      use cal_sph_ele_addresses
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
!
      end module set_sph_local_element
