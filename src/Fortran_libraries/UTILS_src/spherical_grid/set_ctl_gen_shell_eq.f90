!
!      module set_ctl_gen_shell_eq
!
      module set_ctl_gen_shell_eq
!
!        programmed by H.Matsui on July, 2007
!
      use m_precision
!
      implicit  none
!
!      subroutine s_set_control_4_gen_shell_eq
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_4_gen_shell_eq
!
      use m_constants
      use m_read_mesh_data
      use m_spheric_constants
      use m_spheric_parameter
      use m_node_id_spherical_IO
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_sphere_model
      use m_ctl_data_4_divide_sphere
      use set_control_platform_data
      use skip_comment_f
!
      integer(kind = kint) :: i, kr
!
!
      call turn_off_debug_flag_by_ctl(izero)
      call set_control_mesh_def
      call set_control_sph_mesh
!
!
      iflag_shell_mode = iflag_no_FEMMESH
      if(i_sph_g_type .gt. 0) then
        if      (cmp_no_case(sph_grid_type_ctl, 'no_pole') .gt. 0       &
     &         ) then
          iflag_shell_mode = iflag_MESH_same
        else if (cmp_no_case(sph_grid_type_ctl, 'with_pole') .gt. 0     &
     &         ) then
          iflag_shell_mode = iflag_MESH_w_pole
        else if (cmp_no_case(sph_grid_type_ctl, 'with_center') .gt. 0   &
     &         ) then
          iflag_shell_mode = iflag_MESH_w_center
        end if
      else
        iflag_shell_mode = iflag_MESH_same
      end if
!
      nidx_global_rtp(1) = 2
      nidx_global_rtp(2) = 1
      nidx_global_rtp(3) = 4
      l_truncation = -1
!
      if (radius_ctl%icou .gt. 0) then
        nidx_global_rtp(1) = radius_ctl%num
      end if
!
!      if (i_ntheta_shell .gt. 0) then
!        nidx_global_rtp(2) = ngrid_elevation_ctl
!      end if
!
      if (i_nphi_shell .gt. 0) then
        nidx_global_rtp(3) = ngrid_azimuth_ctl
        l_truncation = nidx_global_rtp(3) / 2
      end if
!
!
      if (nidx_global_rtp(1) .gt. 0) then
        call allocate_radius_1d_gl
!
        do i = 1, nidx_global_rtp(1)
         kr = radius_ctl%ivec(i)
         radius_1d_gl(kr) = radius_ctl%vect(i)
        end do
!
        call dealloc_control_array_i_r(radius_ctl)
      end if
!
!
      nlayer_2_center = -1
      nlayer_ICB =       1
      nlayer_CMB =       nidx_global_rtp(1)
      nlayer_mid_OC =   -1
      if(radial_grp_ctl%icou .gt. 0) then
        do i = 1, radial_grp_ctl%num
          if     (cmp_no_case(radial_grp_ctl%c_tbl(i),'ICB') .gt. 0     &
     &            ) then
            nlayer_ICB = radial_grp_ctl%ivec(i)
          else if(cmp_no_case(radial_grp_ctl%c_tbl(i),'CMB') .gt. 0     &
     &            ) then
            nlayer_CMB = radial_grp_ctl%ivec(i)
          else if(cmp_no_case(radial_grp_ctl%c_tbl(i),'to_center').gt.0 &
     &            ) then
            nlayer_2_center = radial_grp_ctl%ivec(i)
          else if(cmp_no_case(radial_grp_ctl%c_tbl(i),'mid_depth').gt.0 &
     &            ) then
            nlayer_mid_OC = radial_grp_ctl%ivec(i)
          end if
        end do
        call dealloc_control_array_c_i(radial_grp_ctl)
      end if
!
      write(*,*) 'nidx_global_rtp: ', nidx_global_rtp(1:3)
!
!
      end subroutine s_set_control_4_gen_shell_eq
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_shell_eq
