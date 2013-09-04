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
!
      integer(kind = kint) :: i, ip
!
!
      call turn_off_debug_flag_by_ctl(izero)
      call set_control_mesh_def
      call set_control_sph_mesh
!
!
      iflag_shell_mode = iflag_no_FEMMESH
      if(i_sph_g_type .gt. 0) then
        if      (sph_grid_type_ctl .eq. 'no_pole'                       &
     &      .or. sph_grid_type_ctl .eq. 'No_pole'                       &
     &      .or. sph_grid_type_ctl .eq. 'NO_POLE') then
          iflag_shell_mode = iflag_MESH_same
        else if (sph_grid_type_ctl .eq. 'with_pole'                     &
     &      .or. sph_grid_type_ctl .eq. 'With_pole'                     &
     &      .or. sph_grid_type_ctl .eq. 'WITH_POLE') then
          iflag_shell_mode = iflag_MESH_w_pole
        else if (sph_grid_type_ctl .eq. 'with_center'                   &
     &      .or. sph_grid_type_ctl .eq. 'With_center'                   &
     &      .or. sph_grid_type_ctl .eq. 'WITH_CENTER') then
          iflag_shell_mode = iflag_MESH_w_center
        end if
      else
        iflag_shell_mode = iflag_MESH_same
      end if
!
!
      nidx_global_rtp(1) = 2
      nidx_global_rtp(2) = 1
      nidx_global_rtp(3) = 4
      l_truncation = -1
!
      if (i_numlayer_shell .gt. 0) then
        nidx_global_rtp(1) = numlayer_shell_ctl
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
!
        call allocate_radius_1d_gl
!
        do i = 1, nidx_global_rtp(1)
          radius_1d_gl(i) = radius_layer_ctl(i)
        end do
      end if
!
!
      nlayer_2_center = -1
      nlayer_ICB =       1
      nlayer_CMB =       numlayer_shell_ctl
      nlayer_mid_OC =   -1
        if(i_bc_sph .gt. 0) then
          do i = 1, numlayer_bc_ctl
            if     (bc_bondary_name_ctl(i) .eq. 'ICB'                   &
     &         .or. bc_bondary_name_ctl(i) .eq. 'icb') then
              nlayer_ICB = kr_boundary_ctl(i)
            else if(bc_bondary_name_ctl(i) .eq. 'CMB'                   &
     &         .or. bc_bondary_name_ctl(i) .eq. 'cmb') then
              nlayer_CMB = kr_boundary_ctl(i)
            else if(bc_bondary_name_ctl(i) .eq. 'to_Center'             &
     &         .or. bc_bondary_name_ctl(i) .eq. 'to_center'             &
     &         .or. bc_bondary_name_ctl(i) .eq. 'TO_CENTER') then
              nlayer_2_center = kr_boundary_ctl(i)
            else if(bc_bondary_name_ctl(i) .eq. 'Mid_Depth'             &
     &         .or. bc_bondary_name_ctl(i) .eq. 'mid_depth'             &
     &         .or. bc_bondary_name_ctl(i) .eq. 'MID_DEPTH') then
              nlayer_mid_OC = kr_boundary_ctl(i)
            end if
          end do
          call deallocate_boundary_layers
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
