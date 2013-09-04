!>@file   set_ctl_gen_shell_grids.f90
!!@brief  module set_ctl_gen_shell_grids
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine s_set_control_4_gen_shell_grids
!!@endverbatim
!
      module set_ctl_gen_shell_grids
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_4_gen_shell_grids
!
      use m_constants
      use m_read_mesh_data
      use m_spheric_constants
      use m_spheric_parameter
      use m_parallel_sph_grids
      use m_node_id_spherical_IO
      use m_file_format_switch
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_sphere_model
      use m_ctl_data_4_divide_sphere
      use m_int_4_sph_coriolis_IO
!
      use const_sph_radial_grid
      use set_control_platform_data
!
      integer(kind = kint) :: nprocs_ctl
      integer(kind = kint) :: i, np, kr
!
!
      nprocs_ctl = num_subdomain_ctl
      call turn_off_debug_flag_by_ctl(izero)
      call set_control_mesh_def
      call set_control_sph_mesh
!
      if(i_coriolis_tri_int_name .gt. 0) then
        sph_cor_file_name = coriolis_int_file_name
      end if
!
      call choose_file_format(coriolis_file_fmt_ctl,                    &
     &    i_coriolis_file_fmt, ifmt_cor_int_file)
!
      if(i_mem_conserve .gt. 0) then
        if(      memory_conservation_ctl .eq. 'no'                      &
     &      .or. memory_conservation_ctl .eq. 'No'                      &
     &      .or. memory_conservation_ctl .eq. 'NO'                      &
     &      .or. memory_conservation_ctl .eq. 'off'                     &
     &      .or. memory_conservation_ctl .eq. 'Off'                     &
     &      .or. memory_conservation_ctl .eq. 'OFF') then
          iflag_memory_conserve_sph = 0
        else if( memory_conservation_ctl .eq. 'yes'                     &
     &      .or. memory_conservation_ctl .eq. 'Yes'                     &
     &      .or. memory_conservation_ctl .eq. 'YES'                     &
     &      .or. memory_conservation_ctl .eq. 'on'                      &
     &      .or. memory_conservation_ctl .eq. 'On'                      &
     &      .or. memory_conservation_ctl .eq. 'ON') then
          iflag_memory_conserve_sph = 1
        end if
      end if
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
      nidx_global_rtp(2) = 2
      nidx_global_rtp(3) = 4
      l_truncation = 2
!
      if     (raidal_grid_type_ctl .eq. 'explicit'                      &
     &   .or. raidal_grid_type_ctl .eq. 'Explicit'                      &
     &   .or. raidal_grid_type_ctl .eq. 'EXPLICIT') then
       iflag_radial_grid =  igrid_non_euqidist
      else if(raidal_grid_type_ctl .eq. 'chebyshev'                     &
     &   .or. raidal_grid_type_ctl .eq. 'Chebyshev'                     &
     &   .or. raidal_grid_type_ctl .eq. 'CHEBYSHEV') then
       iflag_radial_grid =  igrid_Chebyshev
      else if(raidal_grid_type_ctl .eq. 'equi_distance'                 &
     &   .or. raidal_grid_type_ctl .eq. 'Equi_distance'                 &
     &   .or. raidal_grid_type_ctl .eq. 'EQUI_DISTANCE') then
       iflag_radial_grid =  igrid_euqidistance
      end if
!
      if (i_sph_truncate .gt. 0) then
        l_truncation = ltr_ctl
      end if
!
      if (i_ntheta_shell .gt. 0) then
        nidx_global_rtp(2) = ngrid_elevation_ctl
      end if
!
      if (i_nphi_shell .gt. 0) then
        nidx_global_rtp(3) = ngrid_azimuth_ctl
      end if
!
!
      if(iflag_radial_grid .eq. igrid_non_euqidist) then
        if (i_numlayer_shell .gt. 0) then
          nidx_global_rtp(1) = numlayer_shell_ctl
        end if
!
        if (nidx_global_rtp(1) .gt. 0) then
          call allocate_radius_1d_gl
!
          do i = 1, nidx_global_rtp(1)
            kr = kr_layer_ctl(i)
            radius_1d_gl(kr) = radius_layer_ctl(i)
          end do
        end if
!
        nlayer_2_center = -1
        nlayer_ICB =       1
        nlayer_CMB =       nidx_global_rtp(1)
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
      else
!
        if(i_ICB_radius.gt.0 .and. i_CMB_radius.gt.0) then
          r_ICB = ICB_radius_ctl
          r_CMB = CMB_radius_ctl
        else if(i_shell_size.gt.0 .and. i_shell_ratio.gt.0) then
          r_ICB = fluid_core_size_ctl                                   &
     &           * ICB_to_CMB_ratio_ctl / (one - ICB_to_CMB_ratio_ctl)
          r_CMB = r_ICB + fluid_core_size_ctl
        else
          write(*,*)                                                    &
     &       'Set CMB and ICB radii or ratio and size of outer core'
          stop
        end if
!
        if(i_Min_radius .eq. 0) Min_radius_ctl = r_ICB
        if(i_Max_radius .eq. 0) Max_radius_ctl = r_CMB
!
        call count_set_radial_grid(num_fluid_grid_ctl,                  &
     &      Min_radius_ctl, Max_radius_ctl)
      end if
!
      ndomain_rtp(1:3) = 1
      if (ndir_domain_sph_grid .gt. 0) then
        do i = 1, ndir_domain_sph_grid
          if (         dir_domain_sph_grid_ctl(i) .eq. 'radial'         &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'Radial'         &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'RADIAL'         &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'r'              &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'R'     ) then
             ndomain_rtp(1) = num_domain_sph_grid_ctl(i)
          else if (    dir_domain_sph_grid_ctl(i) .eq. 'meridional'     &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'Meridional'     &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'MERIDiONAL'     &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'theta'          &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'Theta'          &
     &            .or. dir_domain_sph_grid_ctl(i) .eq. 'THETA'   ) then
             ndomain_rtp(2) = num_domain_sph_grid_ctl(i)
           end if
        end do
!
        call deallocate_ndomain_rtp_ctl
      end if
!
      ndomain_rtm(1:3) = 1
      if (ndir_domain_sph_grid .gt. 0) then
        do i = 1, ndir_domain_legendre
          if (         dir_domain_legendre_ctl(i) .eq. 'radial'         &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'Radial'         &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'RADIAL'         &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'r'              &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'R'     ) then
             ndomain_rtm(1) = num_domain_legendre_ctl(i)
          else if (    dir_domain_legendre_ctl(i) .eq. 'zonal'          &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'Zonal'          &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'ZONAL'          &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'phi'            &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'Phi'            &
     &            .or. dir_domain_legendre_ctl(i) .eq. 'PHI'   ) then
             ndomain_rtm(3) = num_domain_legendre_ctl(i)
           end if
        end do
!
        call deallocate_ndomain_rtm_ctl
      end if
!
      ndomain_rlm(1) = ndomain_rtm(1)
      ndomain_rlm(2) = ndomain_rtm(3)
!
      ndomain_rj(1:2) = 1
      if (ndir_domain_sph_grid .gt. 0) then
        do i = 1, ndir_domain_spectr
          if (         dir_domain_spectr_ctl(i) .eq. 'degree_order'     &
     &            .or. dir_domain_spectr_ctl(i) .eq. 'Degree_order'     &
     &            .or. dir_domain_spectr_ctl(i) .eq. 'DEGREE_ORDER'     &
     &            .or. dir_domain_spectr_ctl(i) .eq. 'modes'            &
     &            .or. dir_domain_spectr_ctl(i) .eq. 'Modes'            &
     &            .or. dir_domain_spectr_ctl(i) .eq. 'MODES' ) then
             ndomain_rj(2) = num_domain_spectr_ctl(i)
           end if
        end do
!
        call deallocate_ndomain_rj_ctl
      end if
!
!
      ndomain_sph = ndomain_rj(1)*ndomain_rj(2)
      if (ndomain_sph .ne. nprocs_ctl) then
        write(*,*) 'check num of domain spectr file(r,j)'
        stop
      end if
!
      np = ndomain_rtp(1)*ndomain_rtp(2)*ndomain_rtp(3)
      if (ndomain_sph .ne. np) then
        write(*,*) 'check num of domain for (r,t,p)'
        stop
      end if
      np = ndomain_rtm(1)*ndomain_rtm(2)*ndomain_rtm(3)
      if (ndomain_sph .ne. np) then
        write(*,*) 'check num of domain for (r,t,m)'
        stop
      end if
      np = ndomain_rlm(1)*ndomain_rlm(2)
      if (ndomain_sph .ne. np) then
        write(*,*) 'check num of domain for (r,l,m)'
        stop
      end if
!
      end subroutine s_set_control_4_gen_shell_grids
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_shell_grids
