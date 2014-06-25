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
      use m_machine_parameter
      use m_read_mesh_data
      use m_spheric_constants
      use m_spheric_parameter
      use m_parallel_sph_grids
      use m_sph_1d_global_index
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
      use gen_sph_grids_modes
      use skip_comment_f
!
      integer(kind = kint) :: nprocs_ctl
      integer(kind = kint) :: i, np, kr, icou
      integer(kind = kint) :: iflag_no, iflag_yes
      integer(kind = kint) :: iflag_r, iflag_t, iflag_p
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
      iflag_memory_conserve_sph = 0
      if(i_mem_conserve .gt. 0) then
        iflag_no =  cmp_no_case(memory_conservation_ctl, 'no')          &
     &         + cmp_no_case(memory_conservation_ctl, 'off')
        iflag_yes = cmp_no_case(memory_conservation_ctl, 'yes')         &
     &         + cmp_no_case(memory_conservation_ctl, 'on')
        if(iflag_no .gt. 0) then
          iflag_memory_conserve_sph = 0
        else if(iflag_yes .gt. 0) then
          iflag_memory_conserve_sph = 1
        end if
      end if
!
      iflag_excluding_FEM_mesh = 0
      if(i_mem_conserve .gt. 0) then
        iflag_no =  cmp_no_case(excluding_FEM_mesh_ctl, 'no')           &
     &         + cmp_no_case(excluding_FEM_mesh_ctl, 'off')
        iflag_yes = cmp_no_case(excluding_FEM_mesh_ctl, 'yes')          &
     &         + cmp_no_case(excluding_FEM_mesh_ctl, 'on')
        if(iflag_no .gt. 0) then
          iflag_excluding_FEM_mesh = 0
        else if(iflag_yes .gt. 0) then
          iflag_excluding_FEM_mesh = 1
        end if
      end if
!
      iflag_shell_mode = iflag_no_FEMMESH
      if(i_sph_g_type .gt. 0) then
        if      (cmp_no_case(sph_grid_type_ctl, 'no_pole') .gt. 0) then
          iflag_shell_mode = iflag_MESH_same
        else if(cmp_no_case(sph_grid_type_ctl, 'with_pole')             &
     &     .gt. 0) then
          iflag_shell_mode = iflag_MESH_w_pole
        else if(cmp_no_case(sph_grid_type_ctl, 'with_center')           &
     &     .gt. 0) then
          iflag_shell_mode = iflag_MESH_w_center
        end if
      else
        iflag_shell_mode = iflag_MESH_same
      end if
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_shell_mode', iflag_shell_mode
      end if
!
!
      nidx_global_rtp(1) = 2
      nidx_global_rtp(2) = 2
      nidx_global_rtp(3) = 4
      l_truncation = 2
!
      if     (cmp_no_case(radial_grid_type_ctl, 'explicit')             &
     &     .gt. 0) then
       iflag_radial_grid =  igrid_non_euqidist
      else if(cmp_no_case(radial_grid_type_ctl, 'Chebyshev')            &
     &     .gt. 0) then
       iflag_radial_grid =  igrid_Chebyshev
      else if(cmp_no_case(radial_grid_type_ctl, 'equi_distance')        &
     &     .gt. 0) then
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
!   Set radial group
      if(radial_grp_ctl%icou .gt. 0) then
        numlayer_sph_bc = radial_grp_ctl%num
      else
        numlayer_sph_bc = 0
      end if
      call allocate_sph_radial_group
!
      icou = 0
      do i = 1, numlayer_sph_bc
        if     (cmp_no_case(radial_grp_ctl%c_tbl(i), 'ICB')             &
     &          .gt. 0) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'CMB')             &
     &          .gt. 0) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'to_Center')       &
     &          .gt. 0) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'Mid_Depth')       &
     &          .gt. 0) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else
          icou = icou + 1
          kr_sph_boundary(icou) =  radial_grp_ctl%ivec(i)
          sph_bondary_name(icou) = radial_grp_ctl%c_tbl(i)
        end if
      end do
!
!   Set radial grid explicitly
      iflag_rj_center = 0
      if(iflag_radial_grid .eq. igrid_non_euqidist) then
        if(i_sph_c_type .gt. 0) then
          if(cmp_no_case(sph_coef_type_ctl, 'with_center').gt.0) then
             iflag_rj_center = 1
          end if
        end if
!
        if (radius_ctl%icou .gt. 0) then
          nidx_global_rtp(1) = radius_ctl%num
        end if
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
        nlayer_2_center = -1
        nlayer_ICB =       1
        nlayer_CMB =       nidx_global_rtp(1)
        nlayer_mid_OC =   -1
        if(radial_grp_ctl%icou .gt. 0) then
          do i = 1, radial_grp_ctl%num
            if     (cmp_no_case(radial_grp_ctl%c_tbl(i), 'ICB')         &
     &          .gt. 0) then
              nlayer_ICB = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'CMB')         &
     &          .gt. 0) then
              nlayer_CMB = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'to_Center')   &
     &          .gt. 0) then
              nlayer_2_center = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'Mid_Depth')   &
     &          .gt. 0) then
              nlayer_mid_OC = radial_grp_ctl%ivec(i)
            end if
          end do
!
          call dealloc_control_array_c_i(radial_grp_ctl)
        end if
!
!   Set radial grid by Chebyshev or equaidistance
      else
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
        if(Min_radius_ctl .eq. zero) iflag_rj_center = 1
!
        call count_set_radial_grid(num_fluid_grid_ctl,                  &
     &      Min_radius_ctl, Max_radius_ctl)
      end if
!
      ndomain_rtp(1:3) = 1
      if (ndomain_sph_grid_ctl%num .gt. 0) then
        do i = 1, ndomain_sph_grid_ctl%num
          iflag_r = cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'r')     &
     &           + cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'radial')
          iflag_t = cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'theta') &
     &       + cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'meridional')
          if     (iflag_r .gt. 0) then
            ndomain_rtp(1) = ndomain_sph_grid_ctl%ivec(i)
          else if (iflag_t .gt. 0) then
            ndomain_rtp(2) = ndomain_sph_grid_ctl%ivec(i)
          end if
        end do
!
        call deallocate_ndomain_rtp_ctl
      end if
!
      ndomain_rtm(1:3) = 1
      if (ndomain_legendre_ctl%num .gt. 0) then
        do i = 1, ndomain_legendre_ctl%num
          iflag_r = cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'r')     &
     &           + cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'radial')
          iflag_p = cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'phi')   &
     &           + cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'zonal')
          if     (iflag_r .gt. 0) then
            ndomain_rtm(1) = ndomain_legendre_ctl%ivec(i)
          else if (iflag_p .gt. 0) then
            ndomain_rtm(3) = ndomain_legendre_ctl%ivec(i)
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
      if (ndomain_spectr_ctl%num .gt. 0) then
        do i = 1, ndomain_spectr_ctl%num
          iflag_t = cmp_no_case(ndomain_spectr_ctl%c_tbl(i), 'modes')   &
     &       + cmp_no_case(ndomain_spectr_ctl%c_tbl(i), 'degree_order')
          if(iflag_t .gt. 0) ndomain_rj(2) = ndomain_spectr_ctl%ivec(i)
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
      if(ndomain_rtm(1) .ne. ndomain_rtp(1)) then
        write(*,*) 'Set same number of radial subdomains'
        write(*,*) 'for Legendre transform and spherical grids'
        stop
      end if
!
      if(mod(nidx_global_rtp(3),2) .ne. 0) then
        write(*,*) 'Set even number for the number of zonal grids'
        stop
      end if
!
      if(nidx_global_rtp(2) .lt. (l_truncation+1)*3/2) then
        write(*,*) 'Spherical harmonics transform has Ailiasing'
      else if (nidx_global_rtp(2) .lt. (l_truncation+1)) then
        write(*,*) "Grid has less than Nyquist's sampling theorem"
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'icou, kr_sph_boundary, sph_bondary_name',           &
     &             numlayer_sph_bc
        do icou = 1, numlayer_sph_bc
          write(*,*) icou, kr_sph_boundary(icou),                       &
     &               trim(sph_bondary_name(icou))
        end do
      end if
!
      end subroutine s_set_control_4_gen_shell_grids
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_shell_grids
