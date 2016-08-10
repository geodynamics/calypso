!>@file   set_ctl_gen_shell_grids.f90
!!@brief  module set_ctl_gen_shell_grids
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine s_set_control_4_gen_shell_grids(ierr)
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
      subroutine s_set_control_4_gen_shell_grids(ierr)
!
      use m_constants
      use m_machine_parameter
      use m_read_mesh_data
      use m_spheric_constants
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
      use m_sph_mesh_1d_connect
      use m_error_IDs
!
      use m_node_id_spherical_IO
      use m_file_format_switch
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_sphere_model
      use m_ctl_data_4_divide_sphere
!
      use set_controls_4_sph_shell
      use const_sph_radial_grid
      use set_control_platform_data
      use set_control_sph_subdomains
      use gen_sph_grids_modes
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: nprocs_ctl
      integer(kind = kint) :: i, np, kr, icou
      real(kind = kreal) :: ICB_to_CMB_ratio, fluid_core_size
!
!
      nprocs_ctl = 1
      if(ndomain_ctl%iflag .gt. 0) nprocs_ctl = ndomain_ctl%intvalue
      call turn_off_debug_flag_by_ctl(izero)
      call set_control_mesh_def
      call set_control_sph_mesh
!
      iflag_excluding_FEM_mesh = 0
      if(excluding_FEM_mesh_ctl%iflag .gt. 0                            &
     &    .and. yes_flag(excluding_FEM_mesh_ctl%charavalue)) then
        iflag_excluding_FEM_mesh = 1
      end if
!
      call set_FEM_mesh_mode_4_SPH(iflag_shell_mode)
!
      nidx_global_rtp(1) = 2
      nidx_global_rtp(2) = 2
      nidx_global_rtp(3) = 4
      l_truncation = 2
      m_folding =    1
!
      iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_explicit))  &
     &       iflag_radial_grid =  igrid_non_equidist
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_Chebyshev)) &
     &       iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_equi))      &
     &       iflag_radial_grid =  igrid_equidistance
!
      if (ltr_ctl%iflag .gt. 0) then
        l_truncation = ltr_ctl%intvalue
      end if
!
      if (phi_symmetry_ctl%iflag .gt. 0) then
        m_folding = phi_symmetry_ctl%intvalue
      end if
!
      if (ngrid_elevation_ctl%iflag .gt. 0) then
        nidx_global_rtp(2) = ngrid_elevation_ctl%intvalue
      end if
!
!      if (ngrid_azimuth_ctl%iflag .gt. 0) then
!        nidx_global_rtp(3) = ngrid_azimuth_ctl%intvalue
!      end if
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
        if     (cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      ICB_nod_grp_name)) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      CMB_nod_grp_name)) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      CTR_nod_grp_name)) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'Mid_Depth')) then
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
      if(iflag_radial_grid .eq. igrid_non_equidist) then
        if(cmp_no_case(sph_coef_type_ctl%charavalue, 'with_center')     &
          .and. sph_coef_type_ctl%iflag .gt. 0) iflag_rj_center = 1
!
        if (radius_ctl%icou .gt. 0) nidx_global_rtp(1) = radius_ctl%num
!
        if (nidx_global_rtp(1) .gt. 0) then
          call allocate_radius_1d_gl(nidx_global_rtp(1))
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
            if     (cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      ICB_nod_grp_name) ) then
              nlayer_ICB = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      CMB_nod_grp_name) ) then
              nlayer_CMB = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      CTR_nod_grp_name) ) then
              nlayer_2_center = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      'Mid_Depth') ) then
              nlayer_mid_OC = radial_grp_ctl%ivec(i)
            end if
          end do
!
          call dealloc_control_array_c_i(radial_grp_ctl)
        end if
!
!   Set radial grid by Chebyshev or equaidistance
      else
        if(ICB_radius_ctl%iflag .gt. 0                                  &
     &     .and. CMB_radius_ctl%iflag .gt. 0) then
          r_ICB = ICB_radius_ctl%realvalue
          r_CMB = CMB_radius_ctl%realvalue
        else if(fluid_core_size_ctl%iflag .gt. 0                        &
     &       .and. ICB_to_CMB_ratio_ctl%iflag .gt. 0) then
          ICB_to_CMB_ratio = ICB_to_CMB_ratio_ctl%realvalue
          fluid_core_size =  fluid_core_size_ctl%realvalue
          r_ICB = fluid_core_size                                       &
     &           * ICB_to_CMB_ratio / (one - ICB_to_CMB_ratio)
          r_CMB = r_ICB + fluid_core_size
        else
          write(*,*)                                                    &
     &       'Set CMB and ICB radii or ratio and size of outer core'
          stop
        end if
!
        if(Min_radius_ctl%iflag.eq.0) Min_radius_ctl%realvalue = r_ICB
        if(Max_radius_ctl%iflag.eq.0) Max_radius_ctl%realvalue = r_CMB
!
        if(Min_radius_ctl%realvalue .eq. zero) iflag_rj_center = 1
!
        call count_set_radial_grid(num_fluid_grid_ctl%intvalue,         &
     &      Min_radius_ctl%realvalue, Max_radius_ctl%realvalue)
      end if
!
!
      call set_subdomains_4_sph_shell(nprocs_ctl, ierr, e_message)
      if (ierr .gt. 0) return
!
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(mod(nidx_global_rtp(3),2) .ne. 0) then
          write(*,*) 'Set even number for the number of zonal grids'
          ierr = ierr_mesh
          return
        end if
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
