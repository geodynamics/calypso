!>@file   set_ctl_4_shell_grids.f90
!!@brief  module set_ctl_4_shell_grids
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine set_control_4_shell_grids(nprocs_check,              &
!!     &          spctl, sdctl, sph, gen_sph, ierr)
!!        type(sphere_data_control), intent(inout) :: spctl
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(sph_grids), intent(inout) :: sph
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module set_ctl_4_shell_grids
!
      use m_precision
!
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_gen_sph_shell
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
      use t_spheric_global_ranks
      use t_const_spherical_grid
!
      implicit  none
!
      character(len=kchara), parameter :: cflag_SGS_r = 'SGS_r'
      character(len=kchara), parameter :: cflag_SGS_t = 'SGS_theta'
!
      private :: cflag_SGS_r, cflag_SGS_t
      private :: set_ctl_radius_4_shell, set_control_4_SGS_shell
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_shell_grids(nprocs_check,                &
     &          spctl, sdctl, sph, gen_sph, ierr)
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use m_error_IDs
      use m_file_format_switch
!
      use set_controls_4_sph_shell
      use set_control_sph_subdomains
      use skip_comment_f
!
      integer, intent(in) :: nprocs_check
      type(sphere_data_control), intent(in) :: spctl
      type(sphere_domain_control), intent(in) :: sdctl
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_FEM_mesh_mode_4_SPH(spctl, sph%sph_params)
!
!
      sph%sph_rtp%nidx_global_rtp(1) = 2
      sph%sph_rtp%nidx_global_rtp(2) = 2
      sph%sph_rtp%nidx_global_rtp(3) = 4
      sph%sph_params%l_truncation = 2
      sph%sph_params%m_folding =    1
!
      if(spctl%ltr_ctl%iflag .gt. 0) then
        sph%sph_params%l_truncation = spctl%ltr_ctl%intvalue
      end if
!
      if(spctl%phi_symmetry_ctl%iflag .gt. 0) then
        sph%sph_params%m_folding = spctl%phi_symmetry_ctl%intvalue
      end if
!
      if(spctl%ngrid_elevation_ctl%iflag .gt. 0) then
        sph%sph_rtp%nidx_global_rtp(2)                                  &
     &          = spctl%ngrid_elevation_ctl%intvalue
      end if
!
!      if (spctl%ngrid_azimuth_ctl%iflag .gt. 0) then
!        sph%sph_rtp%nidx_global_rtp(3)                                 &
!     &         = spctl%ngrid_azimuth_ctl%intvalue
!      end if
!
      call set_ctl_radius_4_shell                                       &
     &   (spctl, sph%sph_params, sph%sph_rtp, sph%sph_rj,               &
     &    gen_sph%added_radial_grp, gen_sph%s3d_radius, ierr)
!
      call set_subdomains_4_sph_shell                                   &
     &    (nprocs_check, sdctl, gen_sph%s3d_ranks, ierr, e_message)
      call set_inner_loop_4_sph_shell(sdctl, gen_sph%s3d_ranks)
!
      if (ierr .gt. 0) return
!
!
!   Set layering parameter for SGS models
      call set_control_4_SGS_shell                                      &
     &   (sph, spctl, gen_sph%r_layer_grp, gen_sph%med_layer_grp)
!
!  Check
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
        if(mod(sph%sph_rtp%nidx_global_rtp(3),2) .ne. 0) then
          write(*,*) 'Set even number for the number of zonal grids'
          ierr = ierr_mesh
          return
        end if
      end if
!
      if(sph%sph_rtp%nidx_global_rtp(2)                                 &
     &      .lt. (sph%sph_params%l_truncation+1)*3/2) then
        write(*,*) 'Spherical harmonics transform has Aliasing'
      else if (sph%sph_rtp%nidx_global_rtp(2)                           &
     &      .lt. (sph%sph_params%l_truncation+1)) then
        write(*,*) "Grid has less than Nyquist's sampling theorem"
      end if
!
      end subroutine set_control_4_shell_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_radius_4_shell                                 &
     &         (spctl, sph_params, sph_rtp, sph_rj,                     &
     &          added_radial_grp, s3d_radius, ierr)
!
      use m_error_IDs
!
      use const_sph_radial_grid
      use skip_comment_f
!
      type(sphere_data_control), intent(in) :: spctl
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(spheric_global_radius), intent(inout) :: s3d_radius
      type(layering_group_list), intent(inout) :: added_radial_grp
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: i, kr, icou, nele
      integer(kind = kint) :: increment_cheby = 1
      real(kind = kreal) :: ICB_to_CMB_ratio, fluid_core_size
      real(kind = kreal) :: r_in, r_out
!
!
      sph_params%iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(spctl%radial_grid_type_ctl%charavalue,             &
     &               label_explicit))                                   &
     &       sph_params%iflag_radial_grid =  igrid_non_equidist
      if(cmp_no_case(spctl%radial_grid_type_ctl%charavalue,             &
     &               label_Chebyshev))                                  &
     &       sph_params%iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(spctl%radial_grid_type_ctl%charavalue,             &
     &               label_half_Cbyv))                                  &
     &       sph_params%iflag_radial_grid =  igrid_half_Chebyshev
      if(cmp_no_case(spctl%radial_grid_type_ctl%charavalue,             &
     &               label_equi))                                       &
     &       sph_params%iflag_radial_grid =  igrid_equidistance
!
!   Set radial group
      if(spctl%radial_grp_ctl%icou .le. 0) added_radial_grp%nlayer = 0
      call alloc_layering_group                                         &
     &   (spctl%radial_grp_ctl%num, added_radial_grp)
!
      icou = 0
      do i = 1, added_radial_grp%nlayer
        if     (cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),              &
     &                      ICB_nod_grp_name)) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else if(cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),              &
     &                      CMB_nod_grp_name)) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else if(cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),              &
     &                      CTR_nod_grp_name)) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else if(cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),              &
     &                      'Mid_Depth')) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else
          icou = icou + 1
          added_radial_grp%istart(icou) = spctl%radial_grp_ctl%ivec(i)
          added_radial_grp%iend(icou) =   spctl%radial_grp_ctl%ivec(i)
          added_radial_grp%name(icou) =   spctl%radial_grp_ctl%c_tbl(i)
        end if
      end do
!
!   Set radial grid explicitly
      sph_rj%iflag_rj_center = 0
      if(sph_params%iflag_radial_grid .eq. igrid_non_equidist) then
        if(cmp_no_case(spctl%sph_coef_type_ctl%charavalue,              &
     &                 'with_center')                                   &
          .and. spctl%sph_coef_type_ctl%iflag .gt. 0) then
          sph_rj%iflag_rj_center = 1
        end if
!
        if (spctl%radius_ctl%icou .gt. 0) then
          sph_rtp%nidx_global_rtp(1) = spctl%radius_ctl%num
        end if
!
        if (sph_rtp%nidx_global_rtp(1) .gt. 0) then
          call alloc_radius_1d_gl                                       &
     &       (sph_rtp%nidx_global_rtp(1), s3d_radius)
!
          do i = 1, sph_rtp%nidx_global_rtp(1)
            kr = spctl%radius_ctl%ivec(i)
            s3d_radius%radius_1d_gl(kr) = spctl%radius_ctl%vect(i)
          end do
        end if
!
        sph_params%nlayer_2_center = -1
        sph_params%nlayer_ICB =       1
        sph_params%nlayer_CMB = sph_rtp%nidx_global_rtp(1)
        sph_params%nlayer_mid_OC =   -1
        if(spctl%radial_grp_ctl%icou .gt. 0) then
          do i = 1, spctl%radial_grp_ctl%num
            if     (cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),          &
     &                      ICB_nod_grp_name) ) then
              sph_params%nlayer_ICB = spctl%radial_grp_ctl%ivec(i)
            else if(cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),          &
     &                      CMB_nod_grp_name) ) then
              sph_params%nlayer_CMB = spctl%radial_grp_ctl%ivec(i)
            else if(cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),          &
     &                      CTR_nod_grp_name) ) then
              sph_params%nlayer_2_center = spctl%radial_grp_ctl%ivec(i)
            else if(cmp_no_case(spctl%radial_grp_ctl%c_tbl(i),          &
     &                      'Mid_Depth') ) then
              sph_params%nlayer_mid_OC = spctl%radial_grp_ctl%ivec(i)
            end if
          end do
!
        end if
!
!   Set radial grid by Chebyshev, equaidistance, or half Chebyshev
      else
        if(spctl%ICB_radius_ctl%iflag .gt. 0                            &
     &     .and. spctl%CMB_radius_ctl%iflag .gt. 0) then
          sph_params%radius_ICB = spctl%ICB_radius_ctl%realvalue
          sph_params%radius_CMB = spctl%CMB_radius_ctl%realvalue
        else if(spctl%fluid_core_size_ctl%iflag .gt. 0                  &
     &       .and. spctl%ICB_to_CMB_ratio_ctl%iflag .gt. 0) then
          ICB_to_CMB_ratio = spctl%ICB_to_CMB_ratio_ctl%realvalue
          fluid_core_size =  spctl%fluid_core_size_ctl%realvalue
          sph_params%radius_ICB = fluid_core_size                       &
     &           * ICB_to_CMB_ratio / (one - ICB_to_CMB_ratio)
          sph_params%radius_CMB = sph_params%radius_ICB                 &
     &                           + fluid_core_size
        else
          write(*,*)                                                    &
     &       'Set CMB and ICB radii or ratio and size of outer core'
          ierr = ierr_mesh
          return
        end if
!
        if(spctl%Min_radius_ctl%iflag.eq.0) then
          r_in = sph_params%radius_ICB
        else
          r_in = spctl%Min_radius_ctl%realvalue
        end if
!
        if(spctl%Max_radius_ctl%iflag.eq.0) then
          r_out = sph_params%radius_CMB
        else
          r_out = spctl%Max_radius_ctl%realvalue
        end if
!
        if(r_in .eq. zero)  sph_rj%iflag_rj_center = 1
!
        increment_cheby = 1
        if(spctl%increment_cheby_ctl%iflag .gt. 0) then
          increment_cheby = spctl%increment_cheby_ctl%intvalue
          if(increment_cheby .le. 0) increment_cheby = 1
        end if
!
        if(sph_params%iflag_radial_grid .eq. igrid_half_Chebyshev       &
     &     .or. sph_params%iflag_radial_grid .eq. igrid_Chebyshev) then
          nele = sph_params%nlayer_CMB-sph_params%nlayer_ICB
          if(mod(nele,increment_cheby) .ne. 0) then
            write(*,*) 'Chebyshev increment should be ',                &
     &                 'devisor of (# fluid grid - 1)'
            ierr = ierr_mesh
            return
          end if
        end if
!
        call count_set_radial_grid                                      &
     &     (spctl%num_fluid_grid_ctl, spctl%add_ext_layer_ctl,          &
     &      r_in, r_out, increment_cheby, sph_params,                   &
     &      sph_rtp, s3d_radius)
      end if
!
!       Check whole sphere model
      if(sph_params%iflag_radial_grid .eq. igrid_half_Chebyshev) then
        if(sph_params%radius_ICB .ne. zero) then
          write(*,*)                                                    &
     &     'Set ICB radius to be zero for whole sphere'
          ierr = ierr_mesh
          return
        end if
!
        if(spctl%Min_radius_ctl%iflag .ne. 0                            &
     &     .and. r_in .ne. zero) then
          write(*,*)                                                    &
     &     'Set minimum radius to be zero for whole sphere'
          ierr = ierr_mesh
          return
        end if
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'icou, kr_sph_boundary, sph_bondary_name',           &
     &             added_radial_grp%nlayer
        do icou = 1, added_radial_grp%nlayer
          write(*,*) icou, added_radial_grp%istart(icou),               &
     &               trim(added_radial_grp%name(icou))
        end do
      end if
!
      end subroutine set_ctl_radius_4_shell
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_SGS_shell                                &
     &         (sph, spctl, r_layer_grp, med_layer_grp)
!
      use t_control_1D_layering
!
      type(sph_grids), intent(in) :: sph
      type(sphere_data_control), intent(in) :: spctl
      type(layering_group_list), intent(inout) :: r_layer_grp
      type(layering_group_list), intent(inout) :: med_layer_grp
!
!
!   Set layering parameter for SGS models
      if(spctl%radial_layer_list_ctl%num .gt. 0) then
        call set_group_by_layering_list                                 &
     &    (cflag_SGS_r, spctl%radial_layer_list_ctl, r_layer_grp)
      else if(spctl%num_radial_layer_ctl%iflag .gt. 0) then
        call set_group_by_equidivide(cflag_SGS_r,                       &
     &      sph%sph_params%nlayer_ICB, sph%sph_params%nlayer_CMB,       &
     &      spctl%num_radial_layer_ctl, r_layer_grp)
      else
        call alloc_layering_group(izero, r_layer_grp)
      end if
!
      if(spctl%med_layer_list_ctl%num .gt. 0) then
        call set_group_by_layering_list                                 &
     &    (cflag_SGS_t, spctl%med_layer_list_ctl, med_layer_grp)
      else if(spctl%num_med_layer_ctl%iflag .gt. 0) then
        call set_group_by_equidivide                                    &
     &    (cflag_SGS_t, ione, sph%sph_rtp%nidx_global_rtp(2),           &
     &     spctl%num_med_layer_ctl, med_layer_grp)
      else
        call alloc_layering_group(izero, med_layer_grp)
      end if
!
      end subroutine set_control_4_SGS_shell
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_4_shell_grids
