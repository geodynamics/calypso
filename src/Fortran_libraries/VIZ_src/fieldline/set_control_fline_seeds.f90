!>@file   set_control_fline_seeds.f90
!!@brief  module set_control_fline_seeds
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Set seed points from tracers
!!
!!@verbatim
!!      subroutine count_control_fline_seeds(seeds_ctl, fln_prm)
!!      subroutine s_set_control_fline_seeds(seeds_ctl, fln_prm)
!!        type(fline_seeds_list_ctl), intent(in) :: seeds_ctl
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!@endverbatim
!
      module set_control_fline_seeds
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use t_control_params_4_fline
      use t_fline_seeds_list_ctl
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_fline_seeds(seeds_ctl, fln_prm)
!
      type(fline_seeds_list_ctl), intent(in) :: seeds_ctl
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        if(seeds_ctl%seed_surface_ctl%num .gt. 0) then
          fln_prm%num_each_field_line = seeds_ctl%seed_surface_ctl%num
        end if
      else if(fln_prm%id_fline_seed_type .eq. iflag_position_list) then
        if(seeds_ctl%seed_point_ctl%num .gt. 0) then
          fln_prm%num_each_field_line = seeds_ctl%seed_point_ctl%num
        end if
        if(seeds_ctl%seed_geological_ctl%num .gt. 0) then
          fln_prm%num_each_field_line = fln_prm%num_each_field_line     &
    &                               + seeds_ctl%seed_geological_ctl%num
        end if
        if(seeds_ctl%seed_spherical_ctl%num .gt. 0) then
          fln_prm%num_each_field_line = fln_prm%num_each_field_line     &
    &                               + seeds_ctl%seed_spherical_ctl%num
        end if
      end if
!
      end subroutine count_control_fline_seeds
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_fline_seeds(seeds_ctl, fln_prm)
!
      use coordinate_converter
!
      type(fline_seeds_list_ctl), intent(in) :: seeds_ctl
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      real(kind = kreal) :: rr(1), theta(1), phi(1)
      integer(kind = kint) :: i, icou
!
      real(kind = kreal) :: pi
!
      pi = four * atan(one)
!
!
      if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        do i = 1, fln_prm%num_each_field_line
          fln_prm%id_gl_surf_start_fline(1,i)                           &
     &          = seeds_ctl%seed_surface_ctl%int1(i)
          fln_prm%id_gl_surf_start_fline(2,i)                           &
     &          = seeds_ctl%seed_surface_ctl%int2(i)
        end do
      else if(fln_prm%id_fline_seed_type .eq. iflag_position_list) then
        do i = 1, seeds_ctl%seed_point_ctl%num
          fln_prm%xx_surf_start_fline(1,i)                              &
     &       = seeds_ctl%seed_point_ctl%vec1(i)
          fln_prm%xx_surf_start_fline(2,i)                              &
     &       = seeds_ctl%seed_point_ctl%vec2(i)
          fln_prm%xx_surf_start_fline(3,i)                              &
     &       = seeds_ctl%seed_point_ctl%vec3(i)
        end do
        do i = 1, seeds_ctl%seed_geological_ctl%num
          icou = i + seeds_ctl%seed_point_ctl%num
          rr(1) =              seeds_ctl%seed_geological_ctl%vec1(i)
          theta(1) = (90.0d0 - seeds_ctl%seed_geological_ctl%vec2(i))   &
    &               * pi / 180.0d0
          phi(1) = seeds_ctl%seed_geological_ctl%vec3(i) * pi / 180.0d0
          call position_2_xyz(IONE, rr(1), theta(1), phi(1),            &
    &                         fln_prm%xx_surf_start_fline(1,icou),      &
    &                         fln_prm%xx_surf_start_fline(2,icou),      &
    &                         fln_prm%xx_surf_start_fline(3,icou))
        end do
        do i = 1, seeds_ctl%seed_spherical_ctl%num
          icou = i + seeds_ctl%seed_point_ctl%num                       &
    &              + seeds_ctl%seed_geological_ctl%num
          rr(1) =    seeds_ctl%seed_spherical_ctl%vec1(i)
          theta(1) = seeds_ctl%seed_spherical_ctl%vec2(i)
          phi(1) =   seeds_ctl%seed_spherical_ctl%vec3(i)
!          write(*,*) my_rank, i, 'seed_spherical_ctl',  &
!    &        seeds_ctl%seed_spherical_ctl%vec1(i),            &
!    &        seeds_ctl%seed_spherical_ctl%vec2(i),            &
!    &        seeds_ctl%seed_spherical_ctl%vec3(i)
!          write(*,*) my_rank, i, 'seed_spherical_ctl',                 &
!    &            rr(1), theta(1), phi(1)
          call position_2_xyz(IONE, rr(1), theta(1), phi(1),            &
    &                         fln_prm%xx_surf_start_fline(1,icou),      &
    &                         fln_prm%xx_surf_start_fline(2,icou),      &
    &                         fln_prm%xx_surf_start_fline(3,icou))
!          write(*,*) my_rank, i, icou, 'xx_surf_start_fline',          &
!    &              fln_prm%xx_surf_start_fline(:,icou)
        end do
!       
!        do i = 1, fln_prm%num_each_field_line
!          write(*,*) i, 'fln_prm%xx_surf_start_fline',                 &
!     &        fln_prm%xx_surf_start_fline(:,i)
!        end do
      end if
!
      end subroutine s_set_control_fline_seeds
!
!  ---------------------------------------------------------------------
!
      end module set_control_fline_seeds
