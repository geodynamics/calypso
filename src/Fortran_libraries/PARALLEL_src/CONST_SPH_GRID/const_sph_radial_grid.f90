!>@file   const_sph_radial_grid.f90
!!@brief  module const_sph_radial_grid
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine count_set_radial_grid                                &
!!     &         (num_fluid_grid_ctl, add_ext_layer_ctl, rmin, rmax,    &
!!     &          increment_cheby, sph_param, sph_rtp, s3d_radius)
!!        type(read_integer_item), intent(in) :: num_fluid_grid_ctl
!!        type(ctl_array_real), intent(in) :: add_ext_layer_ctl
!!        type(sph_shell_parameters), intent(inout) :: sph_param
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(spheric_global_radius), intent(inout) :: s3d_radius
!!      subroutine output_set_radial_grid                               &
!!     &         (sph_param, sph_rtp, s3d_radius)
!!        type(sph_shell_parameters), intent(in) :: sph_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!@endverbatim
!
      module const_sph_radial_grid
!
      use m_precision
      use m_constants
!
      use m_spheric_constants
      use t_spheric_parameter
      use t_spheric_global_ranks
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_set_radial_grid                                  &
     &         (num_fluid_grid_ctl, add_ext_layer_ctl, rmin, rmax,      &
     &          increment_cheby, sph_param, sph_rtp, s3d_radius)
!
      use t_control_array_real
      use t_control_array_integer
      use chebyshev_radial_grid
      use half_chebyshev_radial_grid
      use set_radial_grid_sph_shell
!
      type(read_integer_item), intent(in) :: num_fluid_grid_ctl
      type(ctl_array_real), intent(in) :: add_ext_layer_ctl
      integer(kind = kint), intent(in) :: increment_cheby
      real(kind = kreal), intent(in) :: rmin, rmax
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(spheric_global_radius), intent(inout) :: s3d_radius
!
      integer(kind = kint) :: i, nri_tmp
!
!
      sph_param%nlayer_2_center = 1
!
      write(*,*) 'IN sph_rtp%nidx_global_rtp(1)', sph_rtp%nidx_global_rtp(1)
      write(*,*) 'IN sph_param%nlayer_ICB', sph_param%nlayer_ICB
      write(*,*) 'IN sph_param%nlayer_CMB', sph_param%nlayer_CMB
!
      if(sph_param%iflag_radial_grid .eq. igrid_Chebyshev) then
        call count_chebyshev_ext_layers(num_fluid_grid_ctl%intvalue,    &
     &      sph_param%radius_ICB, sph_param%radius_CMB,                 &
     &      rmin, rmax, sph_rtp%nidx_global_rtp(1),                     &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB)
      else if(sph_param%iflag_radial_grid .eq. igrid_half_Chebyshev)    &
     & then
        call count_half_chebyshev_external(num_fluid_grid_ctl%intvalue, &
     &      sph_param%radius_CMB, rmax, sph_rtp%nidx_global_rtp(1),     &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB)
      else if(sph_param%iflag_radial_grid .eq. igrid_equidistance) then
        call count_equi_ext_layers(num_fluid_grid_ctl%intvalue,         &
     &      sph_param%radius_ICB, sph_param%radius_CMB,                 &
     &      rmin, rmax, sph_rtp%nidx_global_rtp(1),                     &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB)
      end if
!
      nri_tmp = sph_rtp%nidx_global_rtp(1)
      do i = 1, add_ext_layer_ctl%num
        if(add_ext_layer_ctl%vect(i) .gt. rmax) then
          sph_rtp%nidx_global_rtp(1) = sph_rtp%nidx_global_rtp(1) + 1
        end if
      end do
!
      write(*,*) 'rmin, rmax', rmin, rmax
      write(*,*) 'sph_rtp%nidx_global_rtp(1)', sph_rtp%nidx_global_rtp(1)
      write(*,*) 'sph_param%nlayer_ICB', sph_param%nlayer_ICB
      write(*,*) 'sph_param%nlayer_CMB', sph_param%nlayer_CMB
!
      call alloc_radius_1d_gl(sph_rtp%nidx_global_rtp(1), s3d_radius)
!
      if(sph_param%iflag_radial_grid .eq. igrid_Chebyshev) then
        call set_chebyshev_distance_shell(nri_tmp,                      &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB,                 &
     &      sph_param%radius_ICB, sph_param%radius_CMB,                 &
     &      s3d_radius%radius_1d_gl(1))
      else if(sph_param%iflag_radial_grid .eq. igrid_half_Chebyshev)    &
     & then
        call half_chebyshev_distance_shell(nri_tmp,                     &
     &      sph_param%nlayer_CMB, sph_param%radius_CMB,                 &
     &      s3d_radius%radius_1d_gl(1))
      else if(sph_param%iflag_radial_grid .eq. igrid_equidistance) then
        call set_equi_distance_shell(nri_tmp,                           &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB,                 &
     &      sph_param%radius_ICB, sph_param%radius_CMB,                 &
     &      s3d_radius%radius_1d_gl(1))
      end if
!
      if(sph_param%iflag_radial_grid .eq. igrid_half_Chebyshev          &
     &    .or. sph_param%iflag_radial_grid .eq. igrid_Chebyshev) then
        call adjust_chebyshev_shell(nri_tmp,                            &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB,                 &
     &      increment_cheby, s3d_radius%radius_1d_gl(1))
      end if
!
      do i = 1, add_ext_layer_ctl%num
        if(add_ext_layer_ctl%vect(i) .gt. rmax) then
          nri_tmp = nri_tmp + 1
          s3d_radius%radius_1d_gl(nri_tmp) = add_ext_layer_ctl%vect(i)
        end if
      end do
!
      end subroutine count_set_radial_grid
!
!  ---------------------------------------------------------------------
!
      subroutine output_set_radial_grid                                 &
     &         (sph_param, sph_rtp, s3d_radius)
!
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spheric_global_radius), intent(in) :: s3d_radius
!
      integer(kind = kint), parameter :: id_file = 14
!
      integer(kind = kint) :: k
!
      write(*,*) 'radial grid is written in radial_point.dat'
      open(id_file, file='radial_point.dat')
!
      write(id_file,'(a)')    '    radial_grid_type_ctl   explicit'
      write(id_file,'(a,i6)') '    array r_layer ',                     &
     &                         sph_rtp%nidx_global_rtp(1)
      do k = 1, sph_rtp%nidx_global_rtp(1)
        write(id_file,'(a,i6,1pE25.15e3)')                              &
     &                        '      r_layer   ', k,                    &
     &                         s3d_radius%radius_1d_gl(k)
      end do
      write(id_file,'(a)')    '    end array r_layer'
      write(id_file,'(a)')    '!'
      write(id_file,'(a)')    '    array  boundaries_ctl   3'
      write(id_file,'(a,i6)')                                           &
     &        '      boundaries_ctl  to_Center ', ione
      write(id_file,'(a,i6)')                                           &
     &        '      boundaries_ctl  ICB       ', sph_param%nlayer_ICB
      write(id_file,'(a,i6)')                                           &
     &        '      boundaries_ctl  CMB       ', sph_param%nlayer_CMB
      write(id_file,'(a)')    '    end array boundaries_ctl'
!
      close(id_file)
!
      end subroutine output_set_radial_grid
!
!  ---------------------------------------------------------------------
!
      end module const_sph_radial_grid
