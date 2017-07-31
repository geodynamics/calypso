!
!      module const_sph_radial_grid
!
!        programmed by H.Matsui on July, 2007
!
!!      subroutine count_set_radial_grid                                &
!!     &          (nele, rmin, rmax, sph_param, sph_rtp, s3d_radius)
!!        type(sph_shell_parameters), intent(inout) :: sph_param
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(spheric_global_radius), intent(inout) :: s3d_radius
!!      subroutine output_set_radial_grid                               &
!!     &         (sph_param, sph_rtp, s3d_radius)
!!        type(sph_shell_parameters), intent(in) :: sph_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(spheric_global_radius), intent(in) :: s3d_radius
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
     &          (nele, rmin, rmax, sph_param, sph_rtp, s3d_radius)
!
      use chebyshev_radial_grid
      use half_chebyshev_radial_grid
      use set_radial_grid_sph_shell
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in) :: rmin, rmax
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(spheric_global_radius), intent(inout) :: s3d_radius
!
!
      sph_param%nlayer_2_center = 1
!
      if(sph_param%iflag_radial_grid .eq. igrid_Chebyshev) then
        call count_chebyshev_ext_layers                                 &
     &     (nele, sph_param%radius_ICB, sph_param%radius_CMB,           &
     &      rmin, rmax, sph_rtp%nidx_global_rtp(1),                     &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB)
      else if(sph_param%iflag_radial_grid .eq. igrid_half_Chebyshev)    &
     & then
        call count_half_chebyshev_external(nele, sph_param%radius_CMB,  &
     &      rmax, sph_rtp%nidx_global_rtp(1),                           &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB)
      else if(sph_param%iflag_radial_grid .eq. igrid_equidistance) then
        call count_equi_ext_layers                                      &
     &     (nele, sph_param%radius_ICB, sph_param%radius_CMB,           &
     &      rmin, rmax, sph_rtp%nidx_global_rtp(1),                     &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB)
      end if
!
      call alloc_radius_1d_gl(sph_rtp%nidx_global_rtp(1), s3d_radius)
!
      if(sph_param%iflag_radial_grid .eq. igrid_Chebyshev) then
        call set_chebyshev_distance_shell(sph_rtp%nidx_global_rtp(1),   &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB,                 &
     &      sph_param%radius_ICB, sph_param%radius_CMB,                 &
     &      s3d_radius%radius_1d_gl)
      else if(sph_param%iflag_radial_grid .eq. igrid_half_Chebyshev)    &
     & then
        call half_chebyshev_distance_shell(sph_rtp%nidx_global_rtp(1),  &
     &      sph_param%nlayer_CMB, sph_param%radius_CMB,                 &
     &      s3d_radius%radius_1d_gl)
      else if(sph_param%iflag_radial_grid .eq. igrid_equidistance) then
        call set_equi_distance_shell(sph_rtp%nidx_global_rtp(1),        &
     &      sph_param%nlayer_ICB, sph_param%nlayer_CMB,                 &
     &      sph_param%radius_ICB, sph_param%radius_CMB,                 &
     &      s3d_radius%radius_1d_gl)
      end if
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
