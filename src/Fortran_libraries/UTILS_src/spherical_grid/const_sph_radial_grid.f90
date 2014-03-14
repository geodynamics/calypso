!
!      module const_sph_radial_grid
!
!        programmed by H.Matsui on July, 2007
!
!      subroutine count_set_radial_grid(nele, rmin, rmax)
!      subroutine output_set_radial_grid
!
      module const_sph_radial_grid
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_set_radial_grid(nele, rmin, rmax)
!
      use m_spheric_constants
      use m_spheric_parameter
      use set_radial_grid_sph_shell
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in) :: rmin, rmax
!
!
      nlayer_2_center = 1
!
      if(iflag_radial_grid .eq. igrid_Chebyshev) then
        call count_chebyshev_ext_layers(nele, r_ICB, r_CMB, rmin, rmax, &
     &      nidx_global_rtp(1), nlayer_ICB, nlayer_CMB)
      else if(iflag_radial_grid .eq. igrid_euqidistance) then
        call count_equi_ext_layers(nele, r_ICB, r_CMB, rmin, rmax,      &
     &      nidx_global_rtp(1), nlayer_ICB, nlayer_CMB)
      end if
!
      call allocate_radius_1d_gl
!
      if(iflag_radial_grid .eq. igrid_Chebyshev) then
        call set_chebyshev_distance_shell(nidx_global_rtp(1),           &
     &      nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, radius_1d_gl)
      else if(iflag_radial_grid .eq. igrid_euqidistance) then
        call set_equi_distance_shell(nidx_global_rtp(1),                &
     &        nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, radius_1d_gl)
      end if
!
      end subroutine count_set_radial_grid
!
!  ---------------------------------------------------------------------
!
      subroutine output_set_radial_grid
!
      use m_spheric_parameter
!
!
      integer(kind = kint), parameter :: id_file = 14
!
      integer(kind = kint) :: k
!
      write(*,*) 'radial grid is written in radial_point.dat'
      open(id_file, file='radial_point.dat')
!
      write(id_file,'(a)')    '    radial_grid_type_ctl   explicit'
      write(id_file,'(a,i6)') '    array r_layer ', nidx_global_rtp(1)
      do k = 1, nidx_global_rtp(1)
        write(id_file,'(a,i6,1pE25.15e3)')                              &
     &                     '      r_layer   ', k, radius_1d_gl(k)
      end do
      write(id_file,'(a)')    '    end array r_layer'
      write(id_file,'(a)')    '!'
      write(id_file,'(a)')    '    array  boundaries_ctl   3'
      write(id_file,'(a,i6)')                                           &
     &        '      boundaries_ctl  to_Center ', ione
      write(id_file,'(a,i6)')                                           &
     &        '      boundaries_ctl  ICB       ', nlayer_ICB
      write(id_file,'(a,i6)')                                           &
     &        '      boundaries_ctl  CMB       ', nlayer_CMB
      write(id_file,'(a)')    '    end array boundaries_ctl'
!
      close(id_file)
!
      end subroutine output_set_radial_grid
!
!  ---------------------------------------------------------------------
!
      end module const_sph_radial_grid
