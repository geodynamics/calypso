!
!     program  sph_zm_snap_noviz

!-----------------------------------------------------------------------
      program sph_zm_snap_noviz
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for zonal mean field           on May, 2012 (ver 2.0)
!

      use m_precision
!
      use m_parallel_var_dof
      use analyzer_noviz_sph_zm_snap
!
      implicit none
!
      call parallel_cal_init
!
      call initialization
      call evolution
!
      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_zm_snap_noviz
