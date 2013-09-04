!main_test_sph.f90
!     program  test_sph_model

!-----------------------------------------------------------------------
      program test_sph_model
! \beginSUBROUTINE
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      use m_precision
!
      use m_parallel_var_dof
      use analyzer_test_sph

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer
      call analyze

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_sph_model
