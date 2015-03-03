!main_test_sph.f90
!     program  test_sph_model
!
      program test_sph_model
!
      use m_precision
!
      use calypso_mpi
      use analyzer_test_sph

      implicit none
!
!
      call calypso_MPI_init
!
      call init_test_sph
      call analyze_test_sph

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_sph_model
