!main_test_sph.f90
!     program  test_sph_model
!
      program test_sph_model
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_test_sph

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer
      call analyze

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_sph_model
