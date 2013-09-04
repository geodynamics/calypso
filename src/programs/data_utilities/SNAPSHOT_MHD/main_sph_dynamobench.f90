!>@file   main_sph_dynamobench.f90
!!@brief  program sph_dynamobench
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!> @brief Main program for dynamo benchmark check
!
      program sph_dynamobench
!
      use m_precision
!
      use analyzer_sph_dynamobench
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_sph_dynamobench
!
      call evolution_sph_dynamobench
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_dynamobench
