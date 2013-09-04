!>@file   main_sph_initial_fld.f90
!!@brief  program sph_meke_initial
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to generate initial field
!!@n       Define initial field at const_sph_initial_spectr.f90
!
     program sph_meke_initial
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_sph_initial_fld
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_const_sph_initial
!
      call parallel_cal_fin
!
      stop
      end program sph_meke_initial
