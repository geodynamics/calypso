!>@file   m_fftw_parameters.f90
!!        module m_fftw_parameters
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Parameters for FFTW
!!
!
      module m_fftw_parameters
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
!>      plan ID for fftw
      integer, parameter :: fftw_plan =    8
!>      data size of complex for FFTW3
      integer, parameter :: fftw_complex = 8
!
!>      Unit real number
      complex(kind = fftw_complex), parameter :: ru = (1.0d0,0.0d0)
!>      Unit imaginary number
      complex(kind = fftw_complex), parameter :: iu = (0.0d0,1.0d0)
!
!>      estimation flag for FFTW
      integer(kind = 4), parameter :: FFTW_KEMO_EST = 64
!>      Meajor flag for FFTW
      integer(kind = 4), parameter :: FFTW_KEMO_MEASURE = 0
!
      end module m_fftw_parameters
