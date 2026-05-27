!>@file   m_OMP_FFTW3_counter.F90
!!@brief  module m_OMP_FFTW3_counter
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2020
!
!>@brief  Fourier transform using FFTW Ver.3 woth OpenMP
!!
!!@verbatim
!!      subroutine check_init_OMP_FFTW()
!!      subroutine check_clean_OMP_FFTW()
!!@endverbatim
!!
!!@n @param icount_OMP_FFTW  Reference counter for OpenMP FFTW
!
      module m_OMP_FFTW3_counter
!
      use m_precision
      use ISO_C_BINDING
!
      implicit none
!
#ifdef OMP_FFTW3
      include "fftw3.f03"
#endif
!
!>      Reference counter for FFTW with OpenMP
      integer(kind = kint_4b), private :: icount_OMP_FFTW = 0
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine check_init_OMP_FFTW()
!
      integer, external :: omp_get_max_threads
      integer(C_int) :: iout
!
!
      if(icount_OMP_FFTW .le. 0) then
        icount_OMP_FFTW = 0
!
#ifdef OMP_FFTW3
        iout = fftw_init_threads()
        call fftw_plan_with_nthreads(omp_get_max_threads());
#endif
      end if
      icount_OMP_FFTW = icount_OMP_FFTW + 1
!
      end subroutine check_init_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine check_clean_OMP_FFTW()
!
!
      icount_OMP_FFTW = icount_OMP_FFTW - 1
      if(icount_OMP_FFTW .gt. 0) return
!
#ifdef OMP_FFTW3
      call fftw_cleanup_threads()
#endif
!
      end subroutine check_clean_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module m_OMP_FFTW3_counter
