!>@file   t_read_sph_series.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine alloc_sph_spectr_series                              &
!!     &         (ltr, sph_IN, n_step, sph_series)
!!      subroutine dealloc_sph_spectr_series(sph_series)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(read_sph_spectr_params), intent(in) :: sph_IN
!!        type(read_sph_spectr_series), intent(inout) :: sph_series
!!
!!      subroutine copy_spectr_IO_to_series                             &
!!     &          (icou, ltr, sph_IN, sph_series)
!!        integer(kind = kint), intent(in) :: icou
!!        type(read_sph_spectr_params), intent(in) :: sph_IN
!!        type(read_sph_spectr_series), intent(inout) :: sph_series
!!@endverbatim
      module t_read_sph_series
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
!
      implicit none
!
!
      type read_sph_spectr_series
!>        Number of time series
        integer(kind = kint) :: n_step = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        spectr time series
        real(kind = kreal), allocatable :: d_spectr(:,:,:,:)
      end type read_sph_spectr_series
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_spectr_series                                &
     &         (ltr, sph_IN, n_step, sph_series)
!
      integer(kind = kint), intent(in) :: ltr, n_step
      type(read_sph_spectr_params), intent(in) :: sph_IN
      type(read_sph_spectr_series), intent(inout) :: sph_series
!
      integer(kind = kint) :: ncomp, nri
!
!
      ncomp = sph_IN%ntot_sph_spec
      nri =   sph_IN%nri_dat
      sph_series%n_step = n_step
      allocate( sph_series%i_step(sph_series%n_step) )
      allocate( sph_series%d_time(sph_series%n_step) )
      allocate( sph_series%d_spectr(ncomp,0:ltr,nri,sph_series%n_step))
!
!$omp parallel workshare
      sph_series%i_step(1:sph_series%n_step) = izero
      sph_series%d_time(1:sph_series%n_step) = zero
!$omp end parallel workshare
!$omp parallel workshare
      sph_series%d_spectr(1:ncomp,0:ltr,1:nri,1:n_step) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_sph_spectr_series
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_spectr_series(sph_series)
!
      type(read_sph_spectr_series), intent(inout) :: sph_series
!
!
      deallocate( sph_series%i_step, sph_series%d_time)
      deallocate( sph_series%d_spectr)
!
      end subroutine dealloc_sph_spectr_series
!
!   --------------------------------------------------------------------
!
      subroutine copy_spectr_IO_to_series                               &
     &          (icou, ltr, sph_IN, sph_series)
!
      integer(kind = kint), intent(in) :: icou, ltr
      type(read_sph_spectr_params), intent(in) :: sph_IN
      type(read_sph_spectr_series), intent(inout) :: sph_series
!
      integer(kind = kint) :: ncomp, nri
!
!
      ncomp = sph_IN%ntot_sph_spec
      nri =   sph_IN%nri_dat
!
      sph_series%i_step(icou) = sph_IN%i_step
      sph_series%d_time(icou) = sph_IN%time
!$omp parallel workshare
      sph_series%d_spectr(1:ncomp,0:ltr,1:nri,icou)                     &
     &            = sph_IN%spectr_IO(1:ncomp,0:ltr,1:nri)
!$omp end parallel workshare
!
      end subroutine copy_spectr_IO_to_series
!
!   --------------------------------------------------------------------
!
      end module t_read_sph_series
