!>@file   t_sph_field_FFTW.F90
!!@brief  module t_sph_field_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine finalize_sph_field_FFTW(FFTW_f)
!!      subroutine alloc_whole_FFTW_plan(Nfft, irt_rtp_smp_stack,       &
!!     &          ncomp_bwd, ncomp_fwd, FFTW_f)
!!      subroutine alloc_fld_FFTW_plan(Nfft, irt_rtp_smp_stack, FFTW_f)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nstacksmp(0:np_smp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!
      module t_sph_field_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_elapsed_labels_SPH_TRNS
!
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
!>      Structure to use SNGLE FFTW
      type work_for_field_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_bwd(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fwd(:)
!
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!
!>        Number of components for forward transform
        integer(kind = kint) :: howmany_fwd
!>        Number of components for backward transform
        integer(kind = kint) :: howmany_bwd
!
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:)
!
!>        Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFTW) :: comm_sph_FFTW
!
!>        temporal area for ordering
        real(kind = kreal), allocatable :: v_tmp(:)
      end type work_for_field_FFTW
!
      private :: dealloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_field_FFTW(FFTW_f)
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: ip
!
!
      call dealloc_comm_table_sph_FFTW(FFTW_f%comm_sph_FFTW)
!
      do ip = 1, np_smp
        call dfftw_destroy_plan(FFTW_f%plan_fwd(ip))
        call dfftw_destroy_plan(FFTW_f%plan_bwd(ip))
        call dfftw_cleanup
      end do
!
      call dealloc_fld_FFTW_plan(FFTW_f)
!
      end subroutine finalize_sph_field_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_whole_FFTW_plan(Nfft, irt_rtp_smp_stack,         &
     &          ncomp_bwd, ncomp_fwd, FFTW_f)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint_gl) :: nnod_rt, ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
      FFTW_f%Nfft_r = Nfft
      FFTW_f%Nfft_c = Nfft/2 + 1
!
      allocate(FFTW_f%plan_bwd(np_smp))
      allocate(FFTW_f%plan_fwd(np_smp))
!
      nnod_rt = ncomp * irt_rtp_smp_stack(np_smp)
      allocate(FFTW_f%X(FFTW_f%Nfft_r*nnod_rt))
      allocate(FFTW_f%C(FFTW_f%Nfft_c*nnod_rt))
      FFTW_f%X = 0.0d0
      FFTW_f%C = 0.0d0
!
      end subroutine alloc_whole_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine alloc_fld_FFTW_plan(Nfft, irt_rtp_smp_stack, FFTW_f)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint_gl) :: nnod_rt
!
!
      FFTW_f%Nfft_r = Nfft
      FFTW_f%Nfft_c = Nfft/2 + 1
!
      allocate(FFTW_f%plan_bwd(np_smp))
      allocate(FFTW_f%plan_fwd(np_smp))
!
      nnod_rt = irt_rtp_smp_stack(np_smp)
      allocate(FFTW_f%X(FFTW_f%Nfft_r*nnod_rt))
      allocate(FFTW_f%C(FFTW_f%Nfft_c*nnod_rt))
      FFTW_f%X = 0.0d0
      FFTW_f%C = 0.0d0
!
      end subroutine alloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_fld_FFTW_plan(FFTW_f)
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      deallocate(FFTW_f%plan_fwd, FFTW_f%plan_bwd)
      deallocate(FFTW_f%X, FFTW_f%C)
!
      end subroutine dealloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module t_sph_field_FFTW
