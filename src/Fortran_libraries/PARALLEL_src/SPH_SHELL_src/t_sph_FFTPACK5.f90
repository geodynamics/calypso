!>@file   t_sph_FFTPACK5.f90
!!@brief  module t_sph_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!      subroutine init_sph_FFTPACK5(sph_rtp, comm_rtp,                 &
!!     &                             ncomp_bwd, ncomp_fwd, fftpack_t)
!!      subroutine finalize_sph_FFTPACK5(fftpack_t)
!!      subroutine verify_sph_FFTPACK5(sph_rtp, comm_rtp,               &
!!     &                               ncomp_bwd, ncomp_fwd, fftpack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_fftpack), intent(inout) :: fftpack_t
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module t_sph_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFT
!
      use calypso_mpi
!
      implicit none
!
!>      Structure to use ISPACK
      type work_for_fftpack
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Size of work constant for FFTPACK
        integer(kind = kint) :: NSV
!>        Work constatnts for FFTPACK
        real(kind = 8), allocatable :: WSV(:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
!
!>      Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFT) :: comm_sph_FFTPACK
!
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for FFTPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_for_fftpack
!
      private :: alloc_work_4_FFTPACK, alloc_const_4_FFTPACK
      private :: dealloc_work_4_FFTPACK, dealloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFTPACK5(sph_rtp, comm_rtp,                   &
     &                             ncomp_bwd, ncomp_fwd, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      fftpack_t%Mmax_smp = sph_rtp%istack_rtp_rt_smp(np_smp)            &
     &                    * max(ncomp_bwd, ncomp_fwd)
!
      call alloc_const_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,    &
     &    ierr)
!
      call alloc_work_4_FFTPACK                                         &
     &   (fftpack_t%Mmax_smp, sph_rtp%nidx_rtp(3), fftpack_t)
!
      end subroutine init_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFTPACK5(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
!
      call dealloc_comm_table_sph_FFT(fftpack_t%comm_sph_FFTPACK)
      call dealloc_const_4_FFTPACK(fftpack_t)
      call dealloc_work_4_FFTPACK(fftpack_t)
!
      end subroutine finalize_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFTPACK5(sph_rtp, comm_rtp,                 &
     &                               ncomp_bwd, ncomp_fwd, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      fftpack_t%Mmax_smp = sph_rtp%istack_rtp_rt_smp(np_smp)            &
     &                    * max(ncomp_bwd, ncomp_fwd)
!
      if(fftpack_t%iflag_fft_len .ne. sph_rtp%nidx_rtp(3)) then
        if(fftpack_t%iflag_fft_len .lt. 0) then
          call alloc_const_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        else if((fftpack_t%Mmax_smp*sph_rtp%nidx_rtp(3))                &
     &          .gt. size(fftpack_t%X) ) then
          call dealloc_const_4_FFTPACK(fftpack_t)
          call alloc_const_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        end if
!
        call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,  &
     &              ierr)
      end if
!
      if(allocated(fftpack_t%WK) .eqv. .false.) then
        call alloc_work_4_FFTPACK                                       &
     &     (fftpack_t%Mmax_smp, sph_rtp%nidx_rtp(3), fftpack_t)
      else if( (fftpack_t%Mmax_smp*sph_rtp%nidx_rtp(3))                 &
     &      .gt. size(fftpack_t%WK,1)  ) then
        call dealloc_work_4_FFTPACK(fftpack_t)
        call alloc_work_4_FFTPACK                                       &
     &     (fftpack_t%Mmax_smp, sph_rtp%nidx_rtp(3), fftpack_t)
      end if
!
      end subroutine verify_sph_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTPACK(Ncomp, Nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
!
      allocate( fftpack_t%X(Ncomp*Nfft) )
      allocate( fftpack_t%WK(Ncomp*Nfft) )
!
      fftpack_t%WK = 0.0d0
!
      end subroutine alloc_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_FFTPACK(nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: itmp
!
      fftpack_t%iflag_fft_len = nfft
      itmp = int(log(real(Nfft)) / log(two),KIND(itmp))
      fftpack_t%NSV = nfft + itmp + ifour
      allocate(fftpack_t%WSV(fftpack_t%NSV) )
      fftpack_t%WSV = 0.0d0
!
      end subroutine alloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
!
        deallocate(fftpack_t%X, fftpack_t%WK)
!
      end subroutine dealloc_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      deallocate( fftpack_t%WSV )
      fftpack_t%iflag_fft_len = 0
!
      end subroutine dealloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      end module t_sph_FFTPACK5
