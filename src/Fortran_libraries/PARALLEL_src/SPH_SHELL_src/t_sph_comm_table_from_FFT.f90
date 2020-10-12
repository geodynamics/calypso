!>@file   t_sph_comm_table_from_FFT.f90
!!@brief  module t_sph_comm_table_from_FFT
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!      subroutine alloc_comm_table_sph_FFT(ntot_sr_rtp, comm_sph_FFT)
!!      subroutine dealloc_comm_table_sph_FFT(comm_sph_FFT)
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!!
!!      subroutine copy_all_rtp_FFT_to_send_smp                         &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,     &
!!     &          X_FFT, comm_sph_FFT, n_WS, WS)
!!      subroutine copy_1comp_rtp_FFT_to_send_smp                       &
!!     &         (nd, nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd, &
!!     &          X_FFT, comm_sph_FFT, n_WS, WS)
!!      subroutine copy_all_rtp_FFT_to_send(nnod_rtp, irt_rtp_smp_stack,&
!!     &          ncomp_fwd, X_FFT, comm_sph_FFT, n_WS, WS)
!!      subroutine copy_1comp_rtp_FFT_to_send                           &
!!     &         (nd, nnod_rtp, irt_rtp_smp_stack, ncomp_fwd,           &
!!     &          X_FFT, comm_sph_FFT, n_WS, WS)
!!        type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!!
!!      subroutine copy_all_prt_FFT_to_send(nnod_rtp, nidx_rtp,         &
!!     &          ncomp_fwd, X_FFT, comm_sph_FFT, n_WS, WS)
!!      subroutine copy_1comp_prt_FFT_to_send(nd, nnod_rtp, nidx_rtp,   &
!!     &          ncomp_fwd, X_FFT, comm_sph_FFT, n_WS, WS)
!!@endverbatim
!!
      module t_sph_comm_table_from_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use calypso_mpi
!
      implicit none
!
!>      Structure of communication table from FFT to send buffer
      type comm_tbl_from_FFT
!         Number of table
        integer(kind = kint) :: ntot_item
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: ip_smp_fft(:)
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: kl_fft(:)
!         longitudinal address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: m_fft(:)
!         Normalization on FFTPACK data from SEND buffer 
        real(kind = kreal), allocatable :: rnorm_sr_rtp(:)
      end type comm_tbl_from_FFT
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine alloc_comm_table_sph_FFT(ntot_sr_rtp, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      comm_sph_FFT%ntot_item = ntot_sr_rtp
      allocate(comm_sph_FFT%ip_smp_fft(ntot_sr_rtp))
      allocate(comm_sph_FFT%kl_fft(ntot_sr_rtp))
      allocate(comm_sph_FFT%m_fft(ntot_sr_rtp))
      allocate(comm_sph_FFT%rnorm_sr_rtp(ntot_sr_rtp))
!
      end subroutine alloc_comm_table_sph_FFT
!
! ------------------------------------------------------------------
!
      subroutine dealloc_comm_table_sph_FFT(comm_sph_FFT)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      deallocate(comm_sph_FFT%kl_fft, comm_sph_FFT%m_fft)
      deallocate(comm_sph_FFT%ip_smp_fft, comm_sph_FFT%rnorm_sr_rtp)
!
      end subroutine dealloc_comm_table_sph_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_all_rtp_FFT_to_send_smp                           &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,       &
     &          X_FFT, comm_sph_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, nd, num
      integer(kind = kint) :: inod_c, ic_send, ist_fft, ip
!
!
!$omp parallel do private(inum,ip,num,ist_fft,inod_fft,                 &
!$omp&                    nd,ic_send,inod_c)
      do inum = 1, comm_sph_FFT%ntot_item
        ip =  comm_sph_FFT%ip_smp_fft(inum)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
        inod_fft = comm_sph_FFT%kl_fft(inum)                            &
     &            + (comm_sph_FFT%m_fft(inum)-1)*num
!
        ic_send = (inum-1) * ncomp_fwd
        inod_c =  (inod_fft+ist_fft-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &         = comm_sph_FFT%rnorm_sr_rtp(inum)                        &
     &            * X_FFT(inod_c+1:inod_c+ncomp_fwd)
      end do
!$end parallel do
!
      end subroutine copy_all_rtp_FFT_to_send_smp
!
! ------------------------------------------------------------------
!
      subroutine copy_1comp_rtp_FFT_to_send_smp                         &
     &         (nd, nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,   &
     &          X_FFT, comm_sph_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, num
      integer(kind = kint) :: ic_send, ist_fft, ip
!
!
!$omp parallel do private(inum,ip,num,ist_fft,inod_fft,ic_send)
      do inum = 1, comm_sph_FFT%ntot_item
        ip =  comm_sph_FFT%ip_smp_fft(inum)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
        inod_fft = comm_sph_FFT%kl_fft(inum)                            &
     &            + (comm_sph_FFT%m_fft(inum)-1)*num + ist_fft
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send) = comm_sph_FFT%rnorm_sr_rtp(inum) * X_FFT(inod_fft)
      end do
!$end parallel do
!
      end subroutine copy_1comp_rtp_FFT_to_send_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_all_rtp_FFT_to_send(nnod_rtp, irt_rtp_smp_stack,  &
     &          ncomp_fwd, X_FFT, comm_sph_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, inod_c, ic_send
!
!
!$omp parallel do private(inum,inod_fft,ic_send,inod_c)
      do inum = 1, comm_sph_FFT%ntot_item
        inod_fft = comm_sph_FFT%kl_fft(inum)                            &
     &         + (comm_sph_FFT%m_fft(inum)-1)*irt_rtp_smp_stack(np_smp)
!
        ic_send = (inum-1) * ncomp_fwd
        inod_c =  (inod_fft-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &           = comm_sph_FFT%rnorm_sr_rtp(inum)                      &
     &            * X_FFT(inod_c+1:inod_c+ncomp_fwd)
      end do
!$end parallel do
!
      end subroutine copy_all_rtp_FFT_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_1comp_rtp_FFT_to_send                             &
     &         (nd, nnod_rtp, irt_rtp_smp_stack, ncomp_fwd,             &
     &          X_FFT, comm_sph_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, ic_send
!
!
!$omp parallel do private(inum,inod_fft,ic_send)
      do inum = 1, comm_sph_FFT%ntot_item
        inod_fft = comm_sph_FFT%kl_fft(inum)                            &
     &         + (comm_sph_FFT%m_fft(inum)-1)*irt_rtp_smp_stack(np_smp)
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send) = comm_sph_FFT%rnorm_sr_rtp(inum) * X_FFT(inod_fft)
      end do
!$end parallel do
!
      end subroutine copy_1comp_rtp_FFT_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_all_prt_FFT_to_send(nnod_rtp, nidx_rtp,           &
     &          ncomp_fwd, X_FFT, comm_sph_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(nnod_rtp,ncomp_fwd)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, ic_send
!
!
!$omp parallel do private(inum,inod_fft,ic_send)
      do inum = 1, comm_sph_FFT%ntot_item
        inod_fft = comm_sph_FFT%m_fft(inum)                             &
     &            + (comm_sph_FFT%kl_fft(inum)-1) * nidx_rtp(3)
        ic_send = (inum-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &         = comm_sph_FFT%rnorm_sr_rtp(inum)                        &
     &          * X_FFT(inod_fft,1:ncomp_fwd)
      end do
!$end parallel do
!
      end subroutine copy_all_prt_FFT_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_1comp_prt_FFT_to_send(nd, nnod_rtp, nidx_rtp,     &
     &          ncomp_fwd, X_FFT, comm_sph_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, ic_send
!
!
!$omp parallel do private(inum,inod_fft,ic_send)
      do inum = 1, comm_sph_FFT%ntot_item
        inod_fft = comm_sph_FFT%m_fft(inum)                             &
     &            + (comm_sph_FFT%kl_fft(inum)-1) * nidx_rtp(3)
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send) = comm_sph_FFT%rnorm_sr_rtp(inum) * X_FFT(inod_fft)
      end do
!$end parallel do
!
      end subroutine copy_1comp_prt_FFT_to_send
!
! ------------------------------------------------------------------
!
      end module t_sph_comm_table_from_FFT
