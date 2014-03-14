!>@file   sph_trans_scalar.f90
!!@brief  module sph_trans_scalar
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for scalar
!!       and symmetric tensor
!!
!!@verbatim
!!      subroutine sph_b_trans_scalar(ncomp_trans)
!!      subroutine sph_f_trans_scalar(ncomp_trans)
!!
!!   input /outpt arrays
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!
      module sph_trans_scalar
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_phys_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use FFT_selector
      use legendre_transform_select
      use spherical_SRs_N
      use calypso_mpi
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_scalar(ncomp_trans)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: ncomp_FFT
!
!
      ncomp_FFT = ncomp_trans*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp_trans*irt_rtp_smp_stack(0:np_smp)
      vr_rtp(1:ncomp_trans*nnod_rtp) = 0.0d0
!
!      call check_sp_rj(my_rank, ncomp_trans)
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj(1), sp_rlm(1))
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_sp_rlm(my_rank, ncomp_trans)
      call start_eleps_time(22)
      call sel_scalar_bwd_legendre_trans(ncomp_trans, izero,            &
     &    ncomp_trans)
      call end_eleps_time(22)
!
!      call check_vr_rtm(my_rank, ncomp_trans)
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call send_recv_rtm_2_rtp_N(ncomp_trans, vr_rtm(1), vr_rtp(1))
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_vr_rtp(my_rank, ncomp_trans)
      call start_eleps_time(24)
      call backward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,            &
     &    nidx_rtp(3), vr_rtp(1))
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      end subroutine sph_b_trans_scalar
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_scalar(ncomp_trans)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: ncomp_FFT
!
!
      Nstacksmp(0:np_smp) = ncomp_trans*irt_rtp_smp_stack(0:np_smp)
      ncomp_FFT = ncomp_trans*nidx_rtp(1)*nidx_rtp(2)
!
!
!      call check_vr_rtp(my_rank, ncomp_trans)
      call start_eleps_time(24)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,             &
     &    nidx_rtp(3), vr_rtp(1))
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call send_recv_rtp_2_rtm_N(ncomp_trans, vr_rtp(1), vr_rtm(1))
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_vr_rtm(my_rank, ncomp_trans)
!
      call start_eleps_time(23)
      call sel_scalar_fwd_legendre_trans(ncomp_trans, izero,            &
     &    ncomp_trans)
      call end_eleps_time(23)
!      call check_sp_rlm(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call send_recv_rlm_2_rj_N(ncomp_trans, sp_rlm(1), sp_rj(1))
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, ncomp_trans)
!
      end subroutine sph_f_trans_scalar
!
! -----------------------------------------------------------------------
!
      end module sph_trans_scalar
