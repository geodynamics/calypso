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
!!      subroutine sph_b_trans_scalar(nb)
!!      subroutine sph_f_trans_scalar(nb)
!!
!!   input /outpt arrays
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!
!!      subroutine sph_f_trans_tensor(nb)
!!      subroutine sph_b_trans_tensor(nb)
!!
!!   input /outpt arrays
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
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
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
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
      subroutine sph_b_trans_scalar(nb)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: np, ncomp
!
!
      np =    nidx_rtp(3)
      ncomp = nb*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = nb*irt_rtp_smp_stack(0:np_smp)
      vr_rtp(1:nb*nnod_rtp) = 0.0d0
!
!      call check_sp_rj(my_rank, nb)
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call send_recv_rj_2_rlm_N(nb, sp_rj(1), sp_rlm(1))
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_sp_rlm(my_rank, nb)
      call start_eleps_time(22)
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_bwd_trans_scalar_spin(nb)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_bwd_trans_scalar_krin(nb)
      else
        call leg_bwd_trans_scalar_org(nb)
      end if
      call end_eleps_time(22)
!
!      call check_vr_rtm(my_rank, nb)
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call send_recv_rtm_2_rtp_N(nb, vr_rtm(1), vr_rtp(1))
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_vr_rtp(my_rank, nb)
      call start_eleps_time(24)
      call backward_FFT_select(np_smp, Nstacksmp, ncomp, np, vr_rtp(1))
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, nb)
!
      end subroutine sph_b_trans_scalar
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_scalar(nb)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: nb
!
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: np, ncomp
!
!
      vr_rtm(1:nb*nnod_rtm) =      0.0d0
      Nstacksmp(0:np_smp) = nb*irt_rtp_smp_stack(0:np_smp)
!
      np =    nidx_rtp(3)
      ncomp = nb*nidx_rtp(1)*nidx_rtp(2)
!
!
!      call check_vr_rtp(my_rank, nb)
      call start_eleps_time(24)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp, np, vr_rtp(1))
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, nb)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call send_recv_rtp_2_rtm_N(nb, vr_rtp(1), vr_rtm(1))
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_vr_rtm(my_rank, nb)
!
      call start_eleps_time(23)
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_fwd_trans_scalar_spin(nb)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_fwd_trans_scalar_krin(nb)
      else
        call leg_fwd_trans_scalar_org(nb)
      end if
      call end_eleps_time(23)
!      call check_sp_rlm(my_rank, nb)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call send_recv_rlm_2_rj_N(nb, sp_rlm(1), sp_rj(1))
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, nb)
!
      end subroutine sph_f_trans_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_tensor(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: num
!
!
      num = n_sym_tensor * nb
      call sph_b_trans_scalar(num)
!
      end subroutine sph_b_trans_tensor
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_tensor(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: num
!
!
      num = n_sym_tensor * nb
      call sph_f_trans_scalar(num)
!
      end subroutine sph_f_trans_tensor
!
! -----------------------------------------------------------------------
!
      end module sph_trans_scalar
