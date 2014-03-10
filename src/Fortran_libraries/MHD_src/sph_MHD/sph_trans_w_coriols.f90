!>@file   sph_trans_w_coriols.f90
!!@brief  module sph_trans_w_coriols
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform
!!       including Coriolis terms
!!
!!@verbatim
!!      subroutine sph_b_trans_w_coriolis(ncomp_trans)
!!      subroutine sph_f_trans_w_coriolis(ncomp_trans)
!!
!!      subroutine sph_b_trans_licv(ncomp_trans)
!!      subroutine sph_f_trans_licv(ncomp_trans)
!!
!!   input /outpt arrays for single field
!!
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(3*i_rtp  )
!!
!!     forward transform: 
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!     backward transform: 
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!   input /outpt arrays for single field
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(3*i_rtp  )
!!      Scalar spectr:         sp_rj(i_rj)
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!
      module sph_trans_w_coriols
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use FFT_selector
      use legendre_transform_select
      use merge_polidal_toroidal_v
      use spherical_SRs_N
      use const_coriolis_sph_rlm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_w_coriolis(ncomp_trans)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: nvector, ncomp_FFT
!
!
      nvector = ncomp_trans / 3
      ncomp_FFT = ncomp_trans*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp_trans*irt_rtp_smp_stack(0:np_smp)
!
!      call check_sp_rj(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj, sp_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_sp_rlm(my_rank, ncomp_trans)
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      call sum_coriolis_rlm(ncomp_trans, sp_rlm)
      call end_eleps_time(13)
!
      call start_eleps_time(22)
      if(iflag_debug .gt. 0) write(*,*) 'sel_vector_bwd_legendre_trans'
      call sel_vector_bwd_legendre_trans(ncomp_trans, nvector)
      call end_eleps_time(22)
!
!      call check_vr_rtm(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call send_recv_rtm_2_rtp_N(ncomp_trans, vr_rtm, vr_rtp)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      call start_eleps_time(24)
      call backward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,            &
     &    nidx_rtp(3), vr_rtp)
      call end_eleps_time(24)
!
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      end subroutine sph_b_trans_w_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_w_coriolis(ncomp_trans)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: nvector, ncomp_FFT
!
!
      nvector = ncomp_trans / 3
      ncomp_FFT = ncomp_trans*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp_trans*irt_rtp_smp_stack(0:np_smp)
!
!      call check_vr_rtp(my_rank, ncomp_trans)
      call start_eleps_time(24)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,             &
     &    nidx_rtp(3), vr_rtp)
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call send_recv_rtp_2_rtm_N(ncomp_trans, vr_rtp, vr_rtm)
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_vr_rtm(my_rank, ncomp_trans)
!
      call start_eleps_time(23)
      if(iflag_debug .gt. 0) write(*,*) 'sel_vector_fwd_legendre_trans'
      call sel_vector_fwd_legendre_trans(ncomp_trans, nvector)
      call end_eleps_time(23)
!      call check_sp_rlm(my_rank, ncomp_trans)
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm(ncomp_trans, sp_rlm)
      call end_eleps_time(13)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call send_recv_rlm_2_rj_N(ncomp_trans, sp_rlm, sp_rj)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, ncomp_trans)
!
      end subroutine sph_f_trans_w_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_licv(ncomp_trans)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
!      call check_sp_rj(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj, sp_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_sp_rlm(my_rank, ncomp_trans)
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      call sum_coriolis_rlm(ncomp_trans, sp_rlm)
      call end_eleps_time(13)
!
      end subroutine sph_b_trans_licv
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_licv(ncomp_trans)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm(ncomp_trans, sp_rlm)
      call end_eleps_time(24)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call send_recv_rlm_2_rj_N(ncomp_trans, sp_rlm, sp_rj)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, ncomp_trans)
!
      end subroutine sph_f_trans_licv
!
! -----------------------------------------------------------------------
!
      end module sph_trans_w_coriols
