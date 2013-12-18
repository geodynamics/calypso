!>@file   sph_trans_vector.f90
!!@brief  module sph_trans_vector
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for vector
!!       and gradient of scalar
!!
!!@verbatim
!!      subroutine sph_b_trans_vector(nvector, nscalar, ntensor)
!!      subroutine sph_f_trans_vector(nvector, nscalar, ntensor)
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
!!@n @param  nvector  number of fields to be transformed
!
      module sph_trans_vector
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
      use merge_polidal_toroidal_v
      use spherical_SRs_N
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_vector(nvector, nscalar, ntensor)
!
      use m_work_time
      use select_legendre_transform
!
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: nfld_st, ncomp, ncomp_FFT
!
!
      nfld_st = nscalar + 6*ntensor
      ncomp = 3*nvector + nfld_st
      ncomp_FFT = ncomp*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp*irt_rtp_smp_stack(0:np_smp)
!
!      call check_sp_rj(my_rank, ncomp)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call send_recv_rj_2_rlm_N(ncomp, sp_rj, sp_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_sp_rlm(my_rank, ncomp)
!
      call start_eleps_time(22)
      if(nvector .gt. 0) then
        call sel_bwd_leg_trans_vector(ncomp, nvector)
      end if
      if(nfld_st .gt. 0) then
        call sel_bwd_leg_trans_scalar(ncomp, nvector, nfld_st)
      end if
      call end_eleps_time(22)
!
!      call check_vr_rtm(my_rank, ncomp)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call send_recv_rtm_2_rtp_N(ncomp, vr_rtm, vr_rtp)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_vr_rtp(my_rank, ncomp)
!
      call start_eleps_time(24)
      call backward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,            &
     &    nidx_rtp(3), vr_rtp)
      call end_eleps_time(24)
!
!      call check_vr_rtp(my_rank, ncomp)
!
!      call const_vect_sph_b_trans(nvector, vr_rtp)
!
      end subroutine sph_b_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_vector(nvector, nscalar, ntensor)
!
      use m_work_time
      use select_legendre_transform
!
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: ncomp, ncomp_FFT, nfld_st
!
!
      nfld_st = nscalar + 6*ntensor
      ncomp = 3*nvector + nfld_st
      ncomp_FFT = ncomp*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp*irt_rtp_smp_stack(0:np_smp)
!
!      call prod_r_vect_sph_f_trans(nvector, vr_rtp)
!
!      call check_vr_rtp(my_rank, ncomp)
      call start_eleps_time(24)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,             &
     &    nidx_rtp(3), vr_rtp)
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, ncomp)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call send_recv_rtp_2_rtm_N(ncomp, vr_rtp, vr_rtm)
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_vr_rtm(my_rank, ncomp)
!
      call start_eleps_time(23)
      if(nvector .gt. 0) then
        call sel_fwd_leg_trans_vector(ncomp, nvector)
      end if
      if(nfld_st .gt. 0) then
        call sel_fwd_leg_trans_scalar(ncomp, nvector, nfld_st)
      end if
      call end_eleps_time(23)
!      call check_sp_rlm(my_rank, ncomp)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call send_recv_rlm_2_rj_N(ncomp, sp_rlm, sp_rj)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, ncomp)
!
      end subroutine sph_f_trans_vector
!
! -----------------------------------------------------------------------
!
      end module sph_trans_vector
