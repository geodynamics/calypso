!>@file   sph_transforms.f90
!!@brief  module sph_transforms
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for vector
!!       and gradient of scalar
!!
!!@verbatim
!!      subroutine sph_backward_transforms                              &
!!     &         (ncomp_trans, nvector, nscalar, ntensor)
!!      subroutine sph_forward_transforms                               &
!!     &         (ncomp_trans, nvector, nscalar, ntensor)
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
      module sph_transforms
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
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_backward_transforms                                &
     &         (ncomp_trans, nvector, nscalar, ntensor)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: ncomp_FFT, nscalar_trans
!
!
      nscalar_trans = nscalar + 6*ntensor
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
      call start_eleps_time(22)
      if(iflag_debug .gt. 0) write(*,*) 'sel_backward_legendre_trans'
      call sel_backward_legendre_trans                                  &
     &   (ncomp_trans, nvector, nscalar_trans)
      call end_eleps_time(22)
!
!      call check_vr_rtm(my_rank, ncomp_trans)
!
      call finish_send_recv_rj_2_rlm
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
      call finish_send_recv_rtm_2_rtp
!
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      end subroutine sph_backward_transforms
!
! -----------------------------------------------------------------------
!
      subroutine sph_forward_transforms                                 &
     &         (ncomp_trans, nvector, nscalar, ntensor)
!
      use m_work_time
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
!
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: ncomp_FFT, nscalar_trans
!
!
      nscalar_trans = nscalar + 6*ntensor
      ncomp_FFT = ncomp_trans*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ncomp_trans*irt_rtp_smp_stack(0:np_smp)
!
!      call check_vr_rtp(my_rank, ncomp_trans)
      call start_eleps_time(24)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,             &
     &     nidx_rtp(3), vr_rtp)
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
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (ncomp_trans, nvector, nscalar_trans)
      call end_eleps_time(23)
!      call check_sp_rlm(my_rank, ncomp_trans)
!
      call finish_send_recv_rtp_2_rtm
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call send_recv_rlm_2_rj_N(ncomp_trans, sp_rlm, sp_rj)
      call finish_send_recv_rlm_2_rj
!
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, ncomp_trans)
!
      end subroutine sph_forward_transforms
!
! -----------------------------------------------------------------------
!
      end module sph_transforms
