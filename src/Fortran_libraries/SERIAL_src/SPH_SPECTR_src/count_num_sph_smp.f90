!>@file   count_num_sph_smp.f90
!!@brief  module count_num_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  
!!
!!@verbatim
!!      subroutine s_count_num_sph_smp(ierr)
!!@endverbatim
!!
!!@param  ierr  Error flag
!
      module count_num_sph_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Error message
      character(len=kchara), parameter :: e_message_Rsmp                &
     &  = '# of r-grid for SPH trans. should be more than SMP threads'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_count_num_sph_smp(ierr)
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint) :: num
!
!
      ierr = 0
      if(nidx_rtm(1) .lt. np_smp) then
        ierr = 72
        return
      end if
!
      call allocate_sph_param_smp
!
      call count_number_4_smp(np_smp, ione, nnod_rtp,                   &
     &    inod_rtp_smp_stack, maxnod_rtp_smp)
      call count_number_4_smp(np_smp, ione, nnod_rtm,                   &
     &    inod_rtm_smp_stack, maxnod_rtm_smp)
      call count_number_4_smp(np_smp, ione, nnod_rlm,                   &
     &    inod_rlm_smp_stack, maxnod_rlm_smp)
      call count_number_4_smp(np_smp, ione, nnod_rj,                    &
     &    inod_rj_smp_stack, maxnod_rj_smp)
!
      call count_number_4_smp(np_smp, ione, nidx_rtp(1),                &
     &    idx_rtp_smp_stack(0,1), maxidx_rtp_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rtp(2),                &
     &    idx_rtp_smp_stack(0,2), maxidx_rtp_smp(2) )
      call count_number_4_smp(np_smp, ione, nidx_rtp(3),                &
     &    idx_rtp_smp_stack(0,3), maxidx_rtp_smp(3) )
!
      call count_number_4_smp(np_smp, ione, nidx_rtm(1),                &
     &    idx_rtm_smp_stack(0,1), maxidx_rtm_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rtm(2),                &
     &    idx_rtm_smp_stack(0,2), maxidx_rtm_smp(2) )
      call count_number_4_smp(np_smp, ione, nidx_rtm(3),                &
     &    idx_rtm_smp_stack(0,3), maxidx_rtm_smp(3) )
!
      call count_number_4_smp(np_smp, ione, nidx_rlm(1),                &
     &    idx_rlm_smp_stack(0,1), maxidx_rlm_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rlm(2),                &
     &    idx_rlm_smp_stack(0,2), maxidx_rlm_smp(2) )
!
      call count_number_4_smp(np_smp, ione, nidx_rj(1),                 &
     &    idx_rj_smp_stack(0,1), maxidx_rj_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rj(2),                 &
     &    idx_rj_smp_stack(0,2), maxidx_rj_smp(2) )
!
      num = nidx_rtp(1)*nidx_rtp(2)
      call count_number_4_smp(np_smp, ione, num,                        &
     &    irt_rtp_smp_stack, maxirt_rtp_smp )
!
      num = nidx_rtm(1)*nidx_rtm(2)
      call count_number_4_smp(np_smp, ione, num,                        &
     &    irt_rtm_smp_stack, maxirt_rtm_smp )
!
      end subroutine s_count_num_sph_smp
!
!  ---------------------------------------------------------------------
!
      end module count_num_sph_smp
