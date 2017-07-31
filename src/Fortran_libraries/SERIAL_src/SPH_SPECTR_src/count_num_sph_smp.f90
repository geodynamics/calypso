!>@file   count_num_sph_smp.f90
!!@brief  module count_num_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  SEt SMP parameters for spectr data
!!
!!@verbatim
!!      subroutine s_count_num_sph_smp                                  &
!!     &         (sph_rtp, sph_rtm, sph_rlm, sph_rj, ierr)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid),  intent(inout) :: sph_rj
!!@endverbatim
!!
!!@param  ierr  Error flag
!
      module count_num_sph_smp
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
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
      subroutine s_count_num_sph_smp                                    &
     &         (sph_rtp, sph_rtm, sph_rlm, sph_rj, ierr)
!
      use cal_minmax_and_stacks
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid),  intent(inout) :: sph_rj
      integer(kind = kint), intent(inout) :: ierr
!
      call count_num_rtp_smp(sph_rtp, ierr)
      call count_num_rtm_smp(sph_rtm, ierr)
      call count_num_rlm_smp(sph_rlm, ierr)
      call count_num_rj_smp(sph_rj, ierr)
!
      end subroutine s_count_num_sph_smp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rtp_smp(sph_rtp, ierr)
!
      use cal_minmax_and_stacks
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: num
!
      integer(kind = kint)  ::  maxnod_rtp_smp = 0
      integer(kind = kint)  ::  maxidx_rtp_smp(3) = (/0,0,0/)
!
!
      ierr = 0
      if(sph_rtp%nidx_rtp(1) .lt. np_smp) then
        ierr = 72
        return
      end if
!
      call alloc_rtp_param_smp(sph_rtp)
      call count_number_4_smp(np_smp, ione, sph_rtp%nnod_rtp,           &
     &    sph_rtp%istack_inod_rtp_smp, maxnod_rtp_smp)
!
      call count_number_4_smp(np_smp, ione, sph_rtp%nidx_rtp(1),        &
     &    sph_rtp%istack_rtp_kr_smp, maxidx_rtp_smp(1) )
      call count_number_4_smp(np_smp, ione, sph_rtp%nidx_rtp(2),        &
     &    sph_rtp%istack_rtp_lt_smp, maxidx_rtp_smp(2) )
      call count_number_4_smp(np_smp, ione, sph_rtp%nidx_rtp(3),        &
     &    sph_rtp%istack_rtp_mp_smp, maxidx_rtp_smp(3) )
!
      num = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
      call count_number_4_smp(np_smp, ione, num,                        &
     &    sph_rtp%istack_rtp_rt_smp, sph_rtp%maxirt_rtp_smp)
!
      end subroutine count_num_rtp_smp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rtm_smp(sph_rtm, ierr)
!
      use cal_minmax_and_stacks
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: num
!
      integer(kind = kint)  ::  maxnod_rtm_smp = 0
      integer(kind = kint) :: maxirt_rtm_smp =  0
!
      ierr = 0
      if(sph_rtm%nidx_rtm(1) .lt. np_smp) then
        ierr = 72
        return
      end if
!
      call alloc_rtm_param_smp(sph_rtm)
      call count_number_4_smp(np_smp, ione, sph_rtm%nnod_rtm,           &
     &    sph_rtm%istack_inod_rtm_smp, maxnod_rtm_smp)
!
      call count_number_4_smp(np_smp, ione, sph_rtm%nidx_rtm(1),        &
     &    sph_rtm%istack_rtm_kr_smp, sph_rtm%maxidx_rtm_smp(1))
      call count_number_4_smp(np_smp, ione, sph_rtm%nidx_rtm(2),        &
     &    sph_rtm%istack_rtm_lt_smp, sph_rtm%maxidx_rtm_smp(2))
      call count_number_4_smp(np_smp, ione, sph_rtm%nidx_rtm(3),        &
     &    sph_rtm%istack_rtm_m_smp,  sph_rtm%maxidx_rtm_smp(3))
!
      num = sph_rtm%nidx_rtm(1) * sph_rtm%nidx_rtm(2)
      call count_number_4_smp(np_smp, ione, num,                        &
     &    sph_rtm%istack_rtm_rt_smp, maxirt_rtm_smp )
!
      end subroutine count_num_rtm_smp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rlm_smp(sph_rlm, ierr)
!
      use cal_minmax_and_stacks
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint)  ::  maxnod_rlm_smp = 0
      integer(kind = kint)  ::  maxidx_rlm_smp(2) = (/0,0/)
!
!
      ierr = 0
      if(sph_rlm%nidx_rlm(1) .lt. np_smp) then
        ierr = 72
        return
      end if
!
      call alloc_rlm_param_smp(sph_rlm)
      call count_number_4_smp(np_smp, ione, sph_rlm%nnod_rlm,           &
     &    sph_rlm%istack_inod_rlm_smp, maxnod_rlm_smp)
!
      call count_number_4_smp(np_smp, ione, sph_rlm%nidx_rlm(1),        &
     &    sph_rlm%istack_rlm_kr_smp, maxidx_rlm_smp(1) )
      call count_number_4_smp(np_smp, ione, sph_rlm%nidx_rlm(2),        &
     &    sph_rlm%istack_rlm_j_smp, maxidx_rlm_smp(2) )
!
      end subroutine count_num_rlm_smp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rj_smp(sph_rj, ierr)
!
      use cal_minmax_and_stacks
!
      type(sph_rj_grid),  intent(inout) :: sph_rj
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint)  ::  maxnod_rj_smp =  0
      integer(kind = kint) :: maxidx_rj_smp(2) =  (/0,0/)
!
!
      ierr = 0
      if(sph_rj%nidx_rj(2) .lt. np_smp) then
        ierr = 72
        return
      end if
!
      call alloc_rj_param_smp(sph_rj)
      call count_number_4_smp(np_smp, ione, sph_rj%nnod_rj,             &
     &    sph_rj%istack_inod_rj_smp, maxnod_rj_smp)
!
      call count_number_4_smp(np_smp, ione, sph_rj%nidx_rj(1),          &
     &    sph_rj%istack_rj_kr_smp, maxidx_rj_smp(1) )
      call count_number_4_smp(np_smp, ione, sph_rj%nidx_rj(2),          &
     &    sph_rj%istack_rj_j_smp, maxidx_rj_smp(2) )
!
      end subroutine count_num_rj_smp
!
!  ---------------------------------------------------------------------
!
      end module count_num_sph_smp
