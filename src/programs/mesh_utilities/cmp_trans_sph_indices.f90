!>@file   cmp_trans_sph_indices.f90
!!@brief  module cmp_trans_sph_indices
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for vector
!!       and gradient of scalar
!!
!!@verbatim
!!      subroutine allocate_idx_sph_recieve                             &
!!     &          (nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj)
!!      subroutine deallocate_idx_sph_recieve
!!
!!      subroutine sph_type_indices_transfer(iflag_recv, sph, comms_sph,&
!!     &                                     SR_sig, SR_i)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!      integer(kind = kint) function compare_transfer_sph_indices      &
!!     &                                                (id_check, sph)
!!      integer(kind = kint) function check_missing_sph_indices         &
!!     &                                               (id_file, sph)
!!        type(sph_grids), intent(in) :: sph
!!@endverbatim
!
      module cmp_trans_sph_indices
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
      use t_solver_SR_int
!
      implicit none
!
      integer(kind = kint), allocatable :: idx_rtp_recieve(:,:)
      integer(kind = kint), allocatable :: idx_rtm_recieve(:,:)
      integer(kind = kint), allocatable :: idx_rlm_recieve(:,:)
      integer(kind = kint), allocatable :: idx_rj_recieve(:,:)
!
      private :: sph_indices_transfer
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_recieve                               &
     &          (nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
!
      allocate( idx_rtp_recieve(nnod_rtp,3) )
      allocate( idx_rtm_recieve(nnod_rtm,3) )
      allocate( idx_rlm_recieve(nnod_rlm,2) )
      allocate( idx_rj_recieve(nnod_rj,2) )
!
      idx_rtp_recieve = 0
      idx_rtm_recieve = 0
      idx_rlm_recieve = 0
      idx_rj_recieve =  0
!
      end subroutine allocate_idx_sph_recieve
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_recieve
!
      deallocate(idx_rtp_recieve)
      deallocate(idx_rtm_recieve)
      deallocate(idx_rlm_recieve)
      deallocate(idx_rj_recieve )
!
      end subroutine deallocate_idx_sph_recieve
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_type_indices_transfer(iflag_recv, sph, comms_sph,  &
     &                                     SR_sig, SR_i)
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      integer(kind = kint), intent(in) :: iflag_recv
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call sph_indices_transfer(iflag_recv, comms_sph,                  &
     &    sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,                   &
     &    sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                     &
     &    sph%sph_rtp%idx_global_rtp, sph%sph_rtm%idx_global_rtm,       &
     &    sph%sph_rlm%idx_global_rlm, sph%sph_rj%idx_global_rj,         &
     &    SR_sig, SR_i)
!
      end subroutine sph_type_indices_transfer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_indices_transfer(iflag_recv, comms_sph,            &
     &           nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                 &
     &           idx_global_rtp, idx_global_rtm,                        &
     &           idx_global_rlm, idx_global_rj, SR_sig, SR_i)
!
      use spherical_SRs_N
      use sel_spherical_SRs
      use t_sph_trans_comm_tbl
!
      type(sph_comm_tables), intent(in) :: comms_sph
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
      integer(kind = kint), intent(in) :: idx_global_rtp(nnod_rtp,3)
      integer(kind = kint), intent(in) :: idx_global_rtm(nnod_rtm,3)
      integer(kind = kint), intent(in) :: idx_global_rlm(nnod_rlm,2)
      integer(kind = kint), intent(in) :: idx_global_rj(nnod_rj,2)
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      if (my_rank .eq. 0) write(*,*) 'integer comm. for rtp => rtm'
      call send_recv_sph_trans_int(iflag_recv,                          &
     &    nnod_rtp, nnod_rtm, comms_sph%comm_rtp, comms_sph%comm_rtm,   &
     &    idx_global_rtp(1,1), idx_rtm_recieve(1,1), SR_sig, SR_i)
      call send_recv_sph_trans_int(iflag_recv,                          &
     &    nnod_rtp, nnod_rtm, comms_sph%comm_rtp, comms_sph%comm_rtm,   &
     &    idx_global_rtp(1,2), idx_rtm_recieve(1,2), SR_sig, SR_i)
      call send_recv_sph_trans_int(iflag_recv,                          &
     &    nnod_rtp, nnod_rtm, comms_sph%comm_rtp, comms_sph%comm_rtm,   &
     &    idx_global_rtp(1,3), idx_rtm_recieve(1,3), SR_sig, SR_i)
!
      if (my_rank .eq. 0) write(*,*) 'integer comm. for rtm => rtp'
      call send_recv_sph_trans_int(iflag_recv,                          &
     &    nnod_rtm, nnod_rtp, comms_sph%comm_rtm, comms_sph%comm_rtp,   &
     &    idx_global_rtm(1,1), idx_rtp_recieve(1,1), SR_sig, SR_i)
      call send_recv_sph_trans_int(iflag_recv,                          &
     &    nnod_rtm, nnod_rtp, comms_sph%comm_rtm, comms_sph%comm_rtp,   &
     &    idx_global_rtm(1,2), idx_rtp_recieve(1,2), SR_sig, SR_i)
      call send_recv_sph_trans_int(iflag_recv,                          &
     &    nnod_rtm, nnod_rtp, comms_sph%comm_rtm, comms_sph%comm_rtp,   &
     &    idx_global_rtm(1,3), idx_rtp_recieve(1,3), SR_sig, SR_i)
!
      if (my_rank .eq. 0) write(*,*) 'integer comm. for rj => rlm'
      idx_rlm_recieve = -1
      call send_recv_sph_trans_int(iflag_recv, nnod_rj, nnod_rlm,       &
     &   comms_sph%comm_rj, comms_sph%comm_rlm,                         &
     &   idx_global_rj(1,1), idx_rlm_recieve(1,1), SR_sig, SR_i)
      call send_recv_sph_trans_int(iflag_recv, nnod_rj, nnod_rlm,       &
     &   comms_sph%comm_rj, comms_sph%comm_rlm,                         &
     &   idx_global_rj(1,2), idx_rlm_recieve(1,2), SR_sig, SR_i)
!
      if (my_rank .eq. 0) write(*,*) 'integer comm. for rlm => rj'
      idx_rj_recieve = -1
      call send_recv_sph_trans_int(iflag_recv, nnod_rlm, nnod_rj,       &
     &   comms_sph%comm_rlm, comms_sph%comm_rj,                         &
     &   idx_global_rlm(1,1), idx_rj_recieve(1,1), SR_sig, SR_i)
      call send_recv_sph_trans_int(iflag_recv, nnod_rlm, nnod_rj,       &
     &   comms_sph%comm_rlm, comms_sph%comm_rj,                         &
     &   idx_global_rlm(1,2), idx_rj_recieve(1,2), SR_sig, SR_i)
!
      end subroutine sph_indices_transfer
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_transfer_sph_indices        &
     &                                                (id_check, sph)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_check
      type(sph_grids), intent(in) :: sph
!
      integer(kind = kint) :: inod, mphi
      logical :: flag
!
!
      compare_transfer_sph_indices = 0
      write(id_check,*) 'Wrong commnication in rtm => rtp'
      do inod = 1, sph%sph_rtp%nnod_rtp
        mphi = sph%sph_rtp%idx_global_rtp(inod,3)
        if(abs(sph%sph_rtp%idx_gl_1d_rtp_p(mphi,3))                     &
     &            .gt. sph%sph_params%l_truncation) cycle
        flag =    (idx_rtp_recieve(inod,1)                              &
     &              .ne. sph%sph_rtp%idx_global_rtp(inod,1))            &
     &      .and. (idx_rtp_recieve(inod,2)                              &
     &              .ne. sph%sph_rtp%idx_global_rtp(inod,2))            &
     &      .and. (idx_rtp_recieve(inod,3)                              &
     &              .ne. sph%sph_rtp%idx_global_rtp(inod,3))            &
     &      .and. (idx_rtp_recieve(inod,1) .ne. 0)                      &
     &      .and. (idx_rtp_recieve(inod,2) .ne. 0)                      &
     &      .and. (idx_rtp_recieve(inod,3) .ne. 0)
            if(flag) then
              write(id_check,'(i16,6i5)') inod,                         &
     &          sph%sph_rtp%idx_global_rtp(inod,1:3),                   &
     &          idx_rtp_recieve(inod,1:3)
          compare_transfer_sph_indices = 1
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rtp => rtm'
      do inod = 1, sph%sph_rtm%nnod_rtm
        flag =    (idx_rtm_recieve(inod,1)                              &
     &              .ne. sph%sph_rtm%idx_global_rtm(inod,1))            &
     &      .and. (idx_rtm_recieve(inod,2)                              &
     &              .ne. sph%sph_rtm%idx_global_rtm(inod,2))            &
     &      .and. (idx_rtm_recieve(inod,3)                              &
     &              .ne. sph%sph_rtm%idx_global_rtm(inod,3))            &
     &      .and. (idx_rtm_recieve(inod,1) .ne. 0)                      &
     &      .and. (idx_rtm_recieve(inod,2) .ne. 0)                      &
     &      .and. (idx_rtm_recieve(inod,3) .ne. 0)
            if(flag) then
              write(id_check,'(i16,6i5)') inod,                         &
     &          sph%sph_rtm%idx_global_rtm(inod,1:3),                   &
     &          idx_rtm_recieve(inod,1:3)
          compare_transfer_sph_indices = 1
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rj => rlm'
      do inod = 1, sph%sph_rlm%nnod_rlm
        flag =    (idx_rlm_recieve(inod,1)                              &
     &              .ne. sph%sph_rlm%idx_global_rlm(inod,1))            &
     &      .and. (idx_rlm_recieve(inod,2)                              &
     &              .ne. sph%sph_rlm%idx_global_rlm(inod,2))            &
     &      .and. (idx_rlm_recieve(inod,1) .ne. 0)                      &
     &      .and. (idx_rlm_recieve(inod,2) .ne. 0)
        if(flag) then
            write(id_check,'(i16,6i5)') inod,                           &
     &        sph%sph_rlm%idx_global_rlm(inod,1:2),                     &
     &        idx_rlm_recieve(inod,1:2)
          compare_transfer_sph_indices = 1
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rlm => rj'
      do inod = 1, sph%sph_rj%nnod_rj
        flag =    (idx_rj_recieve(inod,1)                               &
     &              .ne. sph%sph_rj%idx_global_rj(inod,1))              &
     &      .and. (idx_rj_recieve(inod,2)                               &
     &              .ne. sph%sph_rj%idx_global_rj(inod,2))              &
     &      .and. (idx_rj_recieve(inod,1) .ne. 0)                       &
     &      .and. (idx_rj_recieve(inod,2) .ne. 0)
        if(flag) then
          if(inod .eq. sph%sph_rj%inod_rj_center) then
            write(*,*) 'Find center at', inod, 'in domain ', my_rank
          else
            write(id_check,'(i16,6i5)') inod,                           &
     &        sph%sph_rj%idx_global_rj(inod,1:2),                       &
     &        idx_rj_recieve(inod,1:2)
            compare_transfer_sph_indices = 1
          end if
        end if
      end do
!
      end function compare_transfer_sph_indices
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function check_missing_sph_indices           &
     &                                                  (id_file, sph)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_grids), intent(in) :: sph
!
      integer(kind = kint) :: inod, kr, lth, mphi, j
      integer(kind = kint) :: iflag_alias, iflag_alias_gl
!
!
      iflag_alias_gl = 0
      check_missing_sph_indices = 0
      write(id_file,*) 'No commnication data in rtm => rtp'
      write(id_file,*)                                                  &
     &   'local_id, global_id, global_r, global_t, global_p, global_m'
      do mphi = 1, sph%sph_rtp%nidx_rtp(3)
        do lth = 1, sph%sph_rtp%nidx_rtp(2)
          do kr = 1, sph%sph_rtp%nidx_rtp(1)
            inod = 1 + sph%sph_rtp%istep_rtp(1) * (kr - 1)              &
     &               + sph%sph_rtp%istep_rtp(2) * (lth - 1)             &
     &               + sph%sph_rtp%istep_rtp(3) * (mphi - 1)
            if(   idx_rtp_recieve(inod,1) .eq. 0                        &
     &       .or. idx_rtp_recieve(inod,2) .eq. 0                        &
     &       .or. idx_rtp_recieve(inod,3) .eq. 0) then
              if(abs(sph%sph_rtp%idx_gl_1d_rtp_p(mphi,2))               &
     &            .gt. sph%sph_params%l_truncation) then
                iflag_alias_gl = 1
              else
                write(id_file,'(4i16,6i5)') inod,                       &
     &             sph%sph_rtp%idx_global_rtp(inod,1:3),                &
     &             sph%sph_rtp%idx_gl_1d_rtp_r(kr),                     &
     &             sph%sph_rtp%idx_gl_1d_rtp_t(lth),                    &
     &             sph%sph_rtp%idx_gl_1d_rtp_p(mphi,1:2)
                check_missing_sph_indices = 1
              end if
            end if
          end do
        end do
      end do
!
      write(id_file,*) 'No commnication in rtp => rtm'
      write(id_file,*)                                                  &
     &         'local_id, global_id, global_r, global_t, global_m'
      do mphi = 1, sph%sph_rtm%nidx_rtm(3)
        do kr = 1, sph%sph_rtm%nidx_rtm(1)
          do lth = 1, sph%sph_rtm%nidx_rtm(2)
            inod =  1 + (kr-1) *   sph%sph_rtm%istep_rtm(1)             &
&                     + (lth-1) *  sph%sph_rtm%istep_rtm(2)             &
     &                + (mphi-1) * sph%sph_rtm%istep_rtm(3)
            if(   idx_rtm_recieve(inod,1) .eq. 0                        &
     &       .or. idx_rtm_recieve(inod,2) .eq. 0                        &
     &       .or. idx_rtm_recieve(inod,3) .eq. 0) then
              write(id_file,'(5i16,6i5)') inod,                         &
     &             sph%sph_rtm%idx_global_rtm(inod,1:3),                &
     &             sph%sph_rtm%idx_gl_1d_rtm_r(kr),                     &
     &             sph%sph_rtm%idx_gl_1d_rtm_t(lth),                    &
     &             sph%sph_rtm%idx_gl_1d_rtm_m(mphi,1:2)
              check_missing_sph_indices = 1
            end if
          end do
        end do
      end do
!
      write(id_file,*) 'No commnication in rj => rlm'
      write(id_file,*)                                                  &
     &     'local_id, global_r, global_j, global_r, global_l, global_m'
      do kr = 1, sph%sph_rlm%nidx_rlm(1)
        do j = 1, sph%sph_rlm%nidx_rlm(2)
          inod = 1 + (kr-1) * sph%sph_rlm%istep_rlm(1)                  &
     &             + (j-1) *  sph%sph_rlm%istep_rlm(2)
          if(      idx_rlm_recieve(inod,1) .lt. 0                       &
     &        .or. idx_rlm_recieve(inod,2) .lt. 0) then
              write(id_file,'(4i16,6i5)') inod,                         &
     &        sph%sph_rlm%idx_global_rlm(inod,1:2),                     &
     &        sph%sph_rlm%idx_gl_1d_rlm_r(kr),                          &
     &        sph%sph_rlm%idx_gl_1d_rlm_j(j,2:3)
              check_missing_sph_indices = 1
          end if
        end do
      end do
!
      write(id_file,*) 'No commnication in rlm => rj'
      write(id_file,*)                                                  &
     &     'local_id, global_r, global_j, global_r, global_l, global_m'
      do kr = 1, sph%sph_rj%nidx_rj(1)
        do j = 1, sph%sph_rj%nidx_rj(2)
          inod = 1 + (kr-1) * sph%sph_rj%istep_rj(1)                    &
     &             + (j-1) *  sph%sph_rj%istep_rj(2)
          if(      idx_rj_recieve(inod,1) .lt. 0                        &
     &        .or. idx_rj_recieve(inod,2) .lt. 0) then
              write(id_file,'(4i16,6i5)') inod,                         &
     &          sph%sph_rj%idx_global_rj(inod,1:2),                     &
     &          sph%sph_rj%idx_gl_1d_rj_r(kr),                          &
     &          sph%sph_rj%idx_gl_1d_rj_j(j,2:3)
              check_missing_sph_indices = 1
          end if
        end do
      end do
!
      if(iflag_alias_gl .gt. 0) write(*,*)                             &
     &          'Skip data for de-aliasing in domain ', my_rank
!
      end function check_missing_sph_indices
!
! -----------------------------------------------------------------------
!
      end module cmp_trans_sph_indices
