!cmp_trans_sph_tests.f90
!      module cmp_trans_sph_tests
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine allocate_real_sph_test                               &
!!     &         (NB, nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj)
!      subroutine deallocate_real_sph_test
!
!!      subroutine set_tesh_sph_elapsed_label
!!      subroutine sph_transfer_test_1(itype, sph, comms_sph)
!!      subroutine sph_transfer_test_2(itype, sph, comms_sph)
!!      subroutine sph_transfer_test_3(itype, sph, comms_sph)
!!      subroutine sph_transfer_test_6(itype, sph, comms_sph)
!!      subroutine sph_transfer_test_N(NB, itype, sph, comms_sph)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!      subroutine compare_transfer_sph_reals(NB, id_check, sph)
!!        type(sph_grids), intent(in) :: sph
!
      module cmp_trans_sph_tests
!
      use m_precision
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!
      implicit none
!
      real(kind = kreal), allocatable :: X_global_rtp(:)
      real(kind = kreal), allocatable :: X_global_rtm(:)
      real(kind = kreal), allocatable :: X_global_rlm(:)
      real(kind = kreal), allocatable :: X_global_rj(:)
!
      real(kind = kreal), allocatable :: X_rtp_recieve(:)
      real(kind = kreal), allocatable :: X_rtm_recieve(:)
      real(kind = kreal), allocatable :: X_rlm_recieve(:)
      real(kind = kreal), allocatable :: X_rj_recieve(:)
!
      private :: set_transfer_sph_reals, set_real_4_sph_transfer_test
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_real_sph_test                                 &
     &         (NB, nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
!
!
      allocate( X_global_rtp(NB*nnod_rtp) )
      allocate( X_global_rtm(NB*nnod_rtm) )
      allocate( X_global_rlm(NB*nnod_rlm) )
      allocate( X_global_rj(NB*nnod_rj) )
      allocate( X_rtp_recieve(NB*nnod_rtp) )
      allocate( X_rtm_recieve(NB*nnod_rtm) )
      allocate( X_rlm_recieve(NB*nnod_rlm) )
      allocate( X_rj_recieve(NB*nnod_rj) )
!
      X_global_rtp = 0.0d0
      X_global_rtm = 0.0d0
      X_global_rlm = 0.0d0
      X_global_rj =  0.0d0
      X_rtp_recieve = 0.0d0
      X_rtm_recieve = 0.0d0
      X_rlm_recieve = 0.0d0
      X_rj_recieve =  0.0d0
!
      end subroutine allocate_real_sph_test
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_real_sph_test
!
      deallocate(X_global_rtp, X_global_rtm)
      deallocate(X_global_rlm, X_global_rj )
      deallocate(X_rtp_recieve, X_rtm_recieve)
      deallocate(X_rlm_recieve, X_rj_recieve )
!
      end subroutine deallocate_real_sph_test
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_tesh_sph_elapsed_label
!
      use m_work_time
!
      integer(kind = kint) :: i
!
!
      num_elapsed = 39
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                 '
      elapse_labels(2) = 'Initialization time        '
      elapse_labels(3) = 'Time evolution loop time   '
      elapse_labels(4) = 'Data IO time               '
      elapse_labels(5) = 'Evolution excluding IO     '
!
      do i = 6, 35
        elapse_labels(i) = 'unused    '
      end do
!
      elapse_labels(36) = 'set_to_send_buf_N    '
      elapse_labels(37) = 'calypso_send_recv_core    '
      elapse_labels(38) = 'set_from_recv_buf_rev_N    '
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
      end subroutine set_tesh_sph_elapsed_label
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_1(itype, sph, comms_sph)
!
      use calypso_mpi
      use m_sel_spherical_SRs
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) ::itype
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      iflag_sph_SR = itype
      call set_transfer_sph_reals                                       &
     &   (ione, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_1'
      call send_recv_sph_trans                                          &
     &   (sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,                   &
     &   comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_1'
      call send_recv_sph_trans                                          &
     &   (sph%sph_rtm%nnod_rtm, sph%sph_rtp%nnod_rtp,                   &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp,                       &
     &    X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_1'
      call send_recv_sph_trans                                          &
     &   (sph%sph_rj%nnod_rj, sph%sph_rlm%nnod_rlm,                     &
     &    comms_sph%comm_rj, comms_sph%comm_rlm,                        &
     &    X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_1'
      call send_recv_sph_trans                                          &
     &   (sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                     &
     &    comms_sph%comm_rlm, comms_sph%comm_rj,                        &
     &    X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_1
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_2(itype, sph, comms_sph)
!
      use calypso_mpi
      use m_sel_spherical_SRs
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) ::itype
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      iflag_sph_SR2 = itype
      call set_transfer_sph_reals                                       &
     &   (itwo, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_2'
      call send_recv_sph_trans_2                                        &
     &   (sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,                   &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_2'
      call send_recv_sph_trans_2                                        &
     &   (sph%sph_rtm%nnod_rtm, sph%sph_rtp%nnod_rtp,                   &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp,                       &
     &    X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_2'
      call send_recv_sph_trans_2                                        &
     &   (sph%sph_rj%nnod_rj, sph%sph_rlm%nnod_rlm,                     &
     &    comms_sph%comm_rj, comms_sph%comm_rlm,                        &
     &    X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_2'
      call send_recv_sph_trans_2                                        &
     &   (sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                     &
     &    comms_sph%comm_rlm, comms_sph%comm_rj,                        &
     &    X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_2
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_3(itype, sph, comms_sph)
!
      use calypso_mpi
      use m_sel_spherical_SRs
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) ::itype
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      iflag_sph_SR3 = itype
      call set_transfer_sph_reals                                       &
     &   (ithree, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_3'
      call send_recv_sph_trans_3                                        &
     &   (sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,                   &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_3'
      call send_recv_sph_trans_3                                        &
     &   (sph%sph_rtm%nnod_rtm, sph%sph_rtp%nnod_rtp,                   &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp,                       &
     &    X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_3'
      call send_recv_sph_trans_3                                        &
     &   (sph%sph_rj%nnod_rj, sph%sph_rlm%nnod_rlm,                     &
     &    comms_sph%comm_rj, comms_sph%comm_rlm,                        &
     &    X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_3'
      call send_recv_sph_trans_3                                        &
     &   (sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                     &
     &    comms_sph%comm_rlm, comms_sph%comm_rj,                        &
     &    X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_3
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_6(itype, sph, comms_sph)
!
      use calypso_mpi
      use m_sel_spherical_SRs
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) ::itype
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      iflag_sph_SR6 = itype
      call set_transfer_sph_reals                                       &
     &   (isix, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_6'
      call send_recv_sph_trans_6                                        &
     &   (sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,                   &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_6'
      call send_recv_sph_trans_6                                        &
     &   (sph%sph_rtm%nnod_rtm, sph%sph_rtp%nnod_rtp,                   &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp,                       &
     &    X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_6'
      call send_recv_sph_trans_6                                        &
     &   (sph%sph_rj%nnod_rj,  sph%sph_rlm%nnod_rlm,                    &
     &    comms_sph%comm_rj, comms_sph%comm_rlm,                        &
     &    X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_6'
      call send_recv_sph_trans_6                                        &
     &   (sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                     &
     &    comms_sph%comm_rlm, comms_sph%comm_rj,                        &
     &    X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_6
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_N(NB, itype, sph, comms_sph)
!
      use calypso_mpi
      use m_sel_spherical_SRs
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: NB, itype
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      iflag_sph_SRN = itype
      call set_transfer_sph_reals                                       &
     &   (NB, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_N'
      call send_recv_sph_trans_N                                        &
     &   (NB, sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,               &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    X_global_rtp(1), X_rtm_recieve(1))
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_N'
      call send_recv_sph_trans_N                                        &
     &   (NB, sph%sph_rtm%nnod_rtm, sph%sph_rtp%nnod_rtp,               &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp,                       &
     &    X_global_rtm(1), X_rtp_recieve(1))
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_N'
      call send_recv_sph_trans_N                                        &
     &   (NB, sph%sph_rj%nnod_rj, sph%sph_rlm%nnod_rlm,                 &
     &    comms_sph%comm_rj, comms_sph%comm_rlm,                        &
     &    X_global_rj(1), X_rlm_recieve(1))
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_N'
      call send_recv_sph_trans_N                                        &
     &   (NB, sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj,                        &
     &    X_global_rlm(1), X_rj_recieve(1))
!
      end subroutine sph_transfer_test_N
!
! -----------------------------------------------------------------------
!
      subroutine set_transfer_sph_reals                                 &
     &         (NB, sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      integer(kind = kint), intent(in) :: NB
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
!
!
      call set_real_4_sph_transfer_test                                 &
     &   (NB, sph_rtp%nnod_rtp, sph_rtm%nnod_rtm,                       &
     &    sph_rlm%nnod_rlm, sph_rj%nnod_rj,                             &
     &    sph_rtp%idx_global_rtp, sph_rtm%idx_global_rtm,               &
     &    sph_rlm%idx_global_rlm, sph_rj%idx_global_rj)
!
      end subroutine set_transfer_sph_reals
!
! -----------------------------------------------------------------------
!
      subroutine set_real_4_sph_transfer_test                           &
     &          (NB, nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,             &
     &           idx_global_rtp, idx_global_rtm,                        &
     &           idx_global_rlm, idx_global_rj)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
      integer(kind = kint), intent(in) :: idx_global_rtp(nnod_rtp,3)
      integer(kind = kint), intent(in) :: idx_global_rtm(nnod_rtm,3)
      integer(kind = kint), intent(in) :: idx_global_rlm(nnod_rlm,2)
      integer(kind = kint), intent(in) :: idx_global_rj(nnod_rj,2)
!
      integer(kind = kint) :: inod, nd, k
!
!
      do inod = 1, nnod_rtp
        do nd = 1, NB
          k = mod(nd-1,3) + 1
          X_global_rtp(NB*inod-NB+nd) = dble(nd*idx_global_rtp(inod,k))
        end do
      end do
!
      do inod = 1, nnod_rtm
        do nd = 1, NB
          k = mod(nd-1,3) + 1
          X_global_rtm(NB*inod-NB+nd) = dble(nd*idx_global_rtm(inod,k))
        end do
      end do
!
      do inod = 1, nnod_rlm
        do nd = 1, NB
          k = mod(nd-1,2) + 1
          X_global_rlm(NB*inod-NB+nd) = dble(nd*idx_global_rlm(inod,k))
        end do
      end do
!
      do inod = 1, nnod_rj
        do nd = 1, NB
          k = mod(nd-1,2) + 1
          X_global_rj(NB*inod-NB+nd) = dble(nd*idx_global_rj(inod,k))
        end do
      end do
!
      end subroutine set_real_4_sph_transfer_test
!
! -----------------------------------------------------------------------
!
      subroutine compare_transfer_sph_reals(NB, id_check, sph)
!
      use cmp_trans_sph_indices
!
      integer(kind = kint), intent(in) :: NB, id_check
      type(sph_grids), intent(in) :: sph
!
!
      integer(kind = kint) :: inod, nd
      real(kind = kreal) :: diff
!
!
      write(id_check,*) 'Wrong commnication in rtm => rtp with ', NB
      do inod = 1, sph%sph_rtp%nnod_rtp
        if(   idx_rtp_recieve(inod,1) .eq. 0                            &
     &   .or. idx_rtp_recieve(inod,2) .eq. 0                            &
     &   .or. idx_rtp_recieve(inod,3) .eq. 0) then
          do nd = 1, NB
            diff = diff + abs(X_rtp_recieve(NB*inod-NB+nd)              &
     &                      - X_global_rtp(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(4i16,1p3E23.15)') inod,                    &
     &       sph%sph_rtp%idx_global_rtp(inod,1:3), diff,                &
     &       X_rtp_recieve(NB*inod-NB+1), X_global_rtp(NB*inod-NB+1)
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rtp => rtm with ', NB
      do inod = 1, sph%sph_rtm%nnod_rtm
        if(   idx_rtm_recieve(inod,1) .eq. 0                            &
     &   .or. idx_rtm_recieve(inod,2) .eq. 0                            &
     &   .or. idx_rtm_recieve(inod,3) .eq. 0) then
          do nd = 1, NB
            diff = diff + abs(X_rtm_recieve(NB*inod-NB+nd)              &
     &                      - X_global_rtm(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(4i16,1pE23.15)') inod,                     &
     &          sph%sph_rtm%idx_global_rtm(inod,1:3), diff
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rj => rlm with ', NB
      do inod = 1, sph%sph_rlm%nnod_rlm
        if(      idx_rlm_recieve(inod,1) .ge. 0                         &
     &      .or. idx_rlm_recieve(inod,2) .ge. 0) then
          diff = 0.0d0
          do nd = 1, NB
            diff = diff + abs(X_rlm_recieve(NB*inod-NB+nd)              &
     &                      - X_global_rlm(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(3i16,1pE23.15)') inod,                     &
     &          sph%sph_rlm%idx_global_rlm(inod,1:2), diff
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rlm => rj with ', NB
      do inod = 1, sph%sph_rj%nnod_rj
        if(      idx_rj_recieve(inod,1) .ge. 0                          &
     &      .or. idx_rj_recieve(inod,2) .ge. 0) then
          diff = 0.0d0
          do nd = 1, NB
            diff = diff + abs(X_rj_recieve(NB*inod-NB+nd)               &
     &                      - X_global_rj(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(3i16,1p3E23.15)') inod,                    &
     &          sph%sph_rj%idx_global_rj(inod,1:2), diff
          end if
        end if
      end do
!
      end subroutine compare_transfer_sph_reals
!
! -----------------------------------------------------------------------
!
      end module cmp_trans_sph_tests
