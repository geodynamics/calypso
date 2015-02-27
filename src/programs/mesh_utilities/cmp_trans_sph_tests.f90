!cmp_trans_sph_tests.f90
!      module cmp_trans_sph_tests
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_real_sph_test(NB)
!      subroutine deallocate_real_sph_test
!
!      subroutine set_tesh_sph_elapsed_label
!      subroutine sph_transfer_test_1(itype)
!      subroutine sph_transfer_test_2(itype)
!      subroutine sph_transfer_test_3(itype)
!      subroutine sph_transfer_test_6(itype)
!      subroutine sph_transfer_test_N(NB, itype)
!      subroutine compare_transfer_sph_reals(NB, id_check)
!
      module cmp_trans_sph_tests
!
      use m_precision
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
      private :: set_transfer_sph_reals
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_real_sph_test(NB)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: NB
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
      subroutine sph_transfer_test_1(itype)
!
      use calypso_mpi
      use m_spheric_parameter
      use spherical_SRs
!
      integer(kind = kint), intent(in) ::itype
!
!
      iflag_sph_SR = itype
      call set_transfer_sph_reals(ione)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_1'
      call send_recv_rtp_2_rtm                                          &
     &    (X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_1'
      call send_recv_rtm_2_rtp                                          &
     &    (X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_1'
      call send_recv_rj_2_rlm                                           &
     &    (X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_1'
      call send_recv_rlm_2_rj                                           &
     &    (X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_1
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_2(itype)
!
      use calypso_mpi
      use m_spheric_parameter
      use spherical_SRs_2
!
      integer(kind = kint), intent(in) ::itype
!
!
      iflag_sph_SR2 = itype
      call set_transfer_sph_reals(itwo)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_2'
      call send_recv_rtp_2_rtm_2                                        &
     &    (X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_2'
      call send_recv_rtm_2_rtp_2                                        &
     &    (X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_2'
      call send_recv_rj_2_rlm_2                                         &
     &    (X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_2'
      call send_recv_rlm_2_rj_2                                         &
     &    (X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_2
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_3(itype)
!
      use calypso_mpi
      use m_spheric_parameter
      use spherical_SRs_3
!
      integer(kind = kint), intent(in) ::itype
!
!
      iflag_sph_SR3 = itype
      call set_transfer_sph_reals(ithree)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_3'
      call send_recv_rtp_2_rtm_3                                        &
     &    (X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_3'
      call send_recv_rtm_2_rtp_3                                        &
     &    (X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_3'
      call send_recv_rj_2_rlm_3                                         &
     &    (X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_3'
      call send_recv_rlm_2_rj_3                                         &
     &    (X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_3
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_6(itype)
!
      use calypso_mpi
      use m_spheric_parameter
      use spherical_SRs_6
!
      integer(kind = kint), intent(in) ::itype
!
!
      iflag_sph_SR6 = itype
      call set_transfer_sph_reals(isix)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_6'
      call send_recv_rtp_2_rtm_6                                        &
     &    (X_global_rtp(1), X_rtm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_6'
      call send_recv_rtm_2_rtp_6                                        &
     &    (X_global_rtm(1), X_rtp_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_6'
      call send_recv_rj_2_rlm_6                                         &
     &    (X_global_rj(1), X_rlm_recieve(1) )
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_6'
      call send_recv_rlm_2_rj_6                                         &
     &    (X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_6
!
! -----------------------------------------------------------------------
!
      subroutine sph_transfer_test_N(NB, itype)
!
      use calypso_mpi
      use m_spheric_parameter
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: NB, itype
!
!
      iflag_sph_SRN = itype
      call set_transfer_sph_reals(NB)
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_N'
      call send_recv_rtp_2_rtm_N                                        &
     &    (NB, X_global_rtp(1), X_rtm_recieve(1) )
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_N'
      call send_recv_rtm_2_rtp_N                                        &
     &    (NB, X_global_rtm(1), X_rtp_recieve(1) )
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_N'
      call send_recv_rj_2_rlm_N                                         &
     &    (NB, X_global_rj(1), X_rlm_recieve(1) )
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_N'
      call send_recv_rlm_2_rj_N                                         &
     &    (NB, X_global_rlm(1), X_rj_recieve(1) )
!
      end subroutine sph_transfer_test_N
!
! -----------------------------------------------------------------------
!
      subroutine set_transfer_sph_reals(NB)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: NB
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
      end subroutine set_transfer_sph_reals
!
! -----------------------------------------------------------------------
!
      subroutine compare_transfer_sph_reals(NB, id_check)
!
      use m_spheric_parameter
      use cmp_trans_sph_indices
!
      integer(kind = kint), intent(in) :: NB, id_check
!
      integer(kind = kint) :: inod, nd
      real(kind = kreal) :: diff
!
!
      write(id_check,*) 'Wrong commnication in rtm => rtp with ', NB
      do inod = 1, nnod_rtp
        if(   idx_rtp_recieve(inod,1) .eq. 0                            &
     &   .or. idx_rtp_recieve(inod,2) .eq. 0                            &
     &   .or. idx_rtp_recieve(inod,3) .eq. 0) then
          do nd = 1, NB
            diff = diff + abs(X_rtp_recieve(NB*inod-NB+nd)              &
     &                      - X_global_rtp(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(4i16,1p3E23.15)') inod,                    &
     &       idx_global_rtp(inod,1:3), diff,                            &
     &       X_rtp_recieve(NB*inod-NB+1), X_global_rtp(NB*inod-NB+1)
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rtp => rtm with ', NB
      do inod = 1, nnod_rtm
        if(   idx_rtm_recieve(inod,1) .eq. 0                            &
     &   .or. idx_rtm_recieve(inod,2) .eq. 0                            &
     &   .or. idx_rtm_recieve(inod,3) .eq. 0) then
          do nd = 1, NB
            diff = diff + abs(X_rtm_recieve(NB*inod-NB+nd)              &
     &                      - X_global_rtm(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(4i16,1pE23.15)') inod,                     &
     &          idx_global_rtm(inod,1:3), diff
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rj => rlm with ', NB
      do inod = 1, nnod_rlm
        if(      idx_rlm_recieve(inod,1) .ge. 0                         &
     &      .or. idx_rlm_recieve(inod,2) .ge. 0) then
          diff = 0.0d0
          do nd = 1, NB
            diff = diff + abs(X_rlm_recieve(NB*inod-NB+nd)              &
     &                      - X_global_rlm(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(3i16,1pE23.15)') inod,                     &
     &          idx_global_rlm(inod,1:2), diff
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rlm => rj with ', NB
      do inod = 1, nnod_rj
        if(      idx_rj_recieve(inod,1) .ge. 0                          &
     &      .or. idx_rj_recieve(inod,2) .ge. 0) then
          diff = 0.0d0
          do nd = 1, NB
            diff = diff + abs(X_rj_recieve(NB*inod-NB+nd)               &
     &                      - X_global_rj(NB*inod-NB+nd))
          end do
          if (diff .gt. 1.0E-11) then
            write(id_check,'(3i16,1p3E23.15)') inod,                    &
     &          idx_global_rj(inod,1:2), diff
          end if
        end if
      end do
!
      end subroutine compare_transfer_sph_reals
!
! -----------------------------------------------------------------------
!
      end module cmp_trans_sph_tests
