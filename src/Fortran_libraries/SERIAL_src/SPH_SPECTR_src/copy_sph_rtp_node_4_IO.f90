!>@file   copy_sph_rtp_node_4_IO.f90
!!@brief  module copy_sph_rtp_node_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr indices structure between IO buffer
!!
!!@verbatim
!!      subroutine copy_sph_node_4_rtp_from_IO(sph_IO, rtp, l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rtp_grid), intent(inout) :: rtp
!!        type(sph_IO_data), intent(in) :: sph_IO
!!      subroutine copy_sph_node_4_rtp_to_IO(l_truncation, rtp, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rtp_grid), intent(in) :: rtp
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      integer(kind = kint) function compare_sph_rtp_node_with_IO      &
!!     &                            (l_truncation, rtp, sph_IO)
!!        type(sph_rtp_grid), intent(in) :: rtp
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module copy_sph_rtp_node_4_IO
!
      use m_precision
!
      use m_constants
      use t_spheric_rtp_data
      use t_node_id_spherical_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rtp_from_IO(sph_IO, rtp, l_truncation)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: rtp
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
      rtp%irank_sph_rtp(1:ithree) = sph_IO%sph_rank(1:ithree)
!
      rtp%nidx_global_rtp(1:ithree) = sph_IO%nidx_gl_sph(1:ithree)
      l_truncation =                  sph_IO%ltr_gl
!
      rtp%nnod_rtp =           sph_IO%numnod_sph
      rtp%nidx_rtp(1:ithree) = sph_IO%nidx_sph(1:ithree)
      rtp%ist_rtp(1:ithree) =  sph_IO%ist_sph(1:ithree)
      rtp%ied_rtp(1:ithree) =  sph_IO%ied_sph(1:ithree)
      rtp%nnod_med =           sph_IO%nidx_sph(1)*sph_IO%nidx_sph(2)
!
      call alloc_spheric_param_rtp(rtp)
      call alloc_sph_1d_index_rtp(rtp)
!
      do i = 1, ithree
        rtp%idx_global_rtp(1:rtp%nnod_rtp,i)                            &
     &       = sph_IO%idx_gl_sph(1:rtp%nnod_rtp,i)
      end do
!
!$omp parallel workshare
      rtp%radius_1d_rtp_r(1:rtp%nidx_rtp(1))                            &
     &       =   sph_IO%r_gl_1(1:rtp%nidx_rtp(1))
      rtp%idx_gl_1d_rtp_r(1:rtp%nidx_rtp(1))                            &
     &       =   sph_IO%idx_gl_1(1:rtp%nidx_rtp(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      rtp%idx_gl_1d_rtp_t(1:rtp%nidx_rtp(2))                            &
     &       =   sph_IO%idx_gl_2(1:rtp%nidx_rtp(2),1)
!$omp end parallel workshare
!
!$omp parallel workshare
      rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),1)                          &
     &       = sph_IO%idx_gl_3(1:rtp%nidx_rtp(3),1)
      rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),2)                          &
     &       = sph_IO%idx_gl_3(1:rtp%nidx_rtp(3),2)
!$omp end parallel workshare
!
      end subroutine copy_sph_node_4_rtp_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rtp_to_IO(l_truncation, rtp, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(in) :: rtp
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8, nrt8
!
!
      sph_IO%numdir_sph =         ithree
      sph_IO%sph_rank(1:ithree) = rtp%irank_sph_rtp(1:ithree)
!
      sph_IO%ncomp_table_1d(1) = ione
      sph_IO%ncomp_table_1d(2) = ione
      sph_IO%ncomp_table_1d(3) = itwo
!
      sph_IO%nidx_gl_sph(1:ithree) = rtp%nidx_global_rtp(1:ithree)
      sph_IO%ltr_gl =                l_truncation
!
      sph_IO%numnod_sph =         rtp%nnod_rtp
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      sph_IO%nidx_sph(1:ithree) = rtp%nidx_rtp(1:ithree)
      sph_IO%ist_sph(1:ithree) =  rtp%ist_rtp(1:ithree)
      sph_IO%ied_sph(1:ithree) =  rtp%ied_rtp(1:ithree)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
!$omp parallel do private(i,nr_8,nrt8)
      do i = 1, rtp%nnod_rtp
        nr_8 = rtp%nidx_global_rtp(1)
        nrt8 = rtp%nidx_global_rtp(1)*rtp%nidx_global_rtp(2)
        sph_IO%idx_gl_sph(i,1) = rtp%idx_global_rtp(i,1)
        sph_IO%idx_gl_sph(i,2) = rtp%idx_global_rtp(i,2)
        sph_IO%idx_gl_sph(i,3) = rtp%idx_global_rtp(i,3)
        sph_IO%inod_gl_sph(i) = rtp%idx_global_rtp(i,1)                 &
     &                        + (rtp%idx_global_rtp(i,2) - 1) * nr_8    &
     &                        + (rtp%idx_global_rtp(i,3) - 1) * nrt8
      end do
!$omp end parallel do
!
!$omp parallel workshare
      sph_IO%r_gl_1(1:rtp%nidx_rtp(1))                                  &
     &        =     rtp%radius_1d_rtp_r(1:rtp%nidx_rtp(1))
      sph_IO%idx_gl_1(1:rtp%nidx_rtp(1))                                &
     &        =   rtp%idx_gl_1d_rtp_r(1:rtp%nidx_rtp(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_2(1:rtp%nidx_rtp(2),1)                              &
     &        = rtp%idx_gl_1d_rtp_t(1:rtp%nidx_rtp(2))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_3(1:rtp%nidx_rtp(3),1)                              &
     &        = rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),1)
      sph_IO%idx_gl_3(1:rtp%nidx_rtp(3),2)                              &
     &        = rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),2)
!$omp end parallel workshare
!
      end subroutine copy_sph_node_4_rtp_to_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_rtp_node_with_IO        &
     &                            (l_truncation, rtp, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(in) :: rtp
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      compare_sph_rtp_node_with_IO = 1
      do i = 1, ithree
        if(sph_IO%sph_rank(i) .ne. rtp%irank_sph_rtp(i)) return
      end do
!
      do i = 1, ithree
        if(sph_IO%nidx_gl_sph(i) .ne. rtp%nidx_global_rtp(i)) return
      end do
      if(sph_IO%ltr_gl .ne. l_truncation) return
!
      if(sph_IO%numnod_sph .ne. rtp%nnod_rtp) return
!
!
      do i = 1, ithree
        if(sph_IO%nidx_sph(i) .ne. rtp%nidx_rtp(i)) return
        if(sph_IO%ist_sph(i) .ne.  rtp%ist_rtp(i)) return
        if(sph_IO%ied_sph(i) .ne.  rtp%ied_rtp(i)) return
      end do
!
      do i = 1, rtp%nnod_rtp
        if(sph_IO%idx_gl_sph(i,1) .ne. rtp%idx_global_rtp(i,1)) return
        if(sph_IO%idx_gl_sph(i,2) .ne. rtp%idx_global_rtp(i,2)) return
        if(sph_IO%idx_gl_sph(i,3) .ne. rtp%idx_global_rtp(i,3)) return
      end do
!
      do i = 1, rtp%nidx_rtp(1)
        if(sph_IO%r_gl_1(i) .ne. rtp%radius_1d_rtp_r(i)) then
          if(abs(sph_IO%r_gl_1(i) - rtp%radius_1d_rtp_r(i))             &
     &       .gt. 1.0d-10) return
        end if
        if(sph_IO%idx_gl_1(i) .ne. rtp%idx_gl_1d_rtp_r(i)) return
      end do
!
      do i = 1, rtp%nidx_rtp(2)
        if(sph_IO%idx_gl_2(i,1) .ne. rtp%idx_gl_1d_rtp_t(i)) return
      end do
!
      do i = 1, rtp%nidx_rtp(3)
        if(sph_IO%idx_gl_3(i,1) .ne. rtp%idx_gl_1d_rtp_p(i,1)) return
        if(sph_IO%idx_gl_3(i,2) .ne. rtp%idx_gl_1d_rtp_p(i,2)) return
      end do
      compare_sph_rtp_node_with_IO = 0
!
      end function compare_sph_rtp_node_with_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_rtp_node_4_IO
