!
!      module copy_sph_node_type_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine copy_sph_node_type_rtp_from_IO(l_truncation, rtp)
!        integer(kind = kint), intent(inout) :: l_truncation
!        type(sph_rtp_grid), intent(inout) :: rtp
!      subroutine copy_sph_node_type_rtm_from_IO(l_truncation, rtm)
!        integer(kind = kint), intent(inout) :: l_truncation
!        type(sph_rtm_grid), intent(inout) :: rtm
!      subroutine copy_sph_node_type_rlm_from_IO(l_truncation, rlm)
!        integer(kind = kint), intent(inout) :: l_truncation
!        type(sph_rlm_grid), intent(inout) :: rlm
!      subroutine copy_sph_node_type_rj_from_IO(l_truncation, rj)
!        integer(kind = kint), intent(inout) :: l_truncation
!        type(sph_rj_grid), intent(inout) :: rj
!
!      subroutine copy_sph_node_type_rtp_to_IO(l_truncation, rtp)
!        integer(kind = kint), intent(in) :: l_truncation
!        type(sph_rtp_grid), intent(inout) :: rtp
!      subroutine copy_sph_node_type_rtm_to_IO(l_truncation, rtm)
!        integer(kind = kint), intent(in) :: l_truncation
!        type(sph_rtm_grid), intent(inout) :: rtm
!      subroutine copy_sph_node_type_rlm_to_IO(l_truncation, rlm)
!        integer(kind = kint), intent(in) :: l_truncation
!        type(sph_rlm_grid), intent(inout) :: rlm
!      subroutine copy_sph_node_type_rj_to_IO(l_truncation, rj)
!        integer(kind = kint), intent(in) :: l_truncation
!        type(sph_rj_grid), intent(inout) :: rj
!
      module copy_sph_node_type_IO
!
      use m_precision
!
      use m_constants
      use t_spheric_parameter
      use m_node_id_spherical_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rtp_from_IO(l_truncation, rtp)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: rtp
      integer(kind = kint) :: i
!
      rtp%sph_rank_rtp(1:ithree) =    sph_rank_IO(1:ithree)
!
      rtp%nidx_global_rtp(1:ithree) = nidx_gl_sph_IO(1:ithree)
      l_truncation =              ltr_gl_IO
!
      rtp%nnod_rtp =      nnod_sph_IO
      rtp%nnod_rtp_pole = nnod_sph_IO
      rtp%nidx_rtp(1:ithree) = nidx_sph_IO(1:ithree)
      rtp%ist_rtp(1:ithree) =  ist_sph_IO(1:ithree)
      rtp%ied_rtp(1:ithree) =  ied_sph_IO(1:ithree)
!
      call alloc_type_spheric_param_rtp(rtp)
      call alloc_type_sph_1d_index_rtp(rtp)
!
      rtp%inod_global_rtp(1:rtp%nnod_rtp)                               &
     &       = inod_gl_sph_IO(1:rtp%nnod_rtp)
      do i = 1, ithree
        rtp%idx_global_rtp(1:rtp%nnod_rtp,i)                            &
     &       = idx_gl_sph_IO(1:rtp%nnod_rtp,i)
      end do
!
      rtp%radius_1d_rtp_r(1:rtp%nidx_rtp(1))                            &
     &       =   r_gl_1_IO(1:rtp%nidx_rtp(1))
      rtp%idx_gl_1d_rtp_r(1:rtp%nidx_rtp(1))                            &
     &       =   idx_gl_1_IO(1:rtp%nidx_rtp(1))
      rtp%idx_gl_1d_rtp_t(1:rtp%nidx_rtp(2))                            &
     &       =   idx_gl_2_IO(1:rtp%nidx_rtp(2),1)
      rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),1)                          &
     &       = idx_gl_3_IO(1:rtp%nidx_rtp(3),1)
      rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),2)                          &
     &       = idx_gl_3_IO(1:rtp%nidx_rtp(3),2)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine copy_sph_node_type_rtp_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rtm_from_IO(l_truncation, rtm)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: rtm
      integer(kind = kint) :: i
!
      rtm%sph_rank_rtm(1:ithree) =    sph_rank_IO(1:ithree)
!
      rtm%nidx_global_rtm(1:ithree) = nidx_gl_sph_IO(1:ithree)
      l_truncation =              ltr_gl_IO
!
      rtm%nnod_rtm = nnod_sph_IO
      rtm%nidx_rtm(1:ithree) = nidx_sph_IO(1:ithree)
      rtm%ist_rtm(1:ithree) =  ist_sph_IO(1:ithree)
      rtm%ied_rtm(1:ithree) =  ied_sph_IO(1:ithree)
!
      call alloc_type_spheric_param_rtm(rtm)
      call alloc_type_sph_1d_index_rtm(rtm)
!
      rtm%inod_global_rtm(1:rtm%nnod_rtm)                               &
     &      = inod_gl_sph_IO(1:rtm%nnod_rtm)
      do i = 1, ithree
        rtm%idx_global_rtm(1:rtm%nnod_rtm,i)                            &
     &      = idx_gl_sph_IO(1:rtm%nnod_rtm,i)
      end do
!
      rtm%radius_1d_rtm_r(1:rtm%nidx_rtm(1))                            &
     &      =   r_gl_1_IO(1:rtm%nidx_rtm(1))
      rtm%idx_gl_1d_rtm_r(1:rtm%nidx_rtm(1))                            &
     &      =   idx_gl_1_IO(1:rtm%nidx_rtm(1))
      rtm%idx_gl_1d_rtm_t(1:rtm%nidx_rtm(2))                            &
     &      =   idx_gl_2_IO(1:rtm%nidx_rtm(2),1)
      rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),1)                          &
     &      = idx_gl_3_IO(1:rtm%nidx_rtm(3),1)
      rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),2)                          &
     &      = idx_gl_3_IO(1:rtm%nidx_rtm(3),2)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine copy_sph_node_type_rtm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rlm_from_IO(l_truncation, rlm)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: rlm
      integer(kind = kint) :: i
!
      rlm%sph_rank_rlm(1:itwo) =    sph_rank_IO(1:itwo)
!
      rlm%nidx_global_rlm(1:itwo) = nidx_gl_sph_IO(1:itwo)
      l_truncation =              ltr_gl_IO
!
      rlm%nnod_rlm = nnod_sph_IO
      rlm%nidx_rlm(1:itwo) = nidx_sph_IO(1:itwo)
      rlm%ist_rlm(1:itwo) =  ist_sph_IO(1:itwo)
      rlm%ied_rlm(1:itwo) =  ied_sph_IO(1:itwo)
!
      call alloc_type_spheric_param_rlm(rlm)
      call alloc_type_sph_1d_index_rlm(rlm)
!
      rlm%inod_global_rlm(1:rlm%nnod_rlm)                               &
     &      = inod_gl_sph_IO(1:rlm%nnod_rlm)
      do i = 1, itwo
        rlm%idx_global_rlm(1:rlm%nnod_rlm,i)                            &
     &      = idx_gl_sph_IO(1:rlm%nnod_rlm,i)
      end do
!
      rlm%radius_1d_rlm_r(1:rlm%nidx_rlm(1))                            &
     &       =   r_gl_1_IO(1:rlm%nidx_rlm(1))
      rlm%idx_gl_1d_rlm_r(1:rlm%nidx_rlm(1))                            &
     &       =   idx_gl_1_IO(1:rlm%nidx_rlm(1))
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),1)                          &
     &       = idx_gl_2_IO(1:rlm%nidx_rlm(2),1)
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),2)                          &
     &       = idx_gl_2_IO(1:rlm%nidx_rlm(2),2)
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),3)                          &
     &       = idx_gl_2_IO(1:rlm%nidx_rlm(2),3)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine copy_sph_node_type_rlm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rj_from_IO(l_truncation, rj)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid), intent(inout) :: rj
      integer(kind = kint) :: i
!
      rj%sph_rank_rj(1:itwo) =     sph_rank_IO(1:itwo)
!
      rj%nidx_global_rj(1:itwo) = nidx_gl_sph_IO(1:itwo)
      l_truncation =            ltr_gl_IO
!
      rj%nnod_rj = nnod_sph_IO
      rj%nidx_rj(1:itwo) = nidx_sph_IO(1:itwo)
      rj%ist_rj(1:itwo) =  ist_sph_IO(1:itwo)
      rj%ied_rj(1:itwo) =  ied_sph_IO(1:itwo)
!
      call alloc_type_spheric_param_rj(rj)
      call alloc_type_sph_1d_index_rj(rj)
!
      rj%inod_global_rj(1:rj%nnod_rj) = inod_gl_sph_IO(1:rj%nnod_rj)
      do i = 1, itwo
        rj%idx_global_rj(1:rj%nnod_rj,i)                                &
     &       = idx_gl_sph_IO(1:rj%nnod_rj,i)
      end do
!
      rj%radius_1d_rj_r(1:rj%nidx_rj(1)) = r_gl_1_IO(1:rj%nidx_rj(1))
      rj%idx_gl_1d_rj_r(1:rj%nidx_rj(1)) = idx_gl_1_IO(1:rj%nidx_rj(1))
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),1)                              &
     &       = idx_gl_2_IO(1:rj%nidx_rj(2),1)
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),2)                              &
     &       = idx_gl_2_IO(1:rj%nidx_rj(2),2)
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),3)                              &
     &      = idx_gl_2_IO(1:rj%nidx_rj(2),3)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine copy_sph_node_type_rj_from_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rtp_to_IO(l_truncation, rtp)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: rtp
      integer(kind = kint) :: i
!
      ndir_sph_IO =              ithree
      sph_rank_IO(1:ithree) =    rtp%sph_rank_rtp(1:ithree)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ione
      ncomp_itbl_1d_IO(3) = itwo
!
      nidx_gl_sph_IO(1:ithree) = rtp%nidx_global_rtp(1:ithree)
      ltr_gl_IO =                l_truncation
!
      nnod_sph_IO = rtp%nnod_rtp
      nidx_sph_IO(1:ithree) = rtp%nidx_rtp(1:ithree)
      ist_sph_IO(1:ithree) =  rtp%ist_rtp(1:ithree)
      ied_sph_IO(1:ithree) =  rtp%ied_rtp(1:ithree)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      inod_gl_sph_IO(1:rtp%nnod_rtp)                                    &
     &        = rtp%inod_global_rtp(1:rtp%nnod_rtp)
      do i = 1, ithree
        idx_gl_sph_IO(1:rtp%nnod_rtp,i)                                 &
     &        = rtp%idx_global_rtp(1:rtp%nnod_rtp,i)
      end do
!
      r_gl_1_IO(1:rtp%nidx_rtp(1))                                      &
     &        =     rtp%radius_1d_rtp_r(1:rtp%nidx_rtp(1))
      idx_gl_1_IO(1:rtp%nidx_rtp(1))                                    &
     &        =   rtp%idx_gl_1d_rtp_r(1:rtp%nidx_rtp(1))
      idx_gl_2_IO(1:rtp%nidx_rtp(2),1)                                  &
     &        = rtp%idx_gl_1d_rtp_t(1:rtp%nidx_rtp(2))
      idx_gl_3_IO(1:rtp%nidx_rtp(3),1)                                  &
     &        = rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),1)
      idx_gl_3_IO(1:rtp%nidx_rtp(3),2)                                  &
     &        = rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),2)
!
      call dealloc_type_sph_1d_index_rtp(rtp)
      call dealloc_type_spheric_param_rtp(rtp)
!
      end subroutine copy_sph_node_type_rtp_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rtm_to_IO(l_truncation, rtm)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: rtm
      integer(kind = kint) :: i
!
      ndir_sph_IO =              ithree
      sph_rank_IO(1:ithree) =    rtm%sph_rank_rtm(1:ithree)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ione
      ncomp_itbl_1d_IO(3) = itwo
!
      nidx_gl_sph_IO(1:ithree) = rtm%nidx_global_rtm(1:ithree)
      ltr_gl_IO =                l_truncation
!
      nnod_sph_IO = rtm%nnod_rtm
      nidx_sph_IO(1:ithree) = rtm%nidx_rtm(1:ithree)
      ist_sph_IO(1:ithree) =  rtm%ist_rtm(1:ithree)
      ied_sph_IO(1:ithree) =  rtm%ied_rtm(1:ithree)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      inod_gl_sph_IO(1:rtm%nnod_rtm)                                    &
     &      = rtm%inod_global_rtm(1:rtm%nnod_rtm)
      do i = 1, ithree
        idx_gl_sph_IO(1:rtm%nnod_rtm,i)                                 &
     &      = rtm%idx_global_rtm(1:rtm%nnod_rtm,i)
      end do
!
      r_gl_1_IO(1:rtm%nidx_rtm(1))                                      &
     &      =     rtm%radius_1d_rtm_r(1:rtm%nidx_rtm(1))
      idx_gl_1_IO(1:rtm%nidx_rtm(1))                                    &
     &       =   rtm%idx_gl_1d_rtm_r(1:rtm%nidx_rtm(1))
      idx_gl_2_IO(1:rtm%nidx_rtm(2),1)                                  &
     &       = rtm%idx_gl_1d_rtm_t(1:rtm%nidx_rtm(2))
      idx_gl_3_IO(1:rtm%nidx_rtm(3),1)                                  &
     &       = rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),1)
      idx_gl_3_IO(1:rtm%nidx_rtm(3),2)                                  &
     &       = rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),2)
!
      call dealloc_type_sph_1d_index_rtm(rtm)
      call dealloc_type_spheric_param_rtm(rtm)
!
      end subroutine copy_sph_node_type_rtm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rlm_to_IO(l_truncation, rlm)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: rlm
      integer(kind = kint) :: i
!
      ndir_sph_IO =            itwo
      sph_rank_IO(1:itwo) =    rlm%sph_rank_rlm(1:itwo)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ithree
!
      nidx_gl_sph_IO(1:itwo) = rlm%nidx_global_rlm(1:itwo)
      ltr_gl_IO =              l_truncation
!
      nnod_sph_IO = rlm%nnod_rlm
      nidx_sph_IO(1:itwo) = rlm%nidx_rlm(1:itwo)
      ist_sph_IO(1:itwo) =  rlm%ist_rlm(1:itwo)
      ied_sph_IO(1:itwo) =  rlm%ied_rlm(1:itwo)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      inod_gl_sph_IO(1:rlm%nnod_rlm)                                    &
     &       =    rlm%inod_global_rlm(1:rlm%nnod_rlm)
      do i = 1, itwo
        idx_gl_sph_IO(1:rlm%nnod_rlm,i)                                 &
     &       = rlm%idx_global_rlm(1:rlm%nnod_rlm,i)
      end do
!
      r_gl_1_IO(1:rlm%nidx_rlm(1))                                      &
     &       =     rlm%radius_1d_rlm_r(1:rlm%nidx_rlm(1))
      idx_gl_1_IO(1:rlm%nidx_rlm(1))                                    &
     &       =   rlm%idx_gl_1d_rlm_r(1:rlm%nidx_rlm(1))
      idx_gl_2_IO(1:rlm%nidx_rlm(2),1)                                  &
     &       = rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),1)
      idx_gl_2_IO(1:rlm%nidx_rlm(2),2)                                  &
     &      = rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),2)
      idx_gl_2_IO(1:rlm%nidx_rlm(2),3)                                  &
     &       = rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),3)
!
      call dealloc_type_sph_1d_index_rlm(rlm)
      call dealloc_type_spheric_param_rlm(rlm)
!
      end subroutine copy_sph_node_type_rlm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_type_rj_to_IO(l_truncation, rj)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(inout) :: rj
      integer(kind = kint) :: i
!
      ndir_sph_IO =            itwo
      sph_rank_IO(1:itwo) =    rj%sph_rank_rj(1:itwo)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ithree
!
      nidx_gl_sph_IO(1:itwo) = rj%nidx_global_rj(1:itwo)
      ltr_gl_IO =              l_truncation
!
      nnod_sph_IO = rj%nnod_rj
      nidx_sph_IO(1:itwo) = rj%nidx_rj(1:itwo)
      ist_sph_IO(1:itwo) =  rj%ist_rj(1:itwo)
      ied_sph_IO(1:itwo) =  rj%ied_rj(1:itwo)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      inod_gl_sph_IO(1:rj%nnod_rj) =    rj%inod_global_rj(1:rj%nnod_rj)
      do i = 1, itwo
        idx_gl_sph_IO(1:rj%nnod_rj,i)                                   &
     &      = rj%idx_global_rj(1:rj%nnod_rj,i)
      end do
!
      r_gl_1_IO(1:rj%nidx_rj(1)) =   rj%radius_1d_rj_r(1:rj%nidx_rj(1))
      idx_gl_1_IO(1:rj%nidx_rj(1)) = rj%idx_gl_1d_rj_r(1:rj%nidx_rj(1))
      idx_gl_2_IO(1:rj%nidx_rj(2),1)                                    &
     &       = rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),1)
      idx_gl_2_IO(1:rj%nidx_rj(2),2)                                    &
     &      = rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),2)
      idx_gl_2_IO(1:rj%nidx_rj(2),3)                                    &
     &      = rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),3)
!
      call dealloc_type_sph_1d_index_rj(rj)
      call dealloc_type_spheric_param_rj(rj)
!
      end subroutine copy_sph_node_type_rj_to_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_node_type_IO
