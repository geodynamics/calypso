!copy_sph_node_4_type.f90
!      module copy_sph_node_4_type
!
!     Written by H. Matsui on July, 2007
!
!      subroutine copy_sph_node_rtp_from_type(ltr, ltr_type, rtp)
!      subroutine copy_sph_node_rtm_from_type(ltr, ltr_type, rtm)
!      subroutine copy_sph_node_rlm_from_type(ltr, ltr_type, rlm)
!      subroutine copy_sph_node_rj_from_type(ltr, ltr_type, rj)
!        integer(kind = kint), intent(in) :: ltr_type
!        integer(kind = kint), intent(inout) :: ltr
!
!      subroutine copy_sph_node_rtp_to_type(ltr, ltr_type, rtp)
!      subroutine copy_sph_node_rtm_to_type(ltr, ltr_type, rtm)
!      subroutine copy_sph_node_rlm_to_type(ltr, ltr_type, rlm)
!      subroutine copy_sph_node_rj_to_type(ltr, ltr_type, rj)
!        integer(kind = kint), intent(in) :: ltr
!        integer(kind = kint), intent(inout) :: ltr_type
!        type(sph_rtp_grid), intent(inout) :: rtp
!        type(sph_rtm_grid), intent(inout) :: rtm
!        type(sph_rlm_grid), intent(inout) :: rlm
!        type(sph_rj_grid), intent(inout) :: rj
!
      module copy_sph_node_4_type
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use t_spheric_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtp_from_type(ltr, ltr_type, rtp)
!
      integer(kind = kint), intent(in) :: ltr_type
      type(sph_rtp_grid), intent(in) :: rtp
!
      integer(kind = kint), intent(inout) :: ltr
!
      integer(kind = kint) :: i
!
      sph_rank_rtp(1:ithree) =    rtp%sph_rank_rtp(1:ithree)
!
      nidx_global_rtp(1:ithree) = rtp%nidx_global_rtp(1:ithree)
      ltr =  ltr_type
!
      nnod_rtp =      rtp%nnod_rtp
      nnod_rtp_pole = rtp%nnod_rtp_pole
!
      nidx_rtp(1:ithree) = rtp%nidx_rtp(1:ithree)
      ist_rtp(1:ithree) =  rtp%ist_rtp(1:ithree)
      ied_rtp(1:ithree) =  rtp%ied_rtp(1:ithree)
!
      call allocate_spheric_param_rtp
      call allocate_sph_1d_index_rtp
!
      do i = 1, ithree
        idx_global_rtp(1:nnod_rtp,i)                                    &
     &         = rtp%idx_global_rtp(1:rtp%nnod_rtp,i) 
      end do
!
      radius_1d_rtp_r(1:nidx_rtp(1))                                    &
     &         =   rtp%radius_1d_rtp_r(1:nidx_rtp(1))
      idx_gl_1d_rtp_r(1:nidx_rtp(1))                                    &
     &         =   rtp%idx_gl_1d_rtp_r(1:nidx_rtp(1))
      idx_gl_1d_rtp_t(1:nidx_rtp(2))                                    &
     &         =   rtp%idx_gl_1d_rtp_t(1:nidx_rtp(2))
      idx_gl_1d_rtp_p(1:nidx_rtp(3),1)                                  &
     &          = rtp%idx_gl_1d_rtp_p(1:nidx_rtp(3),1)
      idx_gl_1d_rtp_p(1:nidx_rtp(3),2)                                  &
     &          = rtp%idx_gl_1d_rtp_p(1:nidx_rtp(3),2)
!
!      call dealloc_type_sph_1d_index_rtp(rtp)
!      call dealloc_type_spheric_param_rtp(rtp)
!
      end subroutine copy_sph_node_rtp_from_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtm_from_type(ltr, ltr_type, rtm)
!
      integer(kind = kint), intent(in) :: ltr_type
      type(sph_rtm_grid), intent(in) :: rtm
!
      integer(kind = kint), intent(inout) :: ltr
!
      integer(kind = kint) :: i
!
!
      sph_rank_rtm(1:ithree) =    rtm%sph_rank_rtm(1:ithree)
!
      nidx_global_rtm(1:ithree) = rtm%nidx_global_rtm(1:ithree)
      ltr =  ltr_type
!
      nnod_rtm = rtm%nnod_rtm 
      nidx_rtm(1:ithree) = rtm%nidx_rtm(1:ithree)
      ist_rtm(1:ithree) =  rtm%ist_rtm(1:ithree)
      ied_rtm(1:ithree) =  rtm%ied_rtm(1:ithree)
!
      call allocate_spheric_param_rtm
      call allocate_sph_1d_index_rtm
!
      do i = 1, ithree
        idx_global_rtm(1:nnod_rtm,i) = rtm%idx_global_rtm(1:nnod_rtm,i)
      end do
!
      radius_1d_rtm_r(1:nidx_rtm(1))                                    &
     &        =   rtm%radius_1d_rtm_r(1:nidx_rtm(1))
      idx_gl_1d_rtm_r(1:nidx_rtm(1))                                    &
     &       =   rtm%idx_gl_1d_rtm_r(1:nidx_rtm(1))
      idx_gl_1d_rtm_t(1:nidx_rtm(2))                                    &
     &       =   rtm%idx_gl_1d_rtm_t(1:nidx_rtm(2))
      idx_gl_1d_rtm_m(1:nidx_rtm(3),1)                                  &
     &       = rtm%idx_gl_1d_rtm_m(1:nidx_rtm(3),1)
      idx_gl_1d_rtm_m(1:nidx_rtm(3),2)                                  &
     &       = rtm%idx_gl_1d_rtm_m(1:nidx_rtm(3),2)
!
!      call dealloc_type_sph_1d_index_rtm(rtm)
!      call dealloc_type_spheric_param_rtm(rtm)
!
      end subroutine copy_sph_node_rtm_from_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rlm_from_type(ltr, ltr_type, rlm)
!
      integer(kind = kint), intent(in) :: ltr_type
      type(sph_rlm_grid), intent(in) :: rlm
!
      integer(kind = kint), intent(inout) :: ltr
!
      integer(kind = kint) :: i
!
      sph_rank_rlm(1:itwo) =    rlm%sph_rank_rlm(1:itwo)
!
      nidx_global_rlm(1:itwo) = rlm%nidx_global_rlm(1:itwo)
      ltr =  ltr_type
!
      nnod_rlm = rlm%nnod_rlm
      nidx_rlm(1:itwo) = rlm%nidx_rlm(1:itwo)
      ist_rlm(1:itwo) =  rlm%ist_rlm(1:itwo)
      ied_rlm(1:itwo) =  rlm%ied_rlm(1:itwo)
!
      call allocate_spheric_param_rlm
      call allocate_sph_1d_index_rlm
!
      do i = 1, itwo
        idx_global_rlm(1:nnod_rlm,i) = rlm%idx_global_rlm(1:nnod_rlm,i)
      end do
!
      radius_1d_rlm_r(1:nidx_rlm(1))                                    &
     &                =   rlm%radius_1d_rlm_r(1:nidx_rlm(1))
      idx_gl_1d_rlm_r(1:nidx_rlm(1))                                    &
     &                =   rlm%idx_gl_1d_rlm_r(1:nidx_rlm(1))
      idx_gl_1d_rlm_j(1:nidx_rlm(2),1)                                  &
     &                = rlm%idx_gl_1d_rlm_j(1:nidx_rlm(2),1)
      idx_gl_1d_rlm_j(1:nidx_rlm(2),2)                                  &
     &                = rlm%idx_gl_1d_rlm_j(1:nidx_rlm(2),2)
      idx_gl_1d_rlm_j(1:nidx_rlm(2),3)                                  &
     &                = rlm%idx_gl_1d_rlm_j(1:nidx_rlm(2),3)
!
!      call dealloc_type_sph_1d_index_rlm(rlm)
!      call dealloc_type_spheric_param_rlm(rlm)
!
      end subroutine copy_sph_node_rlm_from_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rj_from_type(ltr, ltr_type, rj)
!
      integer(kind = kint), intent(in) :: ltr_type
      type(sph_rj_grid), intent(in) :: rj
!
      integer(kind = kint), intent(inout) :: ltr
!
      integer(kind = kint) :: i
!
      sph_rank_rj(1:itwo) =     rj%sph_rank_rj(1:itwo)
!
      nidx_global_rj(1:itwo) = rj%nidx_global_rj(1:itwo)
      ltr =  ltr_type
!
      nnod_rj = rj%nnod_rj
      nidx_rj(1:itwo) = rj%nidx_rj(1:itwo)
      ist_rj(1:itwo) =  rj%ist_rj(1:itwo)
      ied_rj(1:itwo) =  rj%ied_rj(1:itwo)
!
      call allocate_spheric_param_rj
      call allocate_sph_1d_index_rj
!
      do i = 1, itwo
        idx_global_rj(1:nnod_rj,i) = rj%idx_global_rj(1:nnod_rj,i)
      end do
!
      radius_1d_rj_r(1:nidx_rj(1))                                      &
     &                 =   rj%radius_1d_rj_r(1:nidx_rj(1))
      idx_gl_1d_rj_r(1:nidx_rj(1))                                      &
     &                 =   rj%idx_gl_1d_rj_r(1:nidx_rj(1))
      idx_gl_1d_rj_j(1:nidx_rj(2),1)                                    &
     &                 = rj%idx_gl_1d_rj_j(1:nidx_rj(2),1)
      idx_gl_1d_rj_j(1:nidx_rj(2),2)                                    &
     &                 = rj%idx_gl_1d_rj_j(1:nidx_rj(2),2)
      idx_gl_1d_rj_j(1:nidx_rj(2),3)                                    &
     &                 = rj%idx_gl_1d_rj_j(1:nidx_rj(2),3)
!
!      call dealloc_type_sph_1d_index_rj(rj)
!      call dealloc_type_spheric_param_rj(rj)
!
      end subroutine copy_sph_node_rj_from_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtp_to_type(ltr, ltr_type, rtp)
!
      integer(kind = kint), intent(in) :: ltr
!
      type(sph_rtp_grid), intent(inout) :: rtp
      integer(kind = kint), intent(inout) :: ltr_type
!
      integer(kind = kint) :: i
!
      rtp%sph_rank_rtp(1:ithree) =    sph_rank_rtp(1:ithree)
!
      rtp%nidx_global_rtp(1:ithree) = nidx_global_rtp(1:ithree)
      ltr_type =   ltr
!
      rtp%nnod_rtp = nnod_rtp
      rtp%nidx_rtp(1:ithree) = nidx_rtp(1:ithree)
      rtp%ist_rtp(1:ithree) =  ist_rtp(1:ithree)
      rtp%ied_rtp(1:ithree) =  ied_rtp(1:ithree)
!
      call alloc_type_spheric_param_rtp(rtp)
      call alloc_type_sph_1d_index_rtp(rtp)
!
      do i = 1, ithree
        rtp%idx_global_rtp(1:rtp%nnod_rtp,i)                            &
     &       = idx_global_rtp(1:rtp%nnod_rtp,i)
      end do
!
      rtp%radius_1d_rtp_r(1:rtp%nidx_rtp(1))                            &
     &       = radius_1d_rtp_r(1:rtp%nidx_rtp(1))
      rtp%idx_gl_1d_rtp_r(1:rtp%nidx_rtp(1))                            &
     &       = idx_gl_1d_rtp_r(1:rtp%nidx_rtp(1))
      rtp%idx_gl_1d_rtp_t(1:rtp%nidx_rtp(2))                            &
     &       = idx_gl_1d_rtp_t(1:rtp%nidx_rtp(2))
      rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),1)                          &
     &       = idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),1)
      rtp%idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),2)                          &
     &       = idx_gl_1d_rtp_p(1:rtp%nidx_rtp(3),2)
!
!      call deallocate_sph_1d_index_rtp
!      call deallocate_spheric_param_rtp
!
      end subroutine copy_sph_node_rtp_to_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtm_to_type(ltr, ltr_type, rtm)
!
      integer(kind = kint), intent(in) :: ltr
!
      type(sph_rtm_grid), intent(inout) :: rtm
      integer(kind = kint), intent(inout) :: ltr_type
!
      integer(kind = kint) :: i
!
      rtm%sph_rank_rtm(1:ithree) =    sph_rank_rtm(1:ithree)
!
      rtm%nidx_global_rtm(1:ithree) = nidx_global_rtm(1:ithree)
      ltr_type =   ltr
!
      rtm%nnod_rtm = nnod_rtm
      rtm%nidx_rtm(1:ithree) = nidx_rtm(1:ithree)
      rtm%ist_rtm(1:ithree) =  ist_rtm(1:ithree)
      rtm%ied_rtm(1:ithree) =  ied_rtm(1:ithree)
!
      call alloc_type_spheric_param_rtm(rtm)
      call alloc_type_sph_1d_index_rtm(rtm)
!
      do i = 1, ithree
        rtm%idx_global_rtm(1:rtm%nnod_rtm,i)                            &
     &      = idx_global_rtm(1:rtm%nnod_rtm,i)
      end do
!
      rtm%radius_1d_rtm_r(1:rtm%nidx_rtm(1))                            &
     &      =   radius_1d_rtm_r(1:rtm%nidx_rtm(1))
      rtm%idx_gl_1d_rtm_r(1:rtm%nidx_rtm(1))                            &
     &      =   idx_gl_1d_rtm_r(1:rtm%nidx_rtm(1))
      rtm%idx_gl_1d_rtm_t(1:rtm%nidx_rtm(2))                            &
     &      =   idx_gl_1d_rtm_t(1:rtm%nidx_rtm(2))
      rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),1)                          &
     &      = idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),1)
      rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),2)                          &
     &      = idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),2)
!
!      call deallocate_sph_1d_index_rtm
!      call deallocate_spheric_param_rtm
!
      end subroutine copy_sph_node_rtm_to_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rlm_to_type(ltr, ltr_type, rlm)
!
      integer(kind = kint), intent(in) :: ltr
!
      type(sph_rlm_grid), intent(inout) :: rlm
      integer(kind = kint), intent(inout) :: ltr_type
!
      integer(kind = kint) :: i
!
      rlm%sph_rank_rlm(1:itwo) =    sph_rank_rlm(1:itwo)
!
      rlm%nidx_global_rlm(1:itwo) = nidx_global_rlm(1:itwo)
      ltr_type =   ltr
!
      rlm%nnod_rlm = nnod_rlm
      rlm%nidx_rlm(1:itwo) = nidx_rlm(1:itwo)
      rlm%ist_rlm(1:itwo) =  ist_rlm(1:itwo)
      rlm%ied_rlm(1:itwo) =  ied_rlm(1:itwo)
!
      call alloc_type_spheric_param_rlm(rlm)
      call alloc_type_sph_1d_index_rlm(rlm)
!
      do i = 1, itwo
        rlm%idx_global_rlm(1:rlm%nnod_rlm,i)                            &
     &      = idx_global_rlm(1:rlm%nnod_rlm,i)
      end do
!
      rlm%radius_1d_rlm_r(1:rlm%nidx_rlm(1))                            &
     &       =   radius_1d_rlm_r(1:rlm%nidx_rlm(1))
      rlm%idx_gl_1d_rlm_r(1:rlm%nidx_rlm(1))                            &
     &       =   idx_gl_1d_rlm_r(1:rlm%nidx_rlm(1))
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),1)                          &
     &       = idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),1)
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),2)                          &
     &       = idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),2)
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),3)                          &
     &       = idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),3)
!
!      call deallocate_sph_1d_index_rlm
!      call deallocate_spheric_param_rlm
!
      end subroutine copy_sph_node_rlm_to_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rj_to_type(ltr, ltr_type, rj)
!
      integer(kind = kint), intent(in) :: ltr
!
      type(sph_rj_grid), intent(inout) :: rj
      integer(kind = kint), intent(inout) :: ltr_type
!
      integer(kind = kint) :: i
!
      rj%sph_rank_rj(1:itwo) =     sph_rank_rj(1:itwo)
!
      rj%nidx_global_rj(1:itwo) = nidx_global_rj(1:itwo)
      ltr_type =   ltr
!
      rj%nnod_rj = nnod_rj
      rj%nidx_rj(1:itwo) = nidx_rj(1:itwo)
      rj%ist_rj(1:itwo) =  ist_rj(1:itwo)
      rj%ied_rj(1:itwo) =  ied_rj(1:itwo)
!
      call alloc_type_spheric_param_rj(rj)
      call alloc_type_sph_1d_index_rj(rj)
!
      do i = 1, itwo
        rj%idx_global_rj(1:rj%nnod_rj,i)                                &
     &       = idx_global_rj(1:rj%nnod_rj,i)
      end do
!
      rj%radius_1d_rj_r(1:rj%nidx_rj(1))                                &
     &       = radius_1d_rj_r(1:rj%nidx_rj(1))
      rj%idx_gl_1d_rj_r(1:rj%nidx_rj(1))                                &
     &       = idx_gl_1d_rj_r(1:rj%nidx_rj(1))
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),1)                              &
     &       = idx_gl_1d_rj_j(1:rj%nidx_rj(2),1)
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),2)                              &
     &       = idx_gl_1d_rj_j(1:rj%nidx_rj(2),2)
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),3)                              &
     &      = idx_gl_1d_rj_j(1:rj%nidx_rj(2),3)
!
!      call deallocate_sph_1d_index_rj
!      call deallocate_spheric_param_rj
!
      end subroutine copy_sph_node_rj_to_type
!
! ----------------------------------------------------------------------
!
      end module copy_sph_node_4_type
