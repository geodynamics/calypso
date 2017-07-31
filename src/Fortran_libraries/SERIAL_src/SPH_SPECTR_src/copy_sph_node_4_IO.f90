!>@file   copy_sph_node_4_IO.f90
!!@brief  module copy_sph_node_4_IO
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
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      subroutine copy_sph_node_4_rtm_from_IO(sph_IO, rtm, l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      subroutine copy_sph_node_4_rlm_from_IO(sph_IO, rlm, l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      subroutine copy_sph_node_4_rj_from_IO(sph_IO, rj, l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rj_grid), intent(inout) :: rj
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine copy_sph_node_4_rtp_to_IO(l_truncation, rtp, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rtp_grid), intent(inout) :: rtp
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      subroutine copy_sph_node_4_rtm_to_IO(l_truncation, rtm, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      subroutine copy_sph_node_4_rlm_to_IO(l_truncation, rlm, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      subroutine copy_sph_node_4_rj_to_IO(l_truncation, rj, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rj_grid), intent(inout) :: rj
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module copy_sph_node_4_IO
!
      use m_precision
!
      use m_constants
      use t_spheric_parameter
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
      use t_spheric_rtp_data
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: rtp
      type(sph_IO_data), intent(inout) :: sph_IO
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
!
      call alloc_type_spheric_param_rtp(rtp)
      call alloc_type_sph_1d_index_rtp(rtp)
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
      call dealloc_nod_id_sph_IO(sph_IO)
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
      call dealloc_idx_sph_1d3_IO(sph_IO)
!
      end subroutine copy_sph_node_4_rtp_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rtm_from_IO(sph_IO, rtm, l_truncation)
!
      use t_spheric_rtm_data
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: rtm
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
      rtm%irank_sph_rtm(1:ithree) =   sph_IO%sph_rank(1:ithree)
!
      rtm%nidx_global_rtm(1:ithree) = sph_IO%nidx_gl_sph(1:ithree)
      l_truncation =                  sph_IO%ltr_gl
!
      rtm%nnod_rtm =           sph_IO%numnod_sph
      rtm%nidx_rtm(1:ithree) = sph_IO%nidx_sph(1:ithree)
      rtm%ist_rtm(1:ithree) =  sph_IO%ist_sph(1:ithree)
      rtm%ied_rtm(1:ithree) =  sph_IO%ied_sph(1:ithree)
!
      call alloc_type_spheric_param_rtm(rtm)
      call alloc_type_sph_1d_index_rtm(rtm)
!
      do i = 1, ithree
        rtm%idx_global_rtm(1:rtm%nnod_rtm,i)                            &
     &      = sph_IO%idx_gl_sph(1:rtm%nnod_rtm,i)
      end do
!
!$omp parallel workshare
      rtm%radius_1d_rtm_r(1:rtm%nidx_rtm(1))                            &
     &      =   sph_IO%r_gl_1(1:rtm%nidx_rtm(1))
      rtm%idx_gl_1d_rtm_r(1:rtm%nidx_rtm(1))                            &
     &      =   sph_IO%idx_gl_1(1:rtm%nidx_rtm(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      rtm%idx_gl_1d_rtm_t(1:rtm%nidx_rtm(2))                            &
     &      =   sph_IO%idx_gl_2(1:rtm%nidx_rtm(2),1)
!$omp end parallel workshare
!
!$omp parallel workshare
      rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),1)                          &
     &      = sph_IO%idx_gl_3(1:rtm%nidx_rtm(3),1)
      rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),2)                          &
     &      = sph_IO%idx_gl_3(1:rtm%nidx_rtm(3),2)
!$omp end parallel workshare
!
      call dealloc_nod_id_sph_IO(sph_IO)
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
      call dealloc_idx_sph_1d3_IO(sph_IO)
!
      end subroutine copy_sph_node_4_rtm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rlm_from_IO(sph_IO, rlm, l_truncation)
!
      use t_spheric_rlm_data
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: rlm
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
      rlm%irank_sph_rlm(1:itwo) =   sph_IO%sph_rank(1:itwo)
!
      rlm%nidx_global_rlm(1:itwo) = sph_IO%nidx_gl_sph(1:itwo)
      l_truncation =                sph_IO%ltr_gl
!
      rlm%nnod_rlm =         sph_IO%numnod_sph
      rlm%nidx_rlm(1:itwo) = sph_IO%nidx_sph(1:itwo)
      rlm%ist_rlm(1:itwo) =  sph_IO%ist_sph(1:itwo)
      rlm%ied_rlm(1:itwo) =  sph_IO%ied_sph(1:itwo)
!
      call alloc_type_spheric_param_rlm(rlm)
      call alloc_type_sph_1d_index_rlm(rlm)
!
      do i = 1, itwo
        rlm%idx_global_rlm(1:rlm%nnod_rlm,i)                            &
     &      = sph_IO%idx_gl_sph(1:rlm%nnod_rlm,i)
      end do
!
!$omp parallel workshare
      rlm%radius_1d_rlm_r(1:rlm%nidx_rlm(1))                            &
     &       =   sph_IO%r_gl_1(1:rlm%nidx_rlm(1))
      rlm%idx_gl_1d_rlm_r(1:rlm%nidx_rlm(1))                            &
     &       =   sph_IO%idx_gl_1(1:rlm%nidx_rlm(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),1)                          &
     &       = sph_IO%idx_gl_2(1:rlm%nidx_rlm(2),1)
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),2)                          &
     &       = sph_IO%idx_gl_2(1:rlm%nidx_rlm(2),2)
      rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),3)                          &
     &       = sph_IO%idx_gl_2(1:rlm%nidx_rlm(2),3)
!$omp end parallel workshare
!
      call dealloc_nod_id_sph_IO(sph_IO)
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
!
      end subroutine copy_sph_node_4_rlm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rj_from_IO(sph_IO, rj, l_truncation)
!
      use t_spheric_rj_data
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid), intent(inout) :: rj
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
      rj%irank_sph_rj(1:itwo) =   sph_IO%sph_rank(1:itwo)
!
      rj%nidx_global_rj(1:itwo) = sph_IO%nidx_gl_sph(1:itwo)
      l_truncation =              sph_IO%ltr_gl
!
      rj%nnod_rj =         sph_IO%numnod_sph
      rj%nidx_rj(1:itwo) = sph_IO%nidx_sph(1:itwo)
      rj%ist_rj(1:itwo) =  sph_IO%ist_sph(1:itwo)
      rj%ied_rj(1:itwo) =  sph_IO%ied_sph(1:itwo)
!
      call alloc_type_spheric_param_rj(rj)
      call alloc_type_sph_1d_index_rj(rj)
!
      do i = 1, itwo
        rj%idx_global_rj(1:rj%nnod_rj,i)                                &
     &       = sph_IO%idx_gl_sph(1:rj%nnod_rj,i)
      end do
!
!$omp parallel workshare
      rj%radius_1d_rj_r(1:rj%nidx_rj(1))                                &
     &       = sph_IO%r_gl_1(1:rj%nidx_rj(1))
      rj%a_r_1d_rj_r(1:rj%nidx_rj(1))                                   &
     &       = one / rj%radius_1d_rj_r(1:rj%nidx_rj(1))
!
      rj%idx_gl_1d_rj_r(1:rj%nidx_rj(1))                                &
     &       = sph_IO%idx_gl_1(1:rj%nidx_rj(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),1)                              &
     &       = sph_IO%idx_gl_2(1:rj%nidx_rj(2),1)
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),2)                              &
     &       = sph_IO%idx_gl_2(1:rj%nidx_rj(2),2)
      rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),3)                              &
     &      = sph_IO%idx_gl_2(1:rj%nidx_rj(2),3)
!$omp end parallel workshare
!
      call dealloc_nod_id_sph_IO(sph_IO)
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
!
      end subroutine copy_sph_node_4_rj_from_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rtp_to_IO(l_truncation, rtp, sph_IO)
!
      use t_spheric_rtp_data
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: rtp
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
      do i = 1, ithree
        sph_IO%idx_gl_sph(1:rtp%nnod_rtp,i)                             &
     &        = rtp%idx_global_rtp(1:rtp%nnod_rtp,i)
      end do
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
      call dealloc_type_sph_1d_index_rtp(rtp)
      call dealloc_type_spheric_param_rtp(rtp)
!
      end subroutine copy_sph_node_4_rtp_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rtm_to_IO(l_truncation, rtm, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: rtm
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8, nrt8
!
!
      sph_IO%numdir_sph =         ithree
      sph_IO%sph_rank(1:ithree) = rtm%irank_sph_rtm(1:ithree)
!
      sph_IO%ncomp_table_1d(1) = ione
      sph_IO%ncomp_table_1d(2) = ione
      sph_IO%ncomp_table_1d(3) = itwo
!
      sph_IO%nidx_gl_sph(1:ithree) = rtm%nidx_global_rtm(1:ithree)
      sph_IO%ltr_gl =                l_truncation
!
      sph_IO%numnod_sph =         rtm%nnod_rtm
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      sph_IO%nidx_sph(1:ithree) = rtm%nidx_rtm(1:ithree)
      sph_IO%ist_sph(1:ithree) =  rtm%ist_rtm(1:ithree)
      sph_IO%ied_sph(1:ithree) =  rtm%ied_rtm(1:ithree)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
!$omp parallel do private(i,nr_8,nrt8)
      do i = 1, rtm%nnod_rtm
        nr_8 = rtm%nidx_global_rtm(1)
        nrt8 = rtm%nidx_global_rtm(1)*rtm%nidx_global_rtm(2)
        sph_IO%idx_gl_sph(i,1) = rtm%idx_global_rtm(i,1)
        sph_IO%idx_gl_sph(i,2) = rtm%idx_global_rtm(i,2)
        sph_IO%idx_gl_sph(i,3) = rtm%idx_global_rtm(i,3)
        sph_IO%inod_gl_sph(i) = rtm%idx_global_rtm(i,1)                 &
     &                        + (rtm%idx_global_rtm(i,2) - 1) * nr_8    &
     &                        +  rtm%idx_global_rtm(i,3) * nrt8
      end do
!$omp end parallel do
!
!$omp parallel workshare
      sph_IO%r_gl_1(1:rtm%nidx_rtm(1))                                  &
     &      =     rtm%radius_1d_rtm_r(1:rtm%nidx_rtm(1))
      sph_IO%idx_gl_1(1:rtm%nidx_rtm(1))                                &
     &       =   rtm%idx_gl_1d_rtm_r(1:rtm%nidx_rtm(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_2(1:rtm%nidx_rtm(2),1)                              &
     &       = rtm%idx_gl_1d_rtm_t(1:rtm%nidx_rtm(2))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_3(1:rtm%nidx_rtm(3),1)                              &
     &       = rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),1)
      sph_IO%idx_gl_3(1:rtm%nidx_rtm(3),2)                              &
     &       = rtm%idx_gl_1d_rtm_m(1:rtm%nidx_rtm(3),2)
!$omp end parallel workshare
!
      call dealloc_type_sph_1d_index_rtm(rtm)
      call dealloc_type_spheric_param_rtm(rtm)
!
      end subroutine copy_sph_node_4_rtm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rlm_to_IO(l_truncation, rlm, sph_IO)
!
      use t_spheric_rlm_data
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: rlm
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
!
      sph_IO%numdir_sph =       itwo
      sph_IO%sph_rank(1:itwo) = rlm%irank_sph_rlm(1:itwo)
!
      sph_IO%ncomp_table_1d(1) = ione
      sph_IO%ncomp_table_1d(2) = ithree
!
      sph_IO%nidx_gl_sph(1:itwo) = rlm%nidx_global_rlm(1:itwo)
      sph_IO%ltr_gl =              l_truncation
!
      sph_IO%numnod_sph =       rlm%nnod_rlm
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      sph_IO%nidx_sph(1:itwo) = rlm%nidx_rlm(1:itwo)
      sph_IO%ist_sph(1:itwo) =  rlm%ist_rlm(1:itwo)
      sph_IO%ied_sph(1:itwo) =  rlm%ied_rlm(1:itwo)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
!$omp parallel do private(i,nr_8)
      do i = 1, rlm%nnod_rlm
        nr_8 = rlm%nidx_global_rlm(1)
        sph_IO%idx_gl_sph(i,1) = rlm%idx_global_rlm(i,1)
        sph_IO%idx_gl_sph(i,2) = rlm%idx_global_rlm(i,2)
        sph_IO%inod_gl_sph(i) =  rlm%idx_global_rlm(i,1)                &
     &                          + rlm%idx_global_rlm(i,2) * nr_8
      end do
!$omp end parallel do
!
!$omp parallel workshare
      sph_IO%r_gl_1(1:rlm%nidx_rlm(1))                                  &
     &       =     rlm%radius_1d_rlm_r(1:rlm%nidx_rlm(1))
      sph_IO%idx_gl_1(1:rlm%nidx_rlm(1))                                &
     &       =   rlm%idx_gl_1d_rlm_r(1:rlm%nidx_rlm(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_2(1:rlm%nidx_rlm(2),1)                              &
     &       = rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),1)
      sph_IO%idx_gl_2(1:rlm%nidx_rlm(2),2)                              &
     &      = rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),2)
      sph_IO%idx_gl_2(1:rlm%nidx_rlm(2),3)                              &
     &       = rlm%idx_gl_1d_rlm_j(1:rlm%nidx_rlm(2),3)
!$omp end parallel workshare
!
      call dealloc_type_sph_1d_index_rlm(rlm)
      call dealloc_type_spheric_param_rlm(rlm)
!
      end subroutine copy_sph_node_4_rlm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rj_to_IO(l_truncation, rj, sph_IO)
!
      use t_spheric_rj_data
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(inout) :: rj
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
      sph_IO%numdir_sph =       itwo
      sph_IO%sph_rank(1:itwo) = rj%irank_sph_rj(1:itwo)
!
      sph_IO%ncomp_table_1d(1) = ione
      sph_IO%ncomp_table_1d(2) = ithree
!
      sph_IO%nidx_gl_sph(1:itwo) = rj%nidx_global_rj(1:itwo)
      sph_IO%ltr_gl =              l_truncation
!
      sph_IO%numnod_sph =       rj%nnod_rj
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      sph_IO%nidx_sph(1:itwo) = rj%nidx_rj(1:itwo)
      sph_IO%ist_sph(1:itwo) =  rj%ist_rj(1:itwo)
      sph_IO%ied_sph(1:itwo) =  rj%ied_rj(1:itwo)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
!$omp parallel do private(i,nr_8)
      do i = 1, rj%nnod_rj
        nr_8 = rj%nidx_global_rj(1)
        sph_IO%idx_gl_sph(i,1) = rj%idx_global_rj(i,1)
        sph_IO%idx_gl_sph(i,2) = rj%idx_global_rj(i,2)
        sph_IO%inod_gl_sph(i) =  rj%idx_global_rj(i,1)                  &
     &                          + rj%idx_global_rj(i,2) * nr_8
      end do
!$omp end parallel do
!
      if(sph_IO%inod_gl_sph(rj%nnod_rj) .eq. izero) then
        nr_8 = (rj%nidx_global_rj(2) + 1)
        sph_IO%inod_gl_sph(rj%nnod_rj)                                  &
     &       = rj%nidx_global_rj(1) * nr_8 + 1
      end if
!
!$omp parallel workshare
      sph_IO%r_gl_1(1:rj%nidx_rj(1))                                    &
     &       = rj%radius_1d_rj_r(1:rj%nidx_rj(1))
      sph_IO%idx_gl_1(1:rj%nidx_rj(1))                                  &
     &       = rj%idx_gl_1d_rj_r(1:rj%nidx_rj(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_2(1:rj%nidx_rj(2),1)                                &
     &       = rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),1)
      sph_IO%idx_gl_2(1:rj%nidx_rj(2),2)                                &
     &      = rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),2)
      sph_IO%idx_gl_2(1:rj%nidx_rj(2),3)                                &
     &      = rj%idx_gl_1d_rj_j(1:rj%nidx_rj(2),3)
!$omp end parallel workshare
!
      call dealloc_type_sph_1d_index_rj(rj)
      call dealloc_spheric_param_rj(rj)
!
      end subroutine copy_sph_node_4_rj_to_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_node_4_IO
