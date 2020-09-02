!>@file   copy_sph_rtm_node_4_IO.f90
!!@brief  module copy_sph_rtm_node_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr indices structure between IO buffer
!!
!!@verbatim
!!      subroutine copy_sph_node_4_rtm_from_IO(sph_IO, rtm, l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!        type(sph_IO_data), intent(in) :: sph_IO
!!      subroutine copy_sph_node_4_rtm_to_IO(l_truncation, rtm, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rtm_grid), intent(in) :: rtm
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      integer(kind = kint) function compare_sph_rtm_node_with_IO      &
!!     &                            (l_truncation, rtm, sph_IO)
!!        type(sph_rtm_grid), intent(in) :: rtm
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module copy_sph_rtm_node_4_IO
!
      use m_precision
!
      use m_constants
      use t_spheric_rtm_data
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
      subroutine copy_sph_node_4_rtm_from_IO(sph_IO, rtm, l_truncation)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: rtm
      type(sph_IO_data), intent(in) :: sph_IO
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
      call alloc_spheric_param_rtm(rtm)
      call alloc_sph_1d_index_rtm(rtm)
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
      end subroutine copy_sph_node_4_rtm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rtm_to_IO(l_truncation, rtm, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(in) :: rtm
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
      end subroutine copy_sph_node_4_rtm_to_IO
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_rtm_node_with_IO        &
     &                            (l_truncation, rtm, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(in) :: rtm
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      compare_sph_rtm_node_with_IO = 1
      do i = 1, ithree
        if(sph_IO%sph_rank(i) .ne. rtm%irank_sph_rtm(i)) return
      end do
!
      do i = 1, ithree
        if(sph_IO%nidx_gl_sph(i) .ne. rtm%nidx_global_rtm(i)) return
      end do
      if(sph_IO%ltr_gl .ne. l_truncation) return
!
      if(sph_IO%numnod_sph .ne. rtm%nnod_rtm) return
!
      do i = 1, ithree
        if(sph_IO%nidx_sph(i) .ne. rtm%nidx_rtm(i)) return
        if(sph_IO%ist_sph(i) .ne.  rtm%ist_rtm(i)) return
        if(sph_IO%ied_sph(i) .ne.  rtm%ied_rtm(i)) return
      end do
!
      do i = 1, rtm%nnod_rtm
        if(sph_IO%idx_gl_sph(i,1) .ne. rtm%idx_global_rtm(i,1)) return
        if(sph_IO%idx_gl_sph(i,2) .ne. rtm%idx_global_rtm(i,2)) return
        if(sph_IO%idx_gl_sph(i,3) .ne. rtm%idx_global_rtm(i,3)) return
      end do
!
      do i = 1, rtm%nidx_rtm(1)
        if(sph_IO%r_gl_1(i) .ne.     rtm%radius_1d_rtm_r(i)) then
          if(abs(sph_IO%r_gl_1(i) - rtm%radius_1d_rtm_r(i))             &
     &       .gt. 1.0d-10) return
        end if
        if(sph_IO%idx_gl_1(i) .ne.   rtm%idx_gl_1d_rtm_r(i)) return
      end do
!
      do i = 1, rtm%nidx_rtm(2)
        if(sph_IO%idx_gl_2(i,1) .ne. rtm%idx_gl_1d_rtm_t(i)) return
      end do
!
      do i = 1, rtm%nidx_rtm(3)
        if(sph_IO%idx_gl_3(i,1) .ne. rtm%idx_gl_1d_rtm_m(i,1)) return
        if(sph_IO%idx_gl_3(i,2) .ne. rtm%idx_gl_1d_rtm_m(i,2)) return
      end do
      compare_sph_rtm_node_with_IO = 0
!
      end function compare_sph_rtm_node_with_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_rtm_node_4_IO
