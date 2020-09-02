!>@file   copy_sph_rlm_mode_4_IO.f90
!!@brief  module copy_sph_rlm_mode_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr indices structure between IO buffer
!!
!!@verbatim
!!      subroutine copy_sph_node_4_rlm_from_IO(sph_IO, rlm, l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!        type(sph_IO_data), intent(in) :: sph_IO
!!      subroutine copy_sph_node_4_rlm_to_IO(l_truncation, rlm, sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rlm_grid), intent(in) :: rlm
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      integer(kind = kint) function compare_sph_mode_rlm_with_IO      &
!!     &                            (l_truncation, rlm, sph_IO)
!!        type(sph_rlm_grid), intent(in) :: rlm
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module copy_sph_rlm_mode_4_IO
!
      use m_precision
!
      use m_constants
      use t_spheric_rlm_data
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
      subroutine copy_sph_node_4_rlm_from_IO(sph_IO, rlm, l_truncation)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: rlm
      type(sph_IO_data), intent(in) :: sph_IO
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
      call alloc_spheric_param_rlm(rlm)
      call alloc_sph_1d_index_rlm(rlm)
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
      end subroutine copy_sph_node_4_rlm_from_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rlm_to_IO(l_truncation, rlm, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rlm_grid), intent(in) :: rlm
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
      end subroutine copy_sph_node_4_rlm_to_IO
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_mode_rlm_with_IO        &
     &                            (l_truncation, rlm, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rlm_grid), intent(in) :: rlm
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      compare_sph_mode_rlm_with_IO = 1
      do i = 1, itwo
        if(sph_IO%sph_rank(i) .ne. rlm%irank_sph_rlm(i)) return
      end do
!
      do i = 1, itwo
        if(sph_IO%nidx_gl_sph(i) .ne. rlm%nidx_global_rlm(i)) return
      end do
      if(sph_IO%ltr_gl .ne. l_truncation) return
!
      if(sph_IO%numnod_sph .ne. rlm%nnod_rlm) return
!
      do i = 1, itwo
        if(sph_IO%nidx_sph(i) .ne. rlm%nidx_rlm(i)) return
        if(sph_IO%ist_sph(i) .ne.  rlm%ist_rlm(i)) return
        if(sph_IO%ied_sph(i) .ne.  rlm%ied_rlm(i)) return
      end do
!
      do i = 1, rlm%nnod_rlm
        if(sph_IO%idx_gl_sph(i,1) .ne. rlm%idx_global_rlm(i,1)) return
        if(sph_IO%idx_gl_sph(i,2) .ne. rlm%idx_global_rlm(i,2)) return
      end do
!
      do i = 1, rlm%nidx_rlm(1)
        if(sph_IO%r_gl_1(i)  .ne. rlm%radius_1d_rlm_r(i)) then
          if(abs(sph_IO%r_gl_1(i) - rlm%radius_1d_rlm_r(i))             &
     &       .gt. 1.0d-10) return
        end if
        if(sph_IO%idx_gl_1(i) .ne. rlm%idx_gl_1d_rlm_r(i)) return
      end do
!
      do i = 1, rlm%nidx_rlm(2)
        if(sph_IO%idx_gl_2(i,1) .ne. rlm%idx_gl_1d_rlm_j(i,1)) return
        if(sph_IO%idx_gl_2(i,2) .ne. rlm%idx_gl_1d_rlm_j(i,2)) return
        if(sph_IO%idx_gl_2(i,3) .ne. rlm%idx_gl_1d_rlm_j(i,3)) return
      end do
      compare_sph_mode_rlm_with_IO = 0
!
      end function compare_sph_mode_rlm_with_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_rlm_mode_4_IO
