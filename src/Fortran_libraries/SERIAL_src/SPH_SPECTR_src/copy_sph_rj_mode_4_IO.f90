!>@file   copy_sph_rj_mode_4_IO.f90
!!@brief  module copy_sph_rj_mode_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr indices structure between IO buffer
!!
!!@verbatim
!!      subroutine copy_sph_node_4_rj_from_IO(sph_IO, sph_rj,           &
!!     &                                      l_truncation)
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(sph_IO_data), intent(in) :: sph_IO
!!      subroutine copy_sph_node_4_rj_to_IO(l_truncation, sph_rj,       &
!!     &                                    sph_IO)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!      integer(kind = kint) function compare_sph_node_rj_with_IO       &
!!     &                            (l_truncation, sph_rj, sph_IO)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module copy_sph_rj_mode_4_IO
!
      use m_precision
!
      use m_constants
      use t_spheric_rj_data
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
      subroutine copy_sph_node_4_rj_from_IO(sph_IO, sph_rj,             &
     &                                      l_truncation)
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
      sph_rj%irank_sph_rj(1:itwo) =   sph_IO%sph_rank(1:itwo)
!
      sph_rj%nidx_global_rj(1:itwo) = sph_IO%nidx_gl_sph(1:itwo)
      l_truncation =              sph_IO%ltr_gl
!
      sph_rj%nnod_rj =         sph_IO%numnod_sph
      sph_rj%nidx_rj(1:itwo) = sph_IO%nidx_sph(1:itwo)
      sph_rj%ist_rj(1:itwo) =  sph_IO%ist_sph(1:itwo)
      sph_rj%ied_rj(1:itwo) =  sph_IO%ied_sph(1:itwo)
!
      call alloc_spheric_param_rj(sph_rj)
      call alloc_sph_1d_index_rj(sph_rj)
!
      do i = 1, itwo
        sph_rj%idx_global_rj(1:sph_rj%nnod_rj,i)                        &
     &       = sph_IO%idx_gl_sph(1:sph_rj%nnod_rj,i)
      end do
!
!$omp parallel workshare
      sph_rj%idx_gl_1d_rj_r(1:sph_rj%nidx_rj(1))                        &
     &       = sph_IO%idx_gl_1(1:sph_rj%nidx_rj(1))
!
      sph_rj%radius_1d_rj_r(1:sph_rj%nidx_rj(1))                        &
     &       = sph_IO%r_gl_1(1:sph_rj%nidx_rj(1))
      sph_rj%a_r_1d_rj_r(1:sph_rj%nidx_rj(1))                           &
     &       = one / sph_rj%radius_1d_rj_r(1:sph_rj%nidx_rj(1))
!
      sph_rj%ar_1d_rj(1:sph_rj%nidx_rj(1),1)                            &
     &       = sph_rj%a_r_1d_rj_r(1:sph_rj%nidx_rj(1))
      sph_rj%ar_1d_rj(1:sph_rj%nidx_rj(1),2)                            &
     &       = sph_rj%ar_1d_rj(1:sph_rj%nidx_rj(1),1)                   &
     &        * sph_rj%a_r_1d_rj_r(1:sph_rj%nidx_rj(1))
      sph_rj%ar_1d_rj(1:sph_rj%nidx_rj(1),3)                            &
     &       = sph_rj%ar_1d_rj(1:sph_rj%nidx_rj(1),2)                   &
     &        * sph_rj%a_r_1d_rj_r(1:sph_rj%nidx_rj(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_rj%idx_gl_1d_rj_j(1:sph_rj%nidx_rj(2),1)                      &
     &       = sph_IO%idx_gl_2(1:sph_rj%nidx_rj(2),1)
      sph_rj%idx_gl_1d_rj_j(1:sph_rj%nidx_rj(2),2)                      &
     &       = sph_IO%idx_gl_2(1:sph_rj%nidx_rj(2),2)
      sph_rj%idx_gl_1d_rj_j(1:sph_rj%nidx_rj(2),3)                      &
     &      = sph_IO%idx_gl_2(1:sph_rj%nidx_rj(2),3)
!$omp end parallel workshare
!
      end subroutine copy_sph_node_4_rj_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_4_rj_to_IO(l_truncation, sph_rj,         &
     &                                    sph_IO)
!
      use t_spheric_rj_data
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
      sph_IO%numdir_sph =       itwo
      sph_IO%sph_rank(1:itwo) = sph_rj%irank_sph_rj(1:itwo)
!
      sph_IO%ncomp_table_1d(1) = ione
      sph_IO%ncomp_table_1d(2) = ithree
!
      sph_IO%nidx_gl_sph(1:itwo) = sph_rj%nidx_global_rj(1:itwo)
      sph_IO%ltr_gl =              l_truncation
!
      sph_IO%numnod_sph =       sph_rj%nnod_rj
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      sph_IO%nidx_sph(1:itwo) = sph_rj%nidx_rj(1:itwo)
      sph_IO%ist_sph(1:itwo) =  sph_rj%ist_rj(1:itwo)
      sph_IO%ied_sph(1:itwo) =  sph_rj%ied_rj(1:itwo)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
!$omp parallel do private(i,nr_8)
      do i = 1, sph_rj%nnod_rj
        nr_8 = sph_rj%nidx_global_rj(1)
        sph_IO%idx_gl_sph(i,1) = sph_rj%idx_global_rj(i,1)
        sph_IO%idx_gl_sph(i,2) = sph_rj%idx_global_rj(i,2)
        sph_IO%inod_gl_sph(i) =  sph_rj%idx_global_rj(i,1)              &
     &                          + sph_rj%idx_global_rj(i,2) * nr_8
      end do
!$omp end parallel do
!
      if(sph_IO%inod_gl_sph(sph_rj%nnod_rj) .eq. izero) then
        nr_8 = (sph_rj%nidx_global_rj(2) + 1)
        sph_IO%inod_gl_sph(sph_rj%nnod_rj)                              &
     &       = sph_rj%nidx_global_rj(1) * nr_8 + 1
      end if
!
!$omp parallel workshare
      sph_IO%r_gl_1(1:sph_rj%nidx_rj(1))                                &
     &       = sph_rj%radius_1d_rj_r(1:sph_rj%nidx_rj(1))
      sph_IO%idx_gl_1(1:sph_rj%nidx_rj(1))                              &
     &       = sph_rj%idx_gl_1d_rj_r(1:sph_rj%nidx_rj(1))
!$omp end parallel workshare
!
!$omp parallel workshare
      sph_IO%idx_gl_2(1:sph_rj%nidx_rj(2),1)                            &
     &       = sph_rj%idx_gl_1d_rj_j(1:sph_rj%nidx_rj(2),1)
      sph_IO%idx_gl_2(1:sph_rj%nidx_rj(2),2)                            &
     &      = sph_rj%idx_gl_1d_rj_j(1:sph_rj%nidx_rj(2),2)
      sph_IO%idx_gl_2(1:sph_rj%nidx_rj(2),3)                            &
     &      = sph_rj%idx_gl_1d_rj_j(1:sph_rj%nidx_rj(2),3)
!$omp end parallel workshare
!
      end subroutine copy_sph_node_4_rj_to_IO
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function compare_sph_node_rj_with_IO         &
     &                            (l_truncation, sph_rj, sph_IO)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
      compare_sph_node_rj_with_IO = 1
      do i = 1, itwo
        if(sph_IO%sph_rank(i) .ne. sph_rj%irank_sph_rj(i)) return
      end do
!
      do i = 1, itwo
        if(sph_IO%nidx_gl_sph(i) .ne. sph_rj%nidx_global_rj(i)) return
      end do
      if(sph_IO%ltr_gl .ne. l_truncation) return
!
      if(sph_IO%numnod_sph .ne. sph_rj%nnod_rj) return
!
      do i = 1, itwo
        if(sph_IO%nidx_sph(i) .ne. sph_rj%nidx_rj(i)) return
        if(sph_IO%ist_sph(i) .ne.  sph_rj%ist_rj(i)) return
        if(sph_IO%ied_sph(i) .ne.  sph_rj%ied_rj(i)) return
      end do
!
      do i = 1, sph_rj%nnod_rj
        if(sph_IO%idx_gl_sph(i,1)                                       &
     &      .ne. sph_rj%idx_global_rj(i,1)) return
        if(sph_IO%idx_gl_sph(i,2)                                       &
     &      .ne. sph_rj%idx_global_rj(i,2)) return
      end do
!
      do i = 1, sph_rj%nidx_rj(1)
        if(sph_IO%r_gl_1(i)  .ne. sph_rj%radius_1d_rj_r(i)) then
          if(abs(sph_IO%r_gl_1(i) - sph_rj%radius_1d_rj_r(i))           &
     &       .gt. 1.0d-10) return
        end if
        if(sph_IO%idx_gl_1(i) .ne. sph_rj%idx_gl_1d_rj_r(i)) return
      end do
!
      do i = 1, sph_rj%nidx_rj(2)
        if(sph_IO%idx_gl_2(i,1) .ne. sph_rj%idx_gl_1d_rj_j(i,1)) return
        if(sph_IO%idx_gl_2(i,2) .ne. sph_rj%idx_gl_1d_rj_j(i,2)) return
        if(sph_IO%idx_gl_2(i,3) .ne. sph_rj%idx_gl_1d_rj_j(i,3)) return
      end do
      compare_sph_node_rj_with_IO = 0
!
      end function compare_sph_node_rj_with_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_rj_mode_4_IO
