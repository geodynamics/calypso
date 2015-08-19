!>@file   copy_sph_node_4_IO.f90
!!@brief  module copy_sph_node_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr indices from IO buffer
!!
!!@verbatim
!!      subroutine copy_sph_node_rtp_from_IO
!!      subroutine copy_sph_node_rtm_from_IO
!!      subroutine copy_sph_node_rlm_from_IO
!!      subroutine copy_sph_node_rj_from_IO
!!
!!      subroutine copy_sph_node_rtp_to_IO
!!      subroutine copy_sph_node_rtm_to_IO
!!      subroutine copy_sph_node_rlm_to_IO
!!      subroutine copy_sph_node_rj_to_IO
!!@endverbatim
!
      module copy_sph_node_4_IO
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
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
      subroutine copy_sph_node_rtp_from_IO
!
      integer(kind = kint) :: i
!
      sph_rank_rtp(1:ithree) =    sph_rank_IO(1:ithree)
!
      nidx_global_rtp(1:ithree) = nidx_gl_sph_IO(1:ithree)
      l_truncation =              ltr_gl_IO
!
      nnod_rtp =      nnod_sph_IO
      nnod_rtp_pole = nnod_rtp
!
      nidx_rtp(1:ithree) = nidx_sph_IO(1:ithree)
      ist_rtp(1:ithree) =  ist_sph_IO(1:ithree)
      ied_rtp(1:ithree) =  ied_sph_IO(1:ithree)
!
      call allocate_spheric_param_rtp
      call allocate_sph_1d_index_rtp
!
      do i = 1, ithree
        idx_global_rtp(1:nnod_rtp,i) = idx_gl_sph_IO(1:nnod_rtp,i)
      end do
!
      radius_1d_rtp_r(1:nidx_rtp(1)) =   r_gl_1_IO(1:nidx_rtp(1))
      idx_gl_1d_rtp_r(1:nidx_rtp(1)) =   idx_gl_1_IO(1:nidx_rtp(1))
      idx_gl_1d_rtp_t(1:nidx_rtp(2)) =   idx_gl_2_IO(1:nidx_rtp(2),1)
      idx_gl_1d_rtp_p(1:nidx_rtp(3),1) = idx_gl_3_IO(1:nidx_rtp(3),1)
      idx_gl_1d_rtp_p(1:nidx_rtp(3),2) = idx_gl_3_IO(1:nidx_rtp(3),2)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine copy_sph_node_rtp_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtm_from_IO
!
      integer(kind = kint) :: i
!
      sph_rank_rtm(1:ithree) =    sph_rank_IO(1:ithree)
!
      nidx_global_rtm(1:ithree) = nidx_gl_sph_IO(1:ithree)
      l_truncation =              ltr_gl_IO
!
      nnod_rtm = nnod_sph_IO
      nidx_rtm(1:ithree) = nidx_sph_IO(1:ithree)
      ist_rtm(1:ithree) =  ist_sph_IO(1:ithree)
      ied_rtm(1:ithree) =  ied_sph_IO(1:ithree)
!
      call allocate_spheric_param_rtm
      call allocate_sph_1d_index_rtm
!
      do i = 1, ithree
        idx_global_rtm(1:nnod_rtm,i) = idx_gl_sph_IO(1:nnod_rtm,i)
      end do
!
      radius_1d_rtm_r(1:nidx_rtm(1)) =   r_gl_1_IO(1:nidx_rtm(1))
      idx_gl_1d_rtm_r(1:nidx_rtm(1)) =   idx_gl_1_IO(1:nidx_rtm(1))
      idx_gl_1d_rtm_t(1:nidx_rtm(2)) =   idx_gl_2_IO(1:nidx_rtm(2),1)
      idx_gl_1d_rtm_m(1:nidx_rtm(3),1) = idx_gl_3_IO(1:nidx_rtm(3),1)
      idx_gl_1d_rtm_m(1:nidx_rtm(3),2) = idx_gl_3_IO(1:nidx_rtm(3),2)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine copy_sph_node_rtm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rlm_from_IO
!
      integer(kind = kint) :: i
!
      sph_rank_rlm(1:itwo) =    sph_rank_IO(1:itwo)
!
      nidx_global_rlm(1:itwo) = nidx_gl_sph_IO(1:itwo)
      l_truncation =              ltr_gl_IO
!
      nnod_rlm = nnod_sph_IO
      nidx_rlm(1:itwo) = nidx_sph_IO(1:itwo)
      ist_rlm(1:itwo) =  ist_sph_IO(1:itwo)
      ied_rlm(1:itwo) =  ied_sph_IO(1:itwo)
!
      call allocate_spheric_param_rlm
      call allocate_sph_1d_index_rlm
!
      do i = 1, itwo
        idx_global_rlm(1:nnod_rlm,i) = idx_gl_sph_IO(1:nnod_rlm,i)
      end do
!
      radius_1d_rlm_r(1:nidx_rlm(1)) =   r_gl_1_IO(1:nidx_rlm(1))
      idx_gl_1d_rlm_r(1:nidx_rlm(1)) =   idx_gl_1_IO(1:nidx_rlm(1))
      idx_gl_1d_rlm_j(1:nidx_rlm(2),1) = idx_gl_2_IO(1:nidx_rlm(2),1)
      idx_gl_1d_rlm_j(1:nidx_rlm(2),2) = idx_gl_2_IO(1:nidx_rlm(2),2)
      idx_gl_1d_rlm_j(1:nidx_rlm(2),3) = idx_gl_2_IO(1:nidx_rlm(2),3)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine copy_sph_node_rlm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rj_from_IO
!
      integer(kind = kint) :: i
!
      sph_rank_rj(1:itwo) =     sph_rank_IO(1:itwo)
!
      nidx_global_rj(1:itwo) = nidx_gl_sph_IO(1:itwo)
      l_truncation =            ltr_gl_IO
!
      nnod_rj = nnod_sph_IO
      nidx_rj(1:itwo) = nidx_sph_IO(1:itwo)
      ist_rj(1:itwo) =  ist_sph_IO(1:itwo)
      ied_rj(1:itwo) =  ied_sph_IO(1:itwo)
!
      call allocate_spheric_param_rj
      call allocate_sph_1d_index_rj
!
      do i = 1, itwo
        idx_global_rj(1:nnod_rj,i) = idx_gl_sph_IO(1:nnod_rj,i)
      end do
!
      radius_1d_rj_r(1:nidx_rj(1)) =   r_gl_1_IO(1:nidx_rj(1))
      a_r_1d_rj_r(1:nidx_rj(1)) = one / radius_1d_rj_r(1:nidx_rj(1))
!
      idx_gl_1d_rj_r(1:nidx_rj(1)) =   idx_gl_1_IO(1:nidx_rj(1))
      idx_gl_1d_rj_j(1:nidx_rj(2),1) = idx_gl_2_IO(1:nidx_rj(2),1)
      idx_gl_1d_rj_j(1:nidx_rj(2),2) = idx_gl_2_IO(1:nidx_rj(2),2)
      idx_gl_1d_rj_j(1:nidx_rj(2),3) = idx_gl_2_IO(1:nidx_rj(2),3)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine copy_sph_node_rj_from_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtp_to_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8, nrt8
!
!
      ndir_sph_IO =              ithree
      sph_rank_IO(1:ithree) =    sph_rank_rtp(1:ithree)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ione
      ncomp_itbl_1d_IO(3) = itwo
!
      nidx_gl_sph_IO(1:ithree) = nidx_global_rtp(1:ithree)
      ltr_gl_IO =                l_truncation
!
      nnod_sph_IO = nnod_rtp
      nidx_sph_IO(1:ithree) = nidx_rtp(1:ithree)
      ist_sph_IO(1:ithree) =  ist_rtp(1:ithree)
      ied_sph_IO(1:ithree) =  ied_rtp(1:ithree)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
!$omp parallel do private(i,nr_8,nrt8)
      do i = 1, nnod_rtp
        nr_8 = nidx_global_rtp(1)
        nrt8 = nidx_global_rtp(1)*nidx_global_rtp(2)
        idx_gl_sph_IO(i,1) = idx_global_rtp(i,1)
        idx_gl_sph_IO(i,2) = idx_global_rtp(i,2)
        idx_gl_sph_IO(i,3) = idx_global_rtp(i,3)
        inod_gl_sph_IO(i) = idx_global_rtp(i,1)                         &
     &                   + (idx_global_rtp(i,2) - 1) * nr_8             &
     &                   + (idx_global_rtp(i,3) - 1) * nrt8
      end do
!$omp end parallel do
!
      r_gl_1_IO(1:nidx_rtp(1)) =     radius_1d_rtp_r(1:nidx_rtp(1))
      idx_gl_1_IO(1:nidx_rtp(1)) =   idx_gl_1d_rtp_r(1:nidx_rtp(1))
      idx_gl_2_IO(1:nidx_rtp(2),1) = idx_gl_1d_rtp_t(1:nidx_rtp(2))
      idx_gl_3_IO(1:nidx_rtp(3),1) = idx_gl_1d_rtp_p(1:nidx_rtp(3),1)
      idx_gl_3_IO(1:nidx_rtp(3),2) = idx_gl_1d_rtp_p(1:nidx_rtp(3),2)
!
      call deallocate_sph_1d_index_rtp
      call deallocate_spheric_param_rtp
!
      end subroutine copy_sph_node_rtp_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtm_to_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8, nrt8
!
!
      ndir_sph_IO =              ithree
      sph_rank_IO(1:ithree) =    sph_rank_rtm(1:ithree)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ione
      ncomp_itbl_1d_IO(3) = itwo
!
      nidx_gl_sph_IO(1:ithree) = nidx_global_rtm(1:ithree)
      ltr_gl_IO =                l_truncation
!
      nnod_sph_IO = nnod_rtm
      nidx_sph_IO(1:ithree) = nidx_rtm(1:ithree)
      ist_sph_IO(1:ithree) =  ist_rtm(1:ithree)
      ied_sph_IO(1:ithree) =  ied_rtm(1:ithree)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
!$omp parallel do private(i,nr_8,nrt8)
      do i = 1, nnod_rtm
        nr_8 = nidx_global_rtm(1)
        nrt8 = nidx_global_rtm(1)*nidx_global_rtm(2)
        idx_gl_sph_IO(i,1) = idx_global_rtm(i,1)
        idx_gl_sph_IO(i,2) = idx_global_rtm(i,2)
        idx_gl_sph_IO(i,3) = idx_global_rtm(i,3)
        inod_gl_sph_IO(i) = idx_global_rtm(i,1)                         &
     &                   + (idx_global_rtm(i,2) - 1) * nr_8             &
     &                   +  idx_global_rtm(i,3) * nrt8
      end do
!$omp end parallel do
!
      r_gl_1_IO(1:nidx_rtm(1)) =     radius_1d_rtm_r(1:nidx_rtm(1))
      idx_gl_1_IO(1:nidx_rtm(1)) =   idx_gl_1d_rtm_r(1:nidx_rtm(1))
      idx_gl_2_IO(1:nidx_rtm(2),1) = idx_gl_1d_rtm_t(1:nidx_rtm(2))
      idx_gl_3_IO(1:nidx_rtm(3),1) = idx_gl_1d_rtm_m(1:nidx_rtm(3),1)
      idx_gl_3_IO(1:nidx_rtm(3),2) = idx_gl_1d_rtm_m(1:nidx_rtm(3),2)
!
      call deallocate_sph_1d_index_rtm
      call deallocate_spheric_param_rtm
!
      end subroutine copy_sph_node_rtm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rlm_to_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
!
      ndir_sph_IO =            itwo
      sph_rank_IO(1:itwo) =    sph_rank_rlm(1:itwo)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ithree
!
      nidx_gl_sph_IO(1:itwo) = nidx_global_rlm(1:itwo)
      ltr_gl_IO =              l_truncation
!
      nnod_sph_IO = nnod_rlm
      nidx_sph_IO(1:itwo) = nidx_rlm(1:itwo)
      ist_sph_IO(1:itwo) =  ist_rlm(1:itwo)
      ied_sph_IO(1:itwo) =  ied_rlm(1:itwo)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
!$omp parallel do private(i,nr_8)
      do i = 1, nnod_rlm
        nr_8 = nidx_global_rlm(1)
        idx_gl_sph_IO(i,1) = idx_global_rlm(i,1)
        idx_gl_sph_IO(i,2) = idx_global_rlm(i,2)
        inod_gl_sph_IO(i) =  idx_global_rlm(i,1)                        &
     &                     + idx_global_rlm(i,2) * nr_8
      end do
!$omp end parallel do
      do i = 1, itwo
        idx_gl_sph_IO(1:nnod_rlm,i) = idx_global_rlm(1:nnod_rlm,i)
      end do
      inod_gl_sph_IO(1:nnod_rlm)                                        &
     &           =  idx_global_rlm(1:nnod_rlm,1)                        &
     &            + idx_global_rlm(1:nnod_rlm,2) * nidx_global_rlm(1)
!
      r_gl_1_IO(1:nidx_rlm(1)) =     radius_1d_rlm_r(1:nidx_rlm(1))
      idx_gl_1_IO(1:nidx_rlm(1)) =   idx_gl_1d_rlm_r(1:nidx_rlm(1))
      idx_gl_2_IO(1:nidx_rlm(2),1) = idx_gl_1d_rlm_j(1:nidx_rlm(2),1)
      idx_gl_2_IO(1:nidx_rlm(2),2) = idx_gl_1d_rlm_j(1:nidx_rlm(2),2)
      idx_gl_2_IO(1:nidx_rlm(2),3) = idx_gl_1d_rlm_j(1:nidx_rlm(2),3)
!
      call deallocate_sph_1d_index_rlm
      call deallocate_spheric_param_rlm
!
      end subroutine copy_sph_node_rlm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rj_to_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
      ndir_sph_IO =            itwo
      sph_rank_IO(1:itwo) =    sph_rank_rj(1:itwo)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ithree
!
      nidx_gl_sph_IO(1:itwo) = nidx_global_rj(1:itwo)
      ltr_gl_IO =              l_truncation
!
      nnod_sph_IO = nnod_rj
      nidx_sph_IO(1:itwo) = nidx_rj(1:itwo)
      ist_sph_IO(1:itwo) =  ist_rj(1:itwo)
      ied_sph_IO(1:itwo) =  ied_rj(1:itwo)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
!$omp parallel do private(i,nr_8)
      do i = 1, nnod_rj
        nr_8 = nidx_global_rj(1)
        idx_gl_sph_IO(i,1) = idx_global_rj(i,1)
        idx_gl_sph_IO(i,2) = idx_global_rj(i,2)
        inod_gl_sph_IO(i) =  idx_global_rj(i,1)                         &
     &                     + idx_global_rj(i,2) * nr_8
      end do
!$omp end parallel do
!
      if(inod_gl_sph_IO(nnod_rj) .eq. izero) then
        nr_8 = (nidx_global_rj(2) + 1)
        inod_gl_sph_IO(nnod_rj) = nidx_global_rj(1) * nr_8 + 1
      end if
!
      r_gl_1_IO(1:nidx_rj(1)) =     radius_1d_rj_r(1:nidx_rj(1))
      idx_gl_1_IO(1:nidx_rj(1)) =   idx_gl_1d_rj_r(1:nidx_rj(1))
      idx_gl_2_IO(1:nidx_rj(2),1) = idx_gl_1d_rj_j(1:nidx_rj(2),1)
      idx_gl_2_IO(1:nidx_rj(2),2) = idx_gl_1d_rj_j(1:nidx_rj(2),2)
      idx_gl_2_IO(1:nidx_rj(2),3) = idx_gl_1d_rj_j(1:nidx_rj(2),3)
!
      call deallocate_sph_1d_index_rj
      call deallocate_spheric_param_rj
!
      end subroutine copy_sph_node_rj_to_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_node_4_IO
