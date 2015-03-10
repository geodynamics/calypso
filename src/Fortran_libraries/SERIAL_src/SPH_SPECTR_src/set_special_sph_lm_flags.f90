!>@file   set_special_sph_lm_flags.f90
!!@brief  module set_special_sph_lm_flags
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Communication tables for spherical transform
!!
!!@verbatim
!!      subroutine set_sph_rj_center_flag                               &
!!     &         (nnod_rj, nidx_rj, inod_rj_center)
!!      subroutine set_special_degree_order_flags(nidx_rj2, nidx_rlm2,  &
!!     &          idx_gl_1d_rj_j, idx_gl_1d_rlm_j, idx_rj_degree_zero,  &
!!     &          idx_rj_degree_one, ist_rtm_order_zero,                &
!!     &          ist_rtm_order_1s, ist_rtm_order_1c)
!!      integer function find_local_rj_mode_address                     &
!!     &      (jmax, idx_gl_1d_rj_j, l, m)
!!@endverbatim
!
      module set_special_sph_lm_flags
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_rj_center_flag                                 &
     &         (nnod_rj, nidx_rj, inod_rj_center)
!
      integer (kind=kint), intent(in) :: nnod_rj
      integer (kind=kint), intent(in) :: nidx_rj(2)
      integer (kind=kint), intent(inout) :: inod_rj_center
!
!
      inod_rj_center = nnod_rj * (nnod_rj - nidx_rj(1)*nidx_rj(2))
!
      end subroutine set_sph_rj_center_flag
!
! ----------------------------------------------------------------------
!
      subroutine set_special_degree_order_flags(nidx_rj2, nidx_rlm2,    &
     &          idx_gl_1d_rj_j, idx_gl_1d_rlm_j, idx_rj_degree_zero,    &
     &          idx_rj_degree_one, ist_rtm_order_zero,                  &
     &          ist_rtm_order_1s, ist_rtm_order_1c)
!
      integer (kind=kint), intent(in) :: nidx_rj2, nidx_rlm2
      integer (kind=kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj2,3)
      integer (kind=kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm2,3)
!
      integer (kind=kint), intent(inout) :: idx_rj_degree_zero
      integer (kind=kint), intent(inout) :: idx_rj_degree_one(-1:1)
!
      integer (kind=kint), intent(inout) :: ist_rtm_order_zero
      integer (kind=kint), intent(inout) :: ist_rtm_order_1s
      integer (kind=kint), intent(inout) :: ist_rtm_order_1c
!
      integer(kind = kint) :: j, m
!
!
!   ------- search degree zero
!
      idx_rj_degree_zero = find_local_rj_mode_address(nidx_rj2,         &
     &                    idx_gl_1d_rj_j, izero, izero)
!
!   ------- search degree one
!
      do m = -1, 1
        idx_rj_degree_one(m) = find_local_rj_mode_address(nidx_rj2,     &
     &                        idx_gl_1d_rj_j, ione, m)
      end do
!
!   ------- search order zero
!
      ist_rtm_order_zero = 0
      do j = 1, nidx_rlm2
        if (idx_gl_1d_rlm_j(j,3) .eq.  0) then
          ist_rtm_order_zero = j
          exit
        end if
      end do
!
      ist_rtm_order_1s = 0
      do j = 1, nidx_rlm2
        if (idx_gl_1d_rlm_j(j,3) .eq. -1) then
          ist_rtm_order_1s = j
          exit
        end if
      end do
!
      ist_rtm_order_1c = 0
      do j = 1, nidx_rlm2
        if (idx_gl_1d_rlm_j(j,3) .eq.  1) then
          ist_rtm_order_1c = j
          exit
        end if
      end do
!
      end subroutine set_special_degree_order_flags
!
!  -------------------------------------------------------------------
!
      integer function find_local_rj_mode_address                       &
     &      (jmax, idx_gl_1d_rj_j, l, m)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(jmax,3)
      integer(kind = kint), intent(in) :: l, m
!
      integer(kind = kint) :: j
!
!
      find_local_rj_mode_address = 0
      do j = 1, jmax
        if (   idx_gl_1d_rj_j(j,2) .eq. l                               &
     &   .and. idx_gl_1d_rj_j(j,3) .eq. m) then
          find_local_rj_mode_address = j
          return
        end if
      end do
!
      end function find_local_rj_mode_address
!
!-----------------------------------------------------------------------
!
      end module set_special_sph_lm_flags
