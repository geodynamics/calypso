!>@file   set_special_sph_lm_flags.f90
!!@brief  module set_special_sph_lm_flags
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Communication tables for spherical transform
!!
!!@verbatim
!!      subroutine set_special_degree_order_flags(nidx_rj2, nidx_rlm2,  &
!!     &          idx_gl_1d_rj_j, idx_gl_1d_rlm_j, idx_rj_degree_zero,  &
!!     &          idx_rj_degree_one, ist_rtm_order_zero,                &
!!     &          ist_rtm_order_1s, ist_rtm_order_1c)
!!@endverbatim
!
      module set_special_sph_lm_flags
!
      use m_precision
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
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
      integer(kind = kint) :: j, jj
!
!
!   ------- search degree zero
!
      idx_rj_degree_zero = 0
      do j = 1, nidx_rj2
        if (idx_gl_1d_rj_j(j,2) .eq. 0) then
          idx_rj_degree_zero = j
          exit
        end if
      end do
!
!   ------- search degree one
!
      idx_rj_degree_one(-1:1) = 0
      do jj = -1, 1
        do j = 1, nidx_rj2
          if (idx_gl_1d_rj_j(j,1) .eq. (jj+2) ) then
            idx_rj_degree_one(jj) = j
            exit
          end if
        end do
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
      end module set_special_sph_lm_flags
