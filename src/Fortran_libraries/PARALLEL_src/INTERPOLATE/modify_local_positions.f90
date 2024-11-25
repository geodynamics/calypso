!>@file   modify_local_positions.f90
!!@brief  module modify_local_positions
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief find point in one element and return local coordinate
!!
!!@verbatim
!!      subroutine s_modify_local_positions(maxitr, eps_iter, xi,       &
!!     &          x_target, nnod_ele_2, x_local_ele, iflag_message,     &
!!     &          differ, ierr_modify)
!!      subroutine modify_local_positions_no_fix(maxitr, eps_iter, xi,  &
!!     &          x_target, nnod_ele_2, x_local_ele, iflag_message,     &
!!     &          differ, ierr_modify)
!!@verbatim
!
      module modify_local_positions
!
      use m_precision
!
      use solver_33_array
      use cal_position_and_grad
!
        implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_modify_local_positions(maxitr, eps_iter, xi,         &
     &          x_target, nnod_ele_2, x_local_ele, iflag_message,       &
     &          differ, ierr_modify)
!
      integer(kind= kint), intent(in) :: iflag_message
      integer(kind= kint), intent(in) :: maxitr
      real(kind = kreal), intent(in) :: x_target(3)
      real(kind = kreal), intent(in) :: eps_iter
      integer(kind= kint), intent(in) :: nnod_ele_2
      real(kind = kreal), intent(in) :: x_local_ele(nnod_ele_2,3)
!
      real(kind = kreal), intent(inout) :: differ
      real(kind = kreal), intent(inout) :: xi(3)
      integer (kind = kint), intent(inout) :: ierr_modify
!
      integer (kind = kint) :: nd, iter
      real(kind=kreal) :: s_correct(3), differ_prev
!
      real(kind=kreal) :: dx(3), xx_z(3)
      real(kind=kreal) :: dnxi(3), dnei(3), dnzi(3)
      real(kind=kreal) :: dnxi_mat(3,3)
!
!
      ierr_modify = maxitr + 1
      do nd = 1, 3
        if ( xi(nd) .gt.  1.0d0 ) xi(nd) = 1.0d0
        if ( xi(nd) .lt. -1.0d0 ) xi(nd) =-1.0d0
      end do
!
      do iter = 1, maxitr
        call cal_position_and_gradient(nnod_ele_2, xx_z,                &
     &      dnxi, dnei, dnzi, x_local_ele, xi)
!
        dx(1:3) = xx_z(1:3)-x_target(1:3)
        differ = sqrt( dx(1)**2 + dx(2)**2 + dx(3)**2 )
!
!        if (iflag_message .eq. 1) then
!          write(60+my_rank,*) 'iteration, differ', iter, differ
!          write(60+my_rank,*) xx_z(1:3)
!          write(60+my_rank,*) x_target(1:3)
!          write(60+my_rank,*) dnxi, dnei, dnzi
!        end if
!
!
        if ( differ .lt. eps_iter) then
          ierr_modify = iter
          exit
        end if
!
        if (iter.gt.3 .and. abs(differ-differ_prev).lt.eps_iter) then
          ierr_modify = -iter
          exit
        end if
        differ_prev = differ
!
        dnxi_mat(1:3,1) = dnxi(1:3)
        dnxi_mat(1:3,2) = dnei(1:3)
        dnxi_mat(1:3,3) = dnzi(1:3)
        call solve_33_array(s_correct, dx, dnxi_mat)
!
        xi(1:3) = xi(1:3) - s_correct(1:3)
!
        do nd = 1, 3
          if ( xi(nd) .gt.  1.0d0 ) xi(nd) = 1.0d0
          if ( xi(nd) .lt. -1.0d0 ) xi(nd) =-1.0d0
        end do
!
      end do
!
      end subroutine s_modify_local_positions
!
!-----------------------------------------------------------------------
!
      subroutine modify_local_positions_no_fix(maxitr, eps_iter, xi,    &
     &          x_target, nnod_ele_2, x_local_ele, iflag_message,       &
     &          differ, ierr_modify)
!
      integer(kind= kint), intent(in) :: iflag_message
      integer(kind= kint), intent(in) :: maxitr
      real(kind = kreal), intent(in) :: x_target(3)
      real(kind = kreal), intent(in) :: eps_iter
      integer(kind= kint), intent(in) :: nnod_ele_2
      real(kind = kreal), intent(in) :: x_local_ele(nnod_ele_2,3)
!
      real(kind = kreal), intent(inout) :: differ
      real(kind = kreal), intent(inout) :: xi(3)
      integer (kind = kint), intent(inout) :: ierr_modify
!
      integer (kind = kint) :: nd, iter
      real(kind=kreal) :: s_correct(3)
!
      real(kind=kreal) :: dx(3), xx_z(3)
      real(kind=kreal) :: dnxi(3), dnei(3), dnzi(3)
      real(kind=kreal) :: dnxi_mat(3,3)
!
!
      ierr_modify = maxitr + 1
      do iter = 1, maxitr
        call cal_position_and_gradient(nnod_ele_2, xx_z,                &
     &      dnxi, dnei, dnzi, x_local_ele, xi)
!
        dx(1:3) = xx_z(1:3)-x_target(1:3)
        differ = sqrt( dx(1)**2 + dx(2)**2 + dx(3)**2 )
!
!        if (iflag_message .eq. 1) then
!          write(60+my_rank,*) 'iteration, differ', iter, differ
!          write(60+my_rank,*) xx_z(1:3)
!          write(60+my_rank,*) x_target(1:3)
!          write(60+my_rank,*) dnxi, dnei, dnzi
!        end if
!
        if ( differ .lt. eps_iter) then
          ierr_modify = iter
          exit
        end if
!
        dnxi_mat(1:3,1) = dnxi(1:3)
        dnxi_mat(1:3,2) = dnei(1:3)
        dnxi_mat(1:3,3) = dnzi(1:3)
        call solve_33_array(s_correct, dx, dnxi_mat)
!
        xi(1:3) = xi(1:3) - s_correct(1:3)
!
      end do
!
      end subroutine modify_local_positions_no_fix
!
!-----------------------------------------------------------------------
!
      end module modify_local_positions
