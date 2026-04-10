!>@file   t_find_interpolate_in_ele.f90
!!@brief  module t_find_interpolate_in_ele
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief find point in one element and return local coordinate
!!
!!@verbatim
!!      subroutine alloc_work_4_interpolate(nnod_4_ele, itp_ele_work)
!!      subroutine dealloc_work_4_interpolate(itp_ele_work)
!!        type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!!
!!      subroutine find_interpolate_in_ele(x_target, maxitr, eps_iter,  &
!!     &          my_rank, iflag_message, error_level,                  &
!!     &          org_node, org_ele, jele, itp_ele_work, xi, ierr_inter)
!!        real(kind=kreal), intent(in) :: x_target(3)
!!        real(kind=kreal), intent(in) :: eps_iter
!!        integer(kind = kint), intent(in) :: maxitr
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        integer(kind = kint), intent(in) :: jele
!!        integer, intent(in) :: my_rank
!!        integer(kind = kint), intent(in) :: iflag_message
!!        real(kind = kreal), intent(in) :: error_level
!!        type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!!        integer(kind = kint), intent(inout) :: ierr_inter
!!        real(kind=kreal), intent(inout)  :: xi(3)
!!@endverbatim
!
      module t_find_interpolate_in_ele
!
      use m_precision
      use m_constants
!
      implicit none
!
      type cal_interpolate_coefs_work
        real(kind=kreal), allocatable :: coefs_by_tet(:)
        real(kind=kreal), allocatable :: x_local_ele(:,:)
        real(kind=kreal) :: differ_tmp
        real(kind=kreal) :: differ_res
!
        integer(kind = kint) :: iflag_org_tmp
      end type cal_interpolate_coefs_work
!
      private :: adjust_to_corner, force_interpolate_in_element
      private :: cal_3vector_4_tet_2nd
!      private ::  copy_position_2_2nd_local_ele
      private :: check_solution_in_tet
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_4_interpolate(nnod_4_ele, itp_ele_work)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
      allocate(itp_ele_work%coefs_by_tet(nnod_4_ele))
      allocate(itp_ele_work%x_local_ele(nnod_4_ele,3))
!
      itp_ele_work%coefs_by_tet = 0.0d0
      itp_ele_work%x_local_ele = 0.0d0
      itp_ele_work%differ_tmp = 0.0d0
      itp_ele_work%differ_res = 0.0d0
!
      end subroutine alloc_work_4_interpolate
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_4_interpolate(itp_ele_work)
!
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
      deallocate(itp_ele_work%coefs_by_tet)
      deallocate(itp_ele_work%x_local_ele)
!
      end subroutine dealloc_work_4_interpolate
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine find_interpolate_in_ele(x_target, maxitr, eps_iter,    &
     &          my_rank, iflag_message, error_level,                    &
     &          org_node, org_ele, jele, itp_ele_work, xi, ierr_inter)
!
      use m_connect_hexa_2_tetra
      use cal_local_position_by_tetra
      use modify_local_positions
      use solver_33_array
!
      use t_geometry_data
      use t_interpolate_coefs_dest
!
      real(kind=kreal), intent(in) :: x_target(3)
      real(kind=kreal), intent(in) :: eps_iter
      integer(kind = kint), intent(in) :: maxitr
!
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      integer(kind = kint), intent(in) :: jele
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: iflag_message
      real(kind = kreal), intent(in) :: error_level
!
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
      integer(kind = kint), intent(inout) :: ierr_inter
      real(kind=kreal), intent(inout)  :: xi(3)
!
      real(kind=kreal) :: s_coef(3)
      real(kind=kreal) :: v_tetra(3,3)
      real(kind=kreal) :: v_target(3)
      real(kind=kreal) :: ref_error
!
      integer (kind = kint) :: itet, i
!
!
      ierr_inter = 1
!
      call copy_position_2_2nd_local_ele(org_node, org_ele,             &
     &    jele, itp_ele_work%x_local_ele)
!
      do itet = 1, num_tetra
        call cal_3vector_4_tet_2nd(org_ele%nnod_4_ele, itet,            &
     &      v_target, v_tetra, x_target, itp_ele_work%x_local_ele)
!
!   solve equations
!
        call solve_33_array(s_coef, v_target, v_tetra)
!
!   check solution
!
        call check_solution_in_tet(ref_error, s_coef)
!
!    satisfy the error level
!
        if(abs(ref_error) .le. error_level) then
!
          call init_coefs_on_tet(org_ele%nnod_4_ele, itet,              &
     &        itp_ele_work%coefs_by_tet, s_coef)
!
          call s_cal_local_position_by_tetra(org_ele%nnod_4_ele, xi,    &
     &        itp_ele_work%coefs_by_tet)
!
          if (iflag_message .eq. 1) then
            do i = 1, org_ele%nnod_4_ele
              write(my_rank+60,*) i, jele,                              &
     &                           itp_ele_work%x_local_ele(i,1:3)
            end do
            write(my_rank+60,*) 's_coef', s_coef
          end if
!
!     improve solution
!
          call s_modify_local_positions(maxitr, eps_iter, xi, x_target, &
     &        org_ele%nnod_4_ele, itp_ele_work%x_local_ele,             &
     &        iflag_message, itp_ele_work%differ_res, ierr_inter)
!
!     finish improvement
!
          if(ierr_inter.gt.0 .and. ierr_inter.le.maxitr) then
             call adjust_to_corner(eps_iter, xi)
             return
          else
            if (iflag_message .eq. 1) then
              write(my_rank+60,*)                                       &
     &               'improvement failed!!: jele, itet:', my_rank,      &
     &                itp_ele_work%differ_tmp, itp_ele_work%differ_res, &
     &                 jele, itet, xi
              if (itp_ele_work%differ_res                               &
     &              .lt. itp_ele_work%differ_tmp) then
                call force_interpolate_in_element(xi)
                itp_ele_work%differ_tmp = itp_ele_work%differ_res
              end if
            end if
          end if
!
        end if
      end do
!
      end subroutine find_interpolate_in_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine adjust_to_corner(epcilon, xi)
!
      real(kind = kreal), intent(in) :: epcilon
      real(kind = kreal), intent(inout) :: xi(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        if(abs(xi(nd) - one) .lt. epcilon) then
          xi(nd) = one
        else if(abs(xi(nd) + one) .lt. epcilon) then
          xi(nd) = -one
        end if
      end do
!
      end subroutine adjust_to_corner
!
!-----------------------------------------------------------------------
!
      subroutine force_interpolate_in_element(xi)
!
      real(kind = kreal), intent(inout) :: xi(3)
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        if ( xi(nd) .gt. one) then
          xi(nd) = one
        else if ( xi(nd) .lt. -one) then
          xi(nd) = -one
        end if
      end do
!
      end subroutine force_interpolate_in_element
!
!-----------------------------------------------------------------------
!
      subroutine init_coefs_on_tet(nnod_4_ele_2, itet, coefs_by_tet, s)
!
      use m_constants
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: itet, nnod_4_ele_2
      real(kind = kreal), intent(in) ::    s(3)
      real(kind = kreal), intent(inout) :: coefs_by_tet(nnod_4_ele_2)
!
      integer(kind = kint) :: i1, i2, i3, i4
!
!
        coefs_by_tet(1:nnod_4_ele_2) = zero
!
        i1 = ie_tetra(1,itet)
        i2 = ie_tetra(2,itet)
        i3 = ie_tetra(3,itet)
        i4 = ie_tetra(4,itet)
!
        coefs_by_tet(i1) = one - ( s(1) + s(2) + s(3) )
        coefs_by_tet(i2) = s(1)
        coefs_by_tet(i3) = s(2)
        coefs_by_tet(i4) = s(3)
!
      end subroutine init_coefs_on_tet
!
!-----------------------------------------------------------------------
!
      subroutine cal_3vector_4_tet_2nd(nnod_4_ele_2, itet,              &
     &          v_target, v_tetra, x_target, x_local)
!
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: itet, nnod_4_ele_2
      real(kind = kreal), intent(in) :: x_target(3)
      real(kind = kreal), intent(in) :: x_local(nnod_4_ele_2,3)
!
      real(kind = kreal), intent(inout) :: v_target(3)
      real(kind = kreal), intent(inout) :: v_tetra(3,3)
!
      integer(kind = kint) :: nd
!
!
       do nd = 1, 3
!
         v_target(nd) =  x_target(nd)                                   &
     &                    - x_local(ie_tetra(1,itet),nd)
!
         v_tetra(nd,1) = x_local(ie_tetra(2,itet),nd)                   &
     &                    - x_local(ie_tetra(1,itet),nd)
         v_tetra(nd,2) = x_local(ie_tetra(3,itet),nd)                   &
     &                    - x_local(ie_tetra(1,itet),nd)
         v_tetra(nd,3) = x_local(ie_tetra(4,itet),nd)                   &
     &                    - x_local(ie_tetra(1,itet),nd)
       end do
!
      end subroutine cal_3vector_4_tet_2nd
!
!-----------------------------------------------------------------------
!
      subroutine copy_position_2_2nd_local_ele(new_node, new_ele,       &
     &          iele, x_local)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: iele
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      real(kind = kreal), intent(inout)                                 &
     &       :: x_local(new_ele%nnod_4_ele,3)
!
      integer(kind = kint) :: i, inod
!
!
       do i = 1, new_ele%nnod_4_ele
         inod = new_ele%ie(iele,i)
         x_local(i,1:3) = new_node%xx(inod,1:3)
       end do
!
      end subroutine copy_position_2_2nd_local_ele
!
!-----------------------------------------------------------------------
!
      subroutine check_solution_in_tet(ref_error, s_coef)
!
      use m_constants
!
      real(kind = kreal), intent(in) ::    s_coef(3)
      real(kind = kreal), intent(inout) :: ref_error
!
      real(kind = kreal) :: sum_tmp
      integer (kind = kint) :: nd
!
!
       sum_tmp = s_coef(1) + s_coef(2) + s_coef(3)
!
       ref_error = zero
       do nd = 1, 3
         if ( s_coef(nd) .lt. zero) then
           ref_error = ref_error - s_coef(nd)
         else if ( s_coef(nd) .gt. one) then
           ref_error = ref_error + s_coef(nd) - one
         end if
       end do
!
       if ( sum_tmp .lt. zero) then
         ref_error = ref_error - sum_tmp
       else if ( sum_tmp .gt. one) then
         ref_error = ref_error + sum_tmp - one
       end if
!
      end subroutine check_solution_in_tet
!
!-----------------------------------------------------------------------
!
      end module t_find_interpolate_in_ele
