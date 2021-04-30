!>@file   diff_geometory_comm_test.f90
!!@brief  module diff_geometory_comm_test
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine set_node_4_comm_test(numnod, internal_node,          &
!!     &          inod_gl, xx, i_gl_test, xx_test)
!!      subroutine set_element_4_comm_test(numele, interior_flag,       &
!!     &                                   x_ele,  xx_test)
!!
!!      integer(kind = kint) function count_node_comm_test              &
!!     &            (num_d, inod_gl, x_org, icomm_gl, x_comm)
!!      subroutine compare_nod_comm_test                                &
!!     &         (num_d, inod_gl, x_org, icomm_gl, x_comm,              &
!!     &          num_diff_l, id_diff, x_diff)
!!
!!      integer(kind = kint) function count_ele_comm_test               &
!!     &                            (num_d, x_org, x_comm)
!!      subroutine compare_ele_comm_test(num_d, x_org, x_comm,          &
!!     &                                 num_diff_l, id_diff, x_diff)
!!@endverbatim
!
!
      module diff_geometory_comm_test
!
      use m_precision
      use m_constants
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_node_4_comm_test(numnod, internal_node,            &
     &          inod_gl, xx, i_gl_test, xx_test)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint_gl), intent(in) :: inod_gl(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint_gl), intent(inout) :: i_gl_test(numnod)
      real(kind = kreal), intent(inout) ::      xx_test(3*numnod)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, internal_node
        i_gl_test(inod) = inod_gl(inod)
        xx_test(3*inod-2) =   xx(inod,1)
        xx_test(3*inod-1) =   xx(inod,2)
        xx_test(3*inod  ) =   xx(inod,3)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod =  internal_node+1, numnod
        i_gl_test(inod) = 0
        xx_test(3*inod-2) =   0.0d0
        xx_test(3*inod-1) =   0.0d0
        xx_test(3*inod  ) =   0.0d0
      end do
!$omp end parallel do
!
!
      end subroutine set_node_4_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine set_element_4_comm_test(numele, interior_flag,         &
     &                                   x_ele, xx_test)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: interior_flag(numele)

      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      real(kind = kreal), intent(inout) ::      xx_test(3*numele)
!
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
      do iele = 1, numele
        if(interior_flag(iele) .eq. 0) then
          xx_test(3*iele-2) = 0.0d0
          xx_test(3*iele-1) = 0.0d0
          xx_test(3*iele  ) = 0.0d0
        else
          xx_test(3*iele-2) = x_ele(iele,1)
          xx_test(3*iele-1) = x_ele(iele,2)
          xx_test(3*iele  ) = x_ele(iele,3)
        end if
      end do
!$omp end parallel do
!
      end subroutine set_element_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function count_node_comm_test                &
     &               (num_d, inod_gl, x_org, icomm_gl, x_comm)
!
      integer(kind = kint), intent(in) :: num_d
      integer(kind = kint_gl), intent(in) :: inod_gl(num_d)
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint_gl), intent(in) :: icomm_gl(num_d)
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint) :: num_diff_l
      integer(kind = kint) :: inod
      real(kind = kreal) :: diff
!
      num_diff_l = 0
      do inod = 1, num_d
        diff =  abs(x_comm(3*inod-2) - x_org(inod,1))                   &
     &        + abs(x_comm(3*inod-1) - x_org(inod,2))                   &
     &        + abs(x_comm(3*inod  ) - x_org(inod,3))
        if(inod_gl(inod) .ne. icomm_gl(inod)                            &
     &    .or.  diff .gt. TINY) num_diff_l = num_diff_l + 1
      end do
      count_node_comm_test =num_diff_l
!
      end function count_node_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine compare_nod_comm_test                                  &
     &         (num_d, inod_gl, x_org, icomm_gl, x_comm,                &
     &          num_diff_l, id_diff, x_diff)
!
      integer(kind = kint), intent(in) :: num_d
      integer(kind = kint_gl), intent(in) :: inod_gl(num_d)
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint_gl), intent(in) :: icomm_gl(num_d)
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: inod, icou
      real(kind = kreal) :: diff
!
      if(num_diff_l .le. 0) return
      icou = 0
      do inod = 1, num_d
        diff =  abs(x_comm(3*inod-2) - x_org(inod,1))                   &
     &        + abs(x_comm(3*inod-1) - x_org(inod,2))                   &
     &        + abs(x_comm(3*inod  ) - x_org(inod,3))
        if(inod_gl(inod) .ne. icomm_gl(inod)                            &
     &     .or.   diff .gt. TINY)  then
          icou = icou + 1
          id_diff(icou) =        inod
          x_diff(6*icou-5) =      x_org(inod,1)
          x_diff(6*icou-4) =      x_org(inod,2)
          x_diff(6*icou-3) =      x_org(inod,3)
          x_diff(6*icou-2) =      x_comm(3*inod-2)
          x_diff(6*icou-1) =      x_comm(3*inod-1)
          x_diff(6*icou  ) =      x_comm(3*inod  )
        end if
      end do
!
      end subroutine compare_nod_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function count_ele_comm_test                 &
     &                            (num_d, x_org, x_comm)
!
      integer(kind = kint), intent(in) :: num_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint) :: num_diff_l
      integer(kind = kint) :: iele
      real(kind = kreal) :: diff
!
      num_diff_l = 0
      do iele = 1, num_d
        diff =  abs(x_comm(3*iele-2) - x_org(iele,1))                   &
     &        + abs(x_comm(3*iele-1) - x_org(iele,2))                   &
     &        + abs(x_comm(3*iele  ) - x_org(iele,3))
        if (diff .gt. TINY) num_diff_l = num_diff_l + 1
      end do
      count_ele_comm_test = num_diff_l
!
      end function count_ele_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine compare_ele_comm_test(num_d, x_org, x_comm,            &
     &                                 num_diff_l, id_diff, x_diff)
!
      integer(kind = kint), intent(in) :: num_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: iele, icou
      real(kind = kreal) :: diff
!
!
      if(num_diff_l .le. 0) return
      icou = 0
      do iele = 1, num_d
        diff =  abs(x_comm(3*iele-2) - x_org(iele,1))                   &
     &        + abs(x_comm(3*iele-1) - x_org(iele,2))                   &
     &        + abs(x_comm(3*iele  ) - x_org(iele,3))
        if (diff .gt. TINY) then
          icou = icou + 1
          id_diff(icou) =        iele
          x_diff(6*icou-5) =      x_org(iele,1)
          x_diff(6*icou-4) =      x_org(iele,2)
          x_diff(6*icou-3) =      x_org(iele,3)
          x_diff(6*icou-2) =      x_comm(3*iele-2)
          x_diff(6*icou-1) =      x_comm(3*iele-1)
          x_diff(6*icou  ) =      x_comm(3*iele  )
        end if
      end do
!
      end subroutine compare_ele_comm_test
!
! ----------------------------------------------------------------------
!
      end module diff_geometory_comm_test
