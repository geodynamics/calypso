!>@file   t_local_fline.f90
!!@brief  module t_local_fline
!!
!!@author H.Matsui
!!@date      Programmed in June, 2024
!
!>@brief  local field line and tracer data structure
!!
!!@verbatim
!!      subroutine reset_fline_start(fline_lc)
!!      subroutine alloc_local_fline(ele, fline_lc)
!!      subroutine dealloc_local_fline(fline_lc)
!!        type(element_data), intent(in) :: ele
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine raise_local_fline_connect(fline_lc)
!!      subroutine raise_local_fline_data(fline_lc)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine alloc_local_fline_conn(nele_buf, fline_lc)
!!        integer(kind = kint), intent(in) :: nele_buf
!!        type(local_fieldline), intent(inout) :: fline_lc
!!      subroutine alloc_local_fline_data(nnod_buf, fline_lc)
!!        integer(kind = kint), intent(in) :: nnod_buf
!!        type(local_fieldline), intent(inout) :: fline_lc
!!      subroutine dealloc_local_fline_conn(fline_lc)
!!      subroutine dealloc_local_fline_data(fline_lc)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module t_local_fline
!
      use m_precision
      use m_constants
      use t_find_interpolate_in_ele
!
      implicit  none
!
      type local_fieldline
        integer(kind = kint) :: nele_line_buf
        integer(kind = kint) :: nele_line_l
        integer(kind = kint), allocatable :: iedge_line_l(:,:)
!
        integer(kind = kint) :: nnod_line_buf
        integer(kind = kint) :: nnod_line_l
        integer(kind = kint_gl), allocatable :: iglobal_fline(:)
        integer(kind = kint), allocatable :: iele_fline(:)
        real(kind = kreal), allocatable ::   xx_line_l(:,:)
        real(kind = kreal), allocatable ::   xi_line_l(:,:)
!
        type(cal_interpolate_coefs_work) :: itp_ele_work_l
      end type local_fieldline
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reset_fline_start(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      fline_lc%nnod_line_l = 0
      fline_lc%nele_line_l = 0
!
      end subroutine reset_fline_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline(ele, fline_lc)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      call reset_fline_start(fline_lc)
!
      call alloc_local_fline_conn(ione, fline_lc)
      call alloc_local_fline_data(itwo, fline_lc)
!
      call alloc_work_4_interpolate(ele%nnod_4_ele,                     &
     &                              fline_lc%itp_ele_work_l)
!
      end subroutine alloc_local_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_fline(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      call dealloc_work_4_interpolate(fline_lc%itp_ele_work_l)
!
      call dealloc_local_fline_conn(fline_lc)
      call dealloc_local_fline_data(fline_lc)
!
      end subroutine dealloc_local_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_connect(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
      type(local_fieldline) :: fline_tmp
!
!
      fline_tmp%nele_line_l = fline_lc%nele_line_l
      call alloc_local_fline_conn(fline_lc%nele_line_buf, fline_tmp)
      call copy_local_fline_connect(fline_lc%nele_line_l, fline_lc,     &
     &                              fline_tmp)
!
      call dealloc_local_fline_conn(fline_lc)
      call alloc_local_fline_conn((itwo*fline_lc%nele_line_l),          &
     &                             fline_lc)
!
      call copy_local_fline_connect(fline_lc%nele_line_l, fline_tmp,    &
     &                              fline_lc)
      call dealloc_local_fline_conn(fline_tmp)
!
      end subroutine raise_local_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_data(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
      type(local_fieldline) :: fline_tmp
!
!
      fline_tmp%nnod_line_l = fline_lc%nnod_line_l
      call alloc_local_fline_data(fline_lc%nnod_line_buf, fline_tmp)
      call copy_local_fline_data(fline_lc%nnod_line_l, fline_lc,        &
     &                           fline_tmp)
!
      call dealloc_local_fline_data(fline_lc)
      call alloc_local_fline_data((itwo*fline_lc%nnod_line_l),          &
     &                            fline_lc)
!
      call copy_local_fline_data(fline_lc%nnod_line_l, fline_tmp,       &
     &                           fline_lc)
      call dealloc_local_fline_data(fline_tmp)
!
      end subroutine raise_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fline_connect(nele_copy, fline_lc,          &
     &                                    fline_new)
!
      integer(kind = kint), intent(in) :: nele_copy
      type(local_fieldline), intent(in) :: fline_lc
      type(local_fieldline), intent(inout) :: fline_new
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, nele_copy
        fline_new%iedge_line_l(1:2,i) = fline_lc%iedge_line_l(1:2,i)
      end do
!$omp end parallel do
!
      end subroutine copy_local_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fline_data(num_copy, fline_lc, fline_new)
!
      integer(kind = kint), intent(in) :: num_copy
      type(local_fieldline), intent(in) :: fline_lc
      type(local_fieldline), intent(inout) :: fline_new
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, num_copy
        fline_new%iglobal_fline(i) = fline_lc%iglobal_fline(i)
        fline_new%iele_fline(i) =    fline_lc%iele_fline(i)
        fline_new%xx_line_l(1:4,i) = fline_lc%xx_line_l(1:4,i)
        fline_new%xi_line_l(1:4,i) = fline_lc%xi_line_l(1:4,i)
      end do
!$omp end parallel do
!
      end subroutine copy_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline_conn(nele_buf, fline_lc)
!
      integer(kind = kint), intent(in) :: nele_buf
      type(local_fieldline), intent(inout) :: fline_lc
!
      fline_lc%nele_line_buf = nele_buf
      allocate(fline_lc%iedge_line_l(2,fline_lc%nele_line_buf))
      if(fline_lc%nele_line_buf .gt. 0) fline_lc%iedge_line_l =  0
!
      end subroutine alloc_local_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline_data(nnod_buf, fline_lc)
!
      integer(kind = kint), intent(in) :: nnod_buf
      type(local_fieldline), intent(inout) :: fline_lc
!
      fline_lc%nnod_line_buf = nnod_buf
      allocate(fline_lc%iglobal_fline(fline_lc%nnod_line_buf))
      allocate(fline_lc%iele_fline(fline_lc%nnod_line_buf))
      allocate(fline_lc%xx_line_l(4,fline_lc%nnod_line_buf))
      allocate(fline_lc%xi_line_l(4,fline_lc%nnod_line_buf))
!
      if(fline_lc%nnod_line_buf .le. 0) return
!$omp parallel workshare
      fline_lc%iglobal_fline(1:fline_lc%nnod_line_buf) = 0
      fline_lc%iele_fline(1:fline_lc%nnod_line_buf) =    0
      fline_lc%xx_line_l(1:4,1:fline_lc%nnod_line_buf) = 0.0d0
      fline_lc%xi_line_l(1:4,1:fline_lc%nnod_line_buf) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_fline_conn(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      deallocate(fline_lc%iedge_line_l)
!
      end subroutine dealloc_local_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_fline_data(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      deallocate(fline_lc%iele_fline, fline_lc%iglobal_fline)
      deallocate(fline_lc%xx_line_l, fline_lc%xi_line_l)
!
      end subroutine dealloc_local_fline_data
!
!  ---------------------------------------------------------------------
!
      end module t_local_fline
