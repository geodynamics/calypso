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
!!      subroutine add_fline_start(xx4_add, v4_add, ntot_comp, col_add, &
!!     &                           fline_lc)
!!      subroutine alloc_local_fline(viz_fields, fline_lc)
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine dealloc_local_fline(fline_lc)
!!      subroutine add_fline_list(iglobal_add, xx4_add, v4_add,         &
!!     &                          ntot_comp, col_add, fline_lc)
!!        integer(kind = kint_gl), intent(in) :: iglobal_add
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: xx4_add(4),
!!        real(kind = kreal), intent(in) :: col_add(ntot_comp)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine raise_local_fline_connect(fline_lc)
!!      subroutine raise_local_fline_data(fline_lc)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine check_local_fline(id_file, fline_lc)
!!        type(local_fieldline), intent(in) :: fline_lc
!!@endverbatim
!
      module t_local_fline
!
      use m_precision
      use m_constants
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
        integer(kind = kint) :: ntot_comp_l
        integer(kind = kint_gl), allocatable :: iglobal_fline(:)
        real(kind = kreal), allocatable ::   xx_line_l(:,:)
        real(kind = kreal), allocatable ::   v_line_l(:,:)
        real(kind = kreal), allocatable ::   col_line_l(:,:)
!
        integer(kind = kint), allocatable :: iedge_line_tmp(:,:)
        integer(kind = kint_gl), allocatable :: iglobal_tmp(:)
        real(kind = kreal), allocatable ::   xx_line_tmp(:,:)
        real(kind = kreal), allocatable ::   v_line_tmp(:,:)
        real(kind = kreal), allocatable ::   col_line_tmp(:,:)
      end type local_fieldline
!
      private :: alloc_local_fline_data
      private :: dealloc_local_fline_conn, dealloc_local_fline_data
      private :: allocate_local_fline_conn_tmp
      private :: allocate_local_fline_data_tmp
      private :: deallocate_local_fline_conn_tmp
      private :: deallocate_local_fline_data_tmp
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
!
      subroutine add_fline_start(xx4_add, v4_add, ntot_comp, col_add,   &
     &                           fline_lc)
!
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: xx4_add(4), v4_add(4)
      real(kind = kreal), intent(in) :: col_add(ntot_comp)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%v_line_l(1:3,fline_lc%nnod_line_l) =  v4_add(1:3)
      fline_lc%col_line_l(1:ntot_comp,fline_lc%nnod_line_l)             &
     &       = col_add(1:ntot_comp)
!
      end subroutine add_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_list(iglobal_add, xx4_add, v4_add,           &
     &                          ntot_comp, col_add, fline_lc)
!
      integer(kind = kint_gl), intent(in) :: iglobal_add
      real(kind = kreal), intent(in) :: xx4_add(4), v4_add(4)
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: col_add(ntot_comp)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      fline_lc%nele_line_l = fline_lc%nele_line_l + 1
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%iedge_line_l(1,fline_lc%nele_line_l)                     &
     &      = fline_lc%nnod_line_l - 1
      fline_lc%iedge_line_l(2,fline_lc%nele_line_l)                     &
     &      = fline_lc%nnod_line_l
!
      fline_lc%iglobal_fline(fline_lc%nnod_line_l) = iglobal_add
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%v_line_l(1:3,fline_lc%nnod_line_l) =  v4_add(1:3)
      fline_lc%col_line_l(1:ntot_comp,fline_lc%nnod_line_l)             &
     &      = col_add(1:ntot_comp)
!
      end subroutine add_fline_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline(viz_fields, fline_lc)
!
      use t_ctl_params_viz_fields
!
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      call reset_fline_start(fline_lc)
!
      fline_lc%ntot_comp_l = viz_fields%ntot_color_comp
      call alloc_local_fline_conn(ione, fline_lc)
      call alloc_local_fline_data(itwo, fline_lc)
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
      integer(kind = kint) :: i
!
!
      call allocate_local_fline_conn_tmp(fline_lc)
!$omp parallel do
      do i = 1, fline_lc%nele_line_l
        fline_lc%iedge_line_tmp(1,i) = fline_lc%iedge_line_l(1,i)
        fline_lc%iedge_line_tmp(2,i) = fline_lc%iedge_line_l(2,i)
      end do
!$omp end parallel do
!
      call dealloc_local_fline_conn(fline_lc)
      call alloc_local_fline_conn(itwo*fline_lc%nele_line_l, fline_lc)
!
!$omp parallel do
      do i = 1, fline_lc%nele_line_l
        fline_lc%iedge_line_l(1,i) = fline_lc%iedge_line_tmp(1,i)
        fline_lc%iedge_line_l(2,i) = fline_lc%iedge_line_tmp(2,i)
      end do
!$omp end parallel do
!
      call deallocate_local_fline_conn_tmp(fline_lc)
!
      end subroutine raise_local_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_data(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
      integer(kind = kint) :: i
!
!
      call allocate_local_fline_data_tmp(fline_lc)
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        fline_lc%iglobal_tmp(i) =   fline_lc%iglobal_fline(i)
        fline_lc%xx_line_tmp(1,i) = fline_lc%xx_line_l(1,i)
        fline_lc%xx_line_tmp(2,i) = fline_lc%xx_line_l(2,i)
        fline_lc%xx_line_tmp(3,i) = fline_lc%xx_line_l(3,i)
        fline_lc%v_line_tmp(1,i) =  fline_lc%v_line_l(1,i)
        fline_lc%v_line_tmp(2,i) =  fline_lc%v_line_l(2,i)
        fline_lc%v_line_tmp(3,i) =  fline_lc%v_line_l(3,i)
        fline_lc%col_line_tmp(1:fline_lc%ntot_comp_l,i)                 &
     &           =  fline_lc%col_line_l(1:fline_lc%ntot_comp_l,i)
      end do
!$omp end parallel do
!
      call dealloc_local_fline_data(fline_lc)
      call alloc_local_fline_data(itwo*fline_lc%nnod_line_l, fline_lc)
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        fline_lc%iglobal_fline(i) = fline_lc%iglobal_tmp(i)
        fline_lc%xx_line_l(1,i) =   fline_lc%xx_line_tmp(1,i)
        fline_lc%xx_line_l(2,i) =   fline_lc%xx_line_tmp(2,i)
        fline_lc%xx_line_l(3,i) =   fline_lc%xx_line_tmp(3,i)
        fline_lc%v_line_l(1,i) =    fline_lc%v_line_tmp(1,i)
        fline_lc%v_line_l(2,i) =    fline_lc%v_line_tmp(2,i)
        fline_lc%v_line_l(3,i) =    fline_lc%v_line_tmp(3,i)
        fline_lc%col_line_l(1:fline_lc%ntot_comp_l,i)                   &
     &        =  fline_lc%col_line_tmp(1:fline_lc%ntot_comp_l,i)
      end do
!$omp end parallel do
!
      call deallocate_local_fline_data_tmp(fline_lc)
!
      end subroutine raise_local_fline_data
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
      allocate(fline_lc%xx_line_l(3,fline_lc%nnod_line_buf))
      allocate(fline_lc%v_line_l(3,fline_lc%nnod_line_buf))
      allocate(fline_lc%col_line_l(fline_lc%ntot_comp_l,                &
     &                             fline_lc%nnod_line_buf))
      if(fline_lc%nele_line_buf .gt. 0) fline_lc%iglobal_fline = 0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%xx_line_l =  0.0d0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%v_line_l =   0.0d0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%col_line_l = 0.0d0
!
      end subroutine alloc_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_conn_tmp(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      allocate(fline_lc%iedge_line_tmp(2,fline_lc%nele_line_l))
      if(fline_lc%nele_line_l .gt. 0) fline_lc%iedge_line_tmp = 0
!
      end subroutine allocate_local_fline_conn_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_data_tmp(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      allocate(fline_lc%iglobal_tmp(fline_lc%nnod_line_l))
      allocate(fline_lc%xx_line_tmp(3,fline_lc%nnod_line_l))
      allocate(fline_lc%v_line_tmp(3,fline_lc%nnod_line_l))
      allocate(fline_lc%col_line_tmp(fline_lc%ntot_comp_l,              &
     &                               fline_lc%nnod_line_l))
!
      if(fline_lc%nele_line_l .le. 0) return
      fline_lc%iglobal_tmp =    0
      fline_lc%xx_line_tmp = 0.0d0
      fline_lc%v_line_tmp =  0.0d0
      fline_lc%col_line_tmp = 0.0d0
!
      end subroutine allocate_local_fline_data_tmp
!
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
!
      deallocate(fline_lc%xx_line_l, fline_lc%v_line_l)
      deallocate(fline_lc%iglobal_fline, fline_lc%col_line_l)
!
      end subroutine dealloc_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_conn_tmp(fline_lc)
      type(local_fieldline), intent(inout) :: fline_lc
!
      deallocate(fline_lc%iedge_line_tmp)
!
      end subroutine deallocate_local_fline_conn_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_data_tmp(fline_lc)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      deallocate(fline_lc%xx_line_tmp, fline_lc%v_line_tmp)
      deallocate(fline_lc%col_line_tmp, fline_lc%iglobal_tmp)
!
      end subroutine deallocate_local_fline_data_tmp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_local_fline(id_file, fline_lc)
!
      integer(kind = kint), intent(in) :: id_file
      type(local_fieldline), intent(in) :: fline_lc
      integer(kind = kint) :: i, nd
!
!
      write(id_file,*) 'xx_line_l', fline_lc%nnod_line_l
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16,1p3e16.7)') i, fline_lc%xx_line_l(1:3,i)
      end do
!
      write(id_file,*) 'v_line_l', fline_lc%nnod_line_l
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16,1p3e16.7)') i, fline_lc%v_line_l(1:3,i)
      end do
!
      write(id_file,*) 'iedge_line_l', fline_lc%nele_line_l
      do i = 1, fline_lc%nele_line_l
        write(id_file,'(2i16,a7,2i16)') i, ione, '  line ',             &
     &                                 fline_lc%iedge_line_l(1:2,i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a)') 'color col_line_l,'
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16)', ADVANCE='NO') i
        do nd = 1, fline_lc%ntot_comp_l
           write(id_file,'(i16,1pe16.7)', ADVANCE='NO')                 &
     &       fline_lc%iglobal_fline(i), fline_lc%col_line_l(nd,i)
        end do
        write(id_file,*)
      end do
!
      close(id_file)
!
      end subroutine check_local_fline
!
!  ---------------------------------------------------------------------
!
      end module t_local_fline
