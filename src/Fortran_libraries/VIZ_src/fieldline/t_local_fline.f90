!t_local_fline.f90
!
!      module t_local_fline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine reset_fline_start(fline_lc)
!!      subroutine add_fline_start(xx4_add, col_add, fline_lc)
!!      subroutine alloc_local_fline(fline_lc)
!!      subroutine dealloc_local_fline(fline_lc)
!!      subroutine add_fline_list(xx4_add, col_add, fline_lc)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine check_local_fline(id_file, fline_lc)
!!      subroutine check_local_fline_dx(id_file, fline_lc)
!!        type(local_fieldline), intent(in) :: fline_lc
!
      module t_local_fline
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type local_fieldline
        integer(kind = kint) :: nnod_line_buf
        integer(kind = kint) :: nele_line_buf
        integer(kind = kint) :: nnod_line_l
        integer(kind = kint) :: nele_line_l
        integer(kind = kint), allocatable :: iedge_line_l(:,:)
        real(kind = kreal), allocatable ::   xx_line_l(:,:)
        real(kind = kreal), allocatable ::   col_line_l(:)
      end type local_fieldline
!
      integer(kind = kint), allocatable :: iedge_line_tmp(:,:)
      real(kind = kreal), allocatable ::   xx_line_tmp(:,:)
      real(kind = kreal), allocatable ::   col_line_tmp(:)
!
      private :: iedge_line_tmp, xx_line_tmp, col_line_tmp
!
      private :: alloc_local_fline_data
      private :: dealloc_local_fline_conn, dealloc_local_fline_data
      private :: allocate_local_fline_conn_tmp
      private :: allocate_local_fline_data_tmp
      private :: deallocate_local_fline_conn_tmp
      private :: deallocate_local_fline_data_tmp
      private :: raise_local_fline_data, raise_local_fline_connect
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
      subroutine add_fline_start(xx4_add, col_add, fline_lc)
!
      real(kind = kreal), intent(in) :: xx4_add(4), col_add
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%col_line_l(fline_lc%nnod_line_l) =    col_add
!
      end subroutine add_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_list(xx4_add, col_add, fline_lc)
!
      real(kind = kreal), intent(in) :: xx4_add(4), col_add
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
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%col_line_l(fline_lc%nnod_line_l) =    col_add
!
      end subroutine add_fline_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      call reset_fline_start(fline_lc)
!
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
      call allocate_local_fline_conn_tmp(fline_lc%nele_line_l)
!$omp parallel do
      do i = 1, fline_lc%nele_line_l
        iedge_line_tmp(1,i) = fline_lc%iedge_line_l(1,i)
        iedge_line_tmp(2,i) = fline_lc%iedge_line_l(2,i)
      end do
!$omp end parallel do
!
      call dealloc_local_fline_conn(fline_lc)
      call alloc_local_fline_conn(itwo*fline_lc%nele_line_l, fline_lc)
!
!$omp parallel do
      do i = 1, fline_lc%nele_line_l
        fline_lc%iedge_line_l(1,i) = iedge_line_tmp(1,i)
        fline_lc%iedge_line_l(2,i) = iedge_line_tmp(2,i)
      end do
!$omp end parallel do
!
      call deallocate_local_fline_conn_tmp
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
      call allocate_local_fline_data_tmp(fline_lc%nnod_line_l)
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        xx_line_tmp(1,i) = fline_lc%xx_line_l(1,i)
        xx_line_tmp(2,i) = fline_lc%xx_line_l(2,i)
        xx_line_tmp(3,i) = fline_lc%xx_line_l(3,i)
        col_line_tmp(i) =  fline_lc%col_line_l(i)
      end do
!$omp end parallel do
!
      call dealloc_local_fline_data(fline_lc)
      call alloc_local_fline_data(itwo*fline_lc%nnod_line_l, fline_lc)
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        fline_lc%xx_line_l(1,i) = xx_line_tmp(1,i)
        fline_lc%xx_line_l(2,i) = xx_line_tmp(2,i)
        fline_lc%xx_line_l(3,i) = xx_line_tmp(3,i)
        fline_lc%col_line_l(i) =  col_line_tmp(i)
      end do
!$omp end parallel do
!
      call deallocate_local_fline_data_tmp
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
      if(fline_lc%nele_line_buf .gt. 0) fline_lc%iedge_line_l = 0
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
      allocate(fline_lc%xx_line_l(3,fline_lc%nnod_line_buf))
      allocate(fline_lc%col_line_l(fline_lc%nnod_line_buf))
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%xx_line_l = 0.0d0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%col_line_l = 0.0d0
!
      end subroutine alloc_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_conn_tmp(nele_line_l)
!
      integer(kind = kint), intent(in) :: nele_line_l
!
      allocate(iedge_line_tmp(2,nele_line_l))
      if(nele_line_l .gt. 0) iedge_line_tmp = 0
!
      end subroutine allocate_local_fline_conn_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_data_tmp(nnod_line_l)
!
      integer(kind = kint), intent(in) :: nnod_line_l
!
!
      allocate(xx_line_tmp(3,nnod_line_l))
      allocate(col_line_tmp(nnod_line_l))
      if(nnod_line_l .gt. 0) xx_line_tmp = 0.0d0
      if(nnod_line_l .gt. 0) col_line_tmp = 0.0d0
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
      deallocate(fline_lc%xx_line_l, fline_lc%col_line_l)
!
      end subroutine dealloc_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_conn_tmp
!
      deallocate(iedge_line_tmp)
!
      end subroutine deallocate_local_fline_conn_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_data_tmp
!
!
      deallocate(xx_line_tmp, col_line_tmp)
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
      integer(kind = kint) :: i
!
!
      write(id_file,*) fline_lc%nnod_line_l, fline_lc%nele_line_l
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16,1p3e16.7)') i, fline_lc%xx_line_l(1:3,i)
      end do
!
      do i = 1, fline_lc%nele_line_l
        write(id_file,'(2i16,a7,2i16)') i, ione,                        &
     &               '  line ', fline_lc%iedge_line_l(1:2,i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a)') 'color col_line_l,'
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16,1pe16.7)') i, fline_lc%col_line_l(i)
      end do
!
      close(id_file)
!
      end subroutine check_local_fline
!
!  ---------------------------------------------------------------------
!
      subroutine check_local_fline_dx(id_file, fline_lc)
!
      integer(kind = kint), intent(in) :: id_file
      type(local_fieldline), intent(in) :: fline_lc
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '#  node information'
      write(id_file,'(a,i16,a)')                                        &
     &   'object 1 class array type float rank 1 shape 3 items  ',      &
     &   fline_lc%nnod_line_l, ' data follows'
!
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(1p3e16.7)') fline_lc%xx_line_l(1:3,i)
      end do
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# element connectivity'
      write(id_file,'(a,i16,a)')                                        &
     &   'object 2 class array type int rank 1 shape 2 items    ',      &
     &   fline_lc%nele_line_l, ' data follows'
      do i = 1, fline_lc%nele_line_l
        write(id_file,'(2i16)') (fline_lc%iedge_line_l(1:2,i)-1)
      end do
      write(id_file,'(a)') 'attribute "element type" string "lines"'
      write(id_file,'(a)') 'attribute "ref" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# scalar'
      write(id_file,'(a,i16,a)')                                        &
     &   'object 3 class array type float rank 1 shape 1 items  ',      &
     &   fline_lc%nnod_line_l, ' data follows'
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(1pe16.7)') fline_lc%col_line_l(i)
      end do
      write(id_file,'(a)') 'attribute "dep" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') 'object "irregular positions irregular connections ascii file" class field'
      write(id_file,'(a)') 'component "positions" value 1'
      write(id_file,'(a)') 'component "connections" value 2'
      write(id_file,'(a)') 'component "data" value    3'
      write(id_file,'(a)') 'end'


      close(id_file)
!
      end subroutine check_local_fline_dx
!
!  ---------------------------------------------------------------------
!
      end module t_local_fline
