!>@file  zlib_cvt_ascii_ele_connect.f90
!!       module zlib_cvt_ascii_ele_connect
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine defleate_ele_connect                                 &
!!     &         (nele, nnod_4_ele, id_global, ie, zbuf)
!!      subroutine infleate_ele_connect                                 &
!!     &         (nele, nnod_4_ele, id_global, ie, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine defleate_ele_int_list(nele, ncomp, ivect, zbuf)
!!      subroutine infleate_ele_int_list(nele, ncomp, ivect, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine defleate_1d_global_address(nnod, numdir, idx, zbuf)
!!      subroutine infleate_1d_global_address(nnod, numdir, idx, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module zlib_cvt_ascii_ele_connect
!
      use m_precision
!
      use t_buffer_4_gzip
      use data_IO_to_textline
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine defleate_ele_connect                                   &
     &         (nele, nnod_4_ele, id_global, ie, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint_gl), intent(in) :: id_global(nele)
      integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
      integer :: nline
      integer(kind = kint_gl) :: ie_tmp(nnod_4_ele)
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ilen_line = len_int8_and_mul_int8_textline(nnod_4_ele)
      zbuf%ilen_gz                                                      &
     &     = int(dble(nele*ilen_line) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        ie_tmp(1:nnod_4_ele) = ie(1,1:nnod_4_ele)
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_mul_int8_textline                                  &
     &         (id_global(1), nnod_4_ele, ie_tmp),                      &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
      else if(nele .gt. 0) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          ie_tmp(1:nnod_4_ele) = ie(ist+1,1:nnod_4_ele)
          call gzip_defleat_begin(ilen_line,                            &
     &        int8_and_mul_int8_textline                                &
     &             (id_global(ist+1), nnod_4_ele, ie_tmp),              &
     &        ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do i = ist+2, ist+nline-1
            ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
            call gzip_defleat_cont(ilen_line,                           &
     &          int8_and_mul_int8_textline                              &
     &              (id_global(i), nnod_4_ele, ie_tmp),                 &
     &          ilen_in, ilen_used)
          end do
!
          ie_tmp(1:nnod_4_ele) = ie(ist+nline,1:nnod_4_ele)
          call gzip_defleat_last(ilen_line,                             &
     &        int8_and_mul_int8_textline                                &
     &             (id_global(ist+nline), nnod_4_ele, ie_tmp),          &
     &        ilen_in, ilen_used)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
      end if
!
      end subroutine defleate_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine infleate_ele_connect                                   &
     &         (nele, nnod_4_ele, id_global, ie, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint_gl), intent(inout) :: id_global(nele)
      integer(kind = kint), intent(inout) :: ie(nele, nnod_4_ele)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ie_tmp(nnod_4_ele)
      integer(kind = kint_gl) :: i, ist
      integer :: nline
!
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
      character(len=1), allocatable :: textbuf(:)
!
!
      ilen_line = len_int8_and_mul_int8_textline(nnod_4_ele)
      allocate(textbuf(ilen_line))
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ione, textbuf(1), ilen_used)
        zbuf%ilen_gzipped = ilen_used
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ilen_line, textbuf(1), ilen_used)
        call read_int8_and_mul_int8_textline                            &
     &     (textbuf(1), id_global(1), nnod_4_ele, ie_tmp)
        ie(1,1:nnod_4_ele) = int(ie_tmp(1:nnod_4_ele), KIND(ie(1,1)))
        zbuf%ilen_gzipped = ilen_used
      else if(nele .gt. 0) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'infleate_ele_connect start',    &
!     &      nele, ilen_line, zbuf%ilen_gz, ilen_tmp
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, zbuf%ilen_gzipped+1,  ilen_in
          call gzip_infleat_begin                                       &
     &       (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),              &
     &        ilen_line, textbuf(1), ilen_used)
          call read_int8_and_mul_int8_textline                          &
     &       (textbuf(1), id_global(ist+1), nnod_4_ele, ie_tmp)
          ie(ist+1,1:nnod_4_ele)                                        &
     &            = int(ie_tmp(1:nnod_4_ele), KIND(ie(1,1)))
!          if(my_rank .eq. 0) write(*,*) 'gzip_infleat_begin', ilen_used
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_cont                                      &
     &         (ilen_in, ilen_line, textbuf(1), ilen_used)
            call read_int8_and_mul_int8_textline                        &
     &         (textbuf(1), id_global(i), nnod_4_ele, ie_tmp)
            ie(i,1:nnod_4_ele)                                          &
     &            = int(ie_tmp(1:nnod_4_ele),KIND(ie(1,1)))
          end do
!          if(my_rank .eq. 0) write(*,*) 'gzip_infleat_cont', ilen_used
!
          call gzip_infleat_last                                        &
     &       (ilen_in, ilen_line, textbuf(1), ilen_used)
          call read_int8_and_mul_int8_textline                          &
     &       (textbuf(1), id_global(ist+nline), nnod_4_ele, ie_tmp)
          ie(ist+nline,1:nnod_4_ele)                                    &
     &             = int(ie_tmp(1:nnod_4_ele), KIND(ie(1,1)))
!          if(my_rank .eq. 0) write(*,*) 'gzip_infleat_last',           &
!     &        ilen_used, ist + nline, nele
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      deallocate(textbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_ele_connect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_ele_int_list(nele, ncomp, ivect, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: ivect(nele,ncomp)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
      integer :: nline
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ilen_line = len_multi_int_textline(ncomp)
      zbuf%ilen_gz                                                      &
     &      = int(dble(nele*ilen_line) *1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ilen_line,                               &
     &      multi_int_textline(ncomp, ivect(1,1)),                      &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
      else if(nele .gt. 0) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          ie_tmp(1:ncomp) = ivect(ist+1,1:ncomp)
          call gzip_defleat_begin(ilen_line,                            &
     &        multi_int_textline(ncomp, ie_tmp),                        &
     &       ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do i = ist+2, ist+nline-1
            ie_tmp(1:ncomp) = ivect(i,1:ncomp)
            call gzip_defleat_cont(ilen_line,                           &
     &         multi_int_textline(ncomp, ie_tmp), ilen_in, ilen_used)
          end do
!
          ie_tmp(1:ncomp) = ivect(ist+nline,1:ncomp)
          call gzip_defleat_last(ilen_line,                             &
     &       multi_int_textline(ncomp, ie_tmp), ilen_in, ilen_used)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
      end if
!
      end subroutine defleate_ele_int_list
!
! -----------------------------------------------------------------------
!
      subroutine infleate_ele_int_list(nele, ncomp, ivect, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(inout) :: ivect(nele, ncomp)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = kint_gl) :: i, ist
      integer :: nline
!
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
      character(len=1), allocatable :: textbuf(:)
!
!
      ilen_line = len_multi_int_textline(ncomp)
      allocate(textbuf(ilen_line))
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ione, textbuf(1), ilen_used)
        zbuf%ilen_gzipped = ilen_used
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ilen_line, textbuf(1), ilen_used)
        call read_multi_int_textline(textbuf(1), ncomp, ivect(1,1))
        zbuf%ilen_gzipped = ilen_used
      else if(nele .gt. 0) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))

        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call gzip_infleat_begin                                       &
     &       (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),              &
     &        ilen_line, textbuf(1), ilen_used)
          call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
          ivect(ist+1,1:ncomp) = ie_tmp(1:ncomp)
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_cont                                      &
     &         (ilen_in, ilen_line, textbuf(1), ilen_used)
            call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
            ivect(i,1:ncomp) = ie_tmp(1:ncomp)
          end do
!
          call gzip_infleat_last                                        &
     &       (ilen_in, ilen_line, textbuf(1), ilen_used)
          call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
          ivect(ist+nline,1:ncomp) = ie_tmp(1:ncomp)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
      end if
!
      deallocate(textbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_ele_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_1d_global_address(nnod, numdir, idx, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: idx(nnod, numdir)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: idx_tmp(numdir)
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ilen_line = len_multi_int_textline(numdir)
      zbuf%ilen_gz                                                      &
     &       = int(dble(nnod*ilen_line) *1.01 + 24,KIND(zbuf%ilen_gz))
      ilen_in = int(zbuf%ilen_gz)
      call alloc_zip_buffer(zbuf)
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      multi_int_textline(numdir, idx(1,1)),                       &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
      else if(nnod .gt. 0) then
        idx_tmp(1:numdir) = idx(1,1:numdir)
        call gzip_defleat_begin(ilen_line,                              &
     &     multi_int_textline(numdir, idx_tmp),                         &
     &     ilen_in, ilen_used, zbuf%gzip_buf(1))
        do i = 2, nnod - 1
          idx_tmp(1:numdir) = idx(i,1:numdir)
          call gzip_defleat_cont(ilen_line,                             &
     &     multi_int_textline(numdir, idx_tmp),                         &
     &        ilen_in, ilen_used)
        end do
        idx_tmp(1:numdir) = idx(nnod,1:numdir)
        call gzip_defleat_last(ilen_line,                               &
     &     multi_int_textline(numdir, idx_tmp),                         &
     &      ilen_in, ilen_used)
      end if
      zbuf%ilen_gzipped = ilen_used
!
      end subroutine defleate_1d_global_address
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_global_address(nnod, numdir, idx, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(inout) :: idx(nnod, numdir)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: idx_tmp(numdir)
      integer(kind = kint_gl) :: i
!
      integer :: ilen_line, ilen_in, ilen_used
!
      character(len=1), allocatable :: textbuf(:)
!
!
      ilen_in = int(zbuf%ilen_gz)
      ilen_line = len_multi_int_textline(numdir)
      allocate(textbuf(ilen_line))
!
      if(nnod .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ione, textbuf(1), ilen_used)
      else if(nnod .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ilen_line, textbuf(1), ilen_used)
        call read_multi_int_textline(textbuf(1), numdir, idx(1,1))
      else if(nnod .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_in, zbuf%gzip_buf(1), ilen_line, textbuf(1), ilen_used)
        call read_multi_int_textline(textbuf(1), numdir, idx_tmp)
        idx(1,1:numdir) = idx_tmp(1:numdir)
!
        do i = 2, nnod-1
          call gzip_infleat_cont                                        &
     &       (ilen_in, ilen_line, textbuf(1), ilen_used)
          call read_multi_int_textline(textbuf(1),  numdir, idx_tmp)
          idx(i,1:numdir) = idx_tmp(1:numdir)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_in, ilen_line, textbuf(1), ilen_used)
        call read_multi_int_textline(textbuf(1), numdir, idx_tmp)
        idx(nnod,1:numdir) = idx_tmp(1:numdir)
      end if
      zbuf%ilen_gzipped = ilen_used
!
      deallocate(textbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1d_global_address
!
! -----------------------------------------------------------------------
!
      end module zlib_cvt_ascii_ele_connect
