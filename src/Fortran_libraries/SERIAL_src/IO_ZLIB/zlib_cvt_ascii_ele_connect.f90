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
      use gzip_defleate
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
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_int8_and_mul_int8_textline(nnod_4_ele)
      zbuf%ilen_gz                                                      &
     &     = int(dble(nele*ilen_line) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nele .le. 0) then
        call gzip_defleat_char_once(ione, char(10),                     &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      else if(nele .eq. 1) then
        ie_tmp(1:nnod_4_ele) = ie(1,1:nnod_4_ele)
        call gzip_defleat_char_once(ilen_line,                          &
     &      int8_and_mul_int8_textline                                  &
     &         (id_global(1), nnod_4_ele, ie_tmp),                      &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      else if(nele .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          ie_tmp(1:nnod_4_ele) = ie(ist+1,1:nnod_4_ele)
          call gzip_defleat_char_begin(ilen_line,                       &
     &        int8_and_mul_int8_textline                                &
     &             (id_global(ist+1), nnod_4_ele, ie_tmp),              &
     &        ilen_in, zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do i = ist+2, ist+nline-1
            ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
            call gzip_defleat_char_cont(ilen_line,                      &
     &          int8_and_mul_int8_textline                              &
     &              (id_global(i), nnod_4_ele, ie_tmp),                 &
     &          zbuf)
          end do
!
          ie_tmp(1:nnod_4_ele) = ie(ist+nline,1:nnod_4_ele)
          call gzip_defleat_char_last(ilen_line,                        &
     &        int8_and_mul_int8_textline                                &
     &             (id_global(ist+nline), nnod_4_ele, ie_tmp),          &
     &        zbuf)
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
      use gzip_infleate
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
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_int8_and_mul_int8_textline(nnod_4_ele)
      call alloc_textbuffer_for_zlib(ilen_line, zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &     (int(zbuf%ilen_gz), zbuf%gzip_buf(1),                        &
     &     ione, zbuf%textbuf, zbuf)
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &     (int(zbuf%ilen_gz), zbuf%gzip_buf(1),                        &
     &      ilen_line, zbuf%textbuf, zbuf)
        call read_int8_and_mul_int8_textline                            &
     &     (zbuf%textbuf(1), id_global(1), nnod_4_ele, ie_tmp)
        ie(1,1:nnod_4_ele) = int(ie_tmp(1:nnod_4_ele), KIND(ie(1,1)))
      else if(nele .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call link_pointer_for_zlib_buffer                             &
     &       (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),              &
     &        ilen_line, zbuf%textbuf, zbuf)
          call gzip_infleat_char_begin(zbuf)
          call read_int8_and_mul_int8_textline                          &
     &       (zbuf%textbuf(1), id_global(ist+1), nnod_4_ele, ie_tmp)
          ie(ist+1,1:nnod_4_ele)                                        &
     &            = int(ie_tmp(1:nnod_4_ele), KIND(ie(1,1)))
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_char_cont(zbuf)
            call read_int8_and_mul_int8_textline                        &
     &         (zbuf%textbuf(1), id_global(i), nnod_4_ele, ie_tmp)
            ie(i,1:nnod_4_ele)                                          &
     &            = int(ie_tmp(1:nnod_4_ele),KIND(ie(1,1)))
          end do
!
          call gzip_infleat_char_last(zbuf)
          call read_int8_and_mul_int8_textline(zbuf%textbuf(1),         &
     &        id_global(ist+nline), nnod_4_ele, ie_tmp)
          call unlink_pointer_for_zlib_buffer(zbuf)
!
          ie(ist+nline,1:nnod_4_ele)                                    &
     &             = int(ie_tmp(1:nnod_4_ele), KIND(ie(1,1)))
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      call dealloc_textbuffer_for_zlib(zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_ele_connect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_ele_int_list(nele, ncomp, ivect, zbuf)
!
      use gzip_defleate
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
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_multi_int_textline(ncomp)
      zbuf%ilen_gz                                                      &
     &      = int(dble(nele*ilen_line) *1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_char_once(ione, char(10),                     &
     &      ilen_in, zbuf, zbuf%gzip_buf(1))
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_char_once(ilen_line,                          &
     &      multi_int_textline(ncomp, ivect(1,1)),                      &
     &      ilen_in, zbuf, zbuf%gzip_buf(1))
      else if(nele .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          ie_tmp(1:ncomp) = ivect(ist+1,1:ncomp)
          call gzip_defleat_char_begin(ilen_line,                       &
     &        multi_int_textline(ncomp, ie_tmp),                        &
     &       ilen_in, zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do i = ist+2, ist+nline-1
            ie_tmp(1:ncomp) = ivect(i,1:ncomp)
            call gzip_defleat_char_cont(ilen_line,                      &
     &         multi_int_textline(ncomp, ie_tmp), zbuf)
          end do
!
          ie_tmp(1:ncomp) = ivect(ist+nline,1:ncomp)
          call gzip_defleat_char_last(ilen_line,                        &
     &       multi_int_textline(ncomp, ie_tmp), zbuf)
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
      use gzip_infleate
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
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_multi_int_textline(ncomp)
      call alloc_textbuffer_for_zlib(ilen_line, zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nele .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ione, zbuf%textbuf, zbuf)
      else if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ilen_line, zbuf%textbuf, zbuf)
        call read_multi_int_textline                                    &
     &     (zbuf%textbuf(1), ncomp, ivect(1,1))
      else if(nele .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))

        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call link_pointer_for_zlib_buffer                             &
     &       (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),              &
     &        ilen_line, zbuf%textbuf, zbuf)
          call gzip_infleat_char_begin(zbuf)
          call read_multi_int_textline(zbuf%textbuf(1), ncomp, ie_tmp)
          ivect(ist+1,1:ncomp) = ie_tmp(1:ncomp)
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_char_cont(zbuf)
            call read_multi_int_textline                                &
     &         (zbuf%textbuf(1), ncomp, ie_tmp)
            ivect(i,1:ncomp) = ie_tmp(1:ncomp)
          end do
!
          call gzip_infleat_char_last(zbuf)
          call read_multi_int_textline(zbuf%textbuf(1), ncomp, ie_tmp)
          call unlink_pointer_for_zlib_buffer(zbuf)
!
          ivect(ist+nline,1:ncomp) = ie_tmp(1:ncomp)
!
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
      end if
!
      call dealloc_textbuffer_for_zlib(zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_ele_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_1d_global_address(nnod, numdir, idx, zbuf)
!
      use gzip_defleate
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: idx(nnod, numdir)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: idx_tmp(numdir)
      integer :: ilen_line
!
!
      ilen_line = len_multi_int_textline(numdir)
      zbuf%ilen_gz                                                      &
     &       = int(dble(nnod*ilen_line) *1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nnod .le. 0) then
        call gzip_defleat_char_once(ione, char(10),                     &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_char_once(ilen_line,                          &
     &      multi_int_textline(numdir, idx(1,1)),                       &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      else if(nnod .gt. 0) then
        idx_tmp(1:numdir) = idx(1,1:numdir)
        call gzip_defleat_char_begin(ilen_line,                         &
     &     multi_int_textline(numdir, idx_tmp),                         &
     &     int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
        do i = 2, nnod - 1
          idx_tmp(1:numdir) = idx(i,1:numdir)
          call gzip_defleat_char_cont(ilen_line,                        &
     &        multi_int_textline(numdir, idx_tmp), zbuf)
        end do
        idx_tmp(1:numdir) = idx(nnod,1:numdir)
        call gzip_defleat_char_last(ilen_line,                          &
     &      multi_int_textline(numdir, idx_tmp), zbuf)
      end if
!
      end subroutine defleate_1d_global_address
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_global_address(nnod, numdir, idx, zbuf)
!
      use gzip_infleate
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(inout) :: idx(nnod, numdir)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: idx_tmp(numdir)
      integer(kind = kint_gl) :: i
!
      integer :: ilen_line, ilen_in
!
!
      ilen_in = int(zbuf%ilen_gz)
      ilen_line = len_multi_int_textline(numdir)
      call alloc_textbuffer_for_zlib(ilen_line, zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nnod .le. 0) then
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ione, zbuf%textbuf, zbuf)
      else if(nnod .eq. 1) then
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ilen_line, zbuf%textbuf, zbuf)
        call read_multi_int_textline                                    &
     &     (zbuf%textbuf(1), numdir, idx(1,1))
      else if(nnod .gt. 0) then
        call link_pointer_for_zlib_buffer                               &
     &     (ilen_in, zbuf%gzip_buf(1), ilen_line, zbuf%textbuf, zbuf)
        call gzip_infleat_char_begin(zbuf)
        call read_multi_int_textline(zbuf%textbuf(1), numdir, idx_tmp)
        idx(1,1:numdir) = idx_tmp(1:numdir)
!
        do i = 2, nnod-1
          call gzip_infleat_char_cont(zbuf)
          call read_multi_int_textline                                  &
     &       (zbuf%textbuf(1), numdir, idx_tmp)
          idx(i,1:numdir) = idx_tmp(1:numdir)
        end do
!
        call gzip_infleat_char_last(zbuf)
        call read_multi_int_textline(zbuf%textbuf(1), numdir, idx_tmp)
        call unlink_pointer_for_zlib_buffer(zbuf)
!
        idx(nnod,1:numdir) = idx_tmp(1:numdir)
      end if
!
      call dealloc_textbuffer_for_zlib(zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1d_global_address
!
! -----------------------------------------------------------------------
!
      end module zlib_cvt_ascii_ele_connect
