!>@file   zlib_cvt_ascii_comm_tbl.f90
!!@brief  module zlib_cvt_ascii_comm_tbl
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine defleate_comm_table(ncolumn, num, int_dat, zbuf)
!!      subroutine infleate_comm_table(ncolumn, num, int_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine defleate_element_type(ncolumn, num, int_dat, zbuf)
!!      subroutine infleate_element_type(ncolumn, num, int_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module zlib_cvt_ascii_comm_tbl
!
      use m_precision
      use m_constants
!
      use t_buffer_4_gzip
      use data_IO_to_textline
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine defleate_comm_table(ncolumn, num, int_dat, zbuf)
!
      integer(kind = kint), intent(in) :: ncolumn
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
      integer(kind = kint) :: nitem_1, nitem_2, nitem_c, nrest
!
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      zbuf%ilen_gz                                                     &
     &   = int(dble(num*len_int_txt) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(num .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
      else
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_line = len_multi_int_textline(ncolumn)
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*)                                  &
!     &     'gz_mpi_write_comm_table start ',                           &
!     &      num, ilen_line, zbuf%ilen_gz, ilen_tmp
!
        do
          nitem_1 = min(num-ist,ncolumn+1)
          nitem_2 = int(min(num-ist,ncolumn*(huge_30/ilen_line)))
          nitem_c = nitem_2 - (mod(nitem_2-1,ncolumn)+1)
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start loop',                  &
!     &         ist+1, ist+nitem_1, ist+nitem_2, ist+nitem_c,           &
!     &         zbuf%ilen_gzipped+1, ilen_in
          if(nitem_1 .le. ncolumn) then
            call gzip_defleat_once(len_multi_int_textline(nitem_1),     &
     &      multi_int_textline(nitem_1, int_dat(ist+1)),                &
     &      ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            exit
          else
            call gzip_defleat_begin(ilen_line,                          &
     &          multi_int_textline(ncolumn, int_dat(ist+1)),            &
     &          ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin',        &
!     &                       ist+ncolumn, ilen_used
!
            do i = ist+ncolumn+1, ist+nitem_c, ncolumn
              call gzip_defleat_cont(ilen_line,                         &
     &            multi_int_textline(ncolumn, int_dat(i)),              &
     &            ilen_in, ilen_used)
            end do
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont',         &
!     &                       ist+nitem_c, ilen_used
!
            nrest = nitem_2 - nitem_c
            call gzip_defleat_last(len_multi_int_textline(nrest),       &
     &          multi_int_textline(nrest, int_dat(ist+nitem_c+1)),      &
     &          ilen_in, ilen_used)
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',         &
!     &                       ilen_used, ist + nitem_2, num
!
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            ist = ist + nitem_2
            if(ist .ge. num) exit
          end if
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      end subroutine defleate_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine infleate_comm_table(ncolumn, num, int_dat, zbuf)
!
      integer(kind=kint), intent(in) :: ncolumn
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
      integer(kind = kint) :: nitem_1, nitem_2, nitem_c, nrest
!
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
      character(len=1), allocatable :: textbuf(:)
!
!
      allocate(textbuf(len_multi_int_textline(ncolumn)))
!
      if(num .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(1), ione, textbuf(1), ilen_used)
        zbuf%ilen_gzipped = ilen_used
      else
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_line = len_multi_int_textline(ncolumn)
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*)                                  &
!     &     'gz_mpi_write_comm_table start ',                           &
!     &      num, ilen_line, zbuf%ilen_gz, ilen_tmp
!
        do
          nitem_1 = int(min(num-ist,ncolumn+1))
          nitem_2 = int(min(num-ist,ncolumn*(huge_30/ilen_line)))
          nitem_c = nitem_2 - (mod(nitem_2-1,ncolumn)+1)
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start loop',                  &
!     &         ist+1, ist+nitem_1, ist+nitem_2, ist+nitem_c,           &
!     &         zbuf%ilen_gzipped+1, ilen_in
          if(nitem_1 .le. ncolumn) then
            call gzip_infleat_once                                      &
     &         (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),            &
     &          len_multi_int_textline(nitem_1), textbuf(1), ilen_used)
            call read_multi_int_textline                                &
     &         (textbuf(1), nitem_1, int_dat(ist+1))
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            exit
          else
            call gzip_infleat_begin                                     &
     &         (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),            &
     &          ilen_line, textbuf(1), ilen_used)
            call read_multi_int_textline                                &
     &         (textbuf(1), ncolumn, int_dat(ist+1))
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin',        &
!     &                       ist+ncolumn, ilen_used
!
            do i = ist+ncolumn+1, ist+nitem_c, ncolumn
              call gzip_infleat_cont                                    &
     &           (ilen_in, ilen_line, textbuf(1), ilen_used)
              call read_multi_int_textline                              &
     &           (textbuf(1), ncolumn, int_dat(i))
            end do
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont',         &
!     &                       ist+nitem_c, ilen_used
!
            nrest = nitem_2 - nitem_c
            call gzip_infleat_last                                      &
     &         (ilen_in, len_multi_int_textline(nrest),                 &
     &          textbuf(1), ilen_used)
            call read_multi_int_textline                                &
     &         (textbuf(1), nrest, int_dat(ist+nitem_c+1))
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',         &
!     &                       ilen_used, ist + nitem_2, num
!
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            ist = ist + nitem_2
            if(ist .ge. num) exit
          end if
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      deallocate(textbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_comm_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_element_type(ncolumn, num, int_dat, zbuf)
!
      integer(kind = kint), intent(in) :: ncolumn
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
      integer(kind = kint) :: nitem_1, nitem_2, nitem_c, nrest
!
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in, ilength
!
!
      zbuf%ilen_gz                                                      &
     &   = int(dble(num*len_6digit_txt) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(num .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
      else
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_line = len_multi_6digit_line(ncolumn)
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'defleate_element_type start ',  &
!     &      num, ilen_line, zbuf%ilen_gz, ilen_tmp
!
        do
          nitem_1 = int(min(num-ist,ncolumn+1))
          nitem_2 = int(min(num-ist,ncolumn*(huge_30/ilen_line)))
          nitem_c = nitem_2 - (mod(nitem_2-1,ncolumn)+1)
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start loop',                  &
!     &         ist+1, ist+nitem_1, ist+nitem_2, ist+nitem_c,           &
!     &         zbuf%ilen_gzipped+1, ilen_in
          if(nitem_1 .le. ncolumn) then
            ilength = int(len_multi_6digit_line(nitem_1))
            call gzip_defleat_once                                      &
     &         (ilength, mul_6digit_int_line(nitem_1, int_dat(ist+1)),  &
     &          ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            exit
          else
            call gzip_defleat_begin(ilen_line,                          &
     &          mul_6digit_int_line(ncolumn, int_dat(ist+1)),           &
     &          ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin',        &
!     &                       ist+ncolumn, ilen_used
!
            do i = ist+ncolumn+1, ist+nitem_c, ncolumn
              call gzip_defleat_cont(ilen_line,                         &
     &            mul_6digit_int_line(ncolumn, int_dat(i)),             &
     &            ilen_in, ilen_used)
            end do
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont',         &
!     &                       ist+nitem_c, ilen_used
!
            nrest = nitem_2 - nitem_c
            ilength = int(len_multi_6digit_line(nrest))
            call gzip_defleat_last(ilength,                             &
     &          mul_6digit_int_line(nrest, int_dat(ist+nitem_c+1)),     &
     &          ilen_in, ilen_used)
!            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',         &
!     &                       ilen_used, ist + nitem_2, num
!
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            ist = ist + nitem_2
            if(ist .ge. num) exit
          end if
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      end subroutine defleate_element_type
!
! -----------------------------------------------------------------------
!
      subroutine infleate_element_type(ncolumn, num, int_dat, zbuf)
!
      integer(kind=kint), intent(in) :: ncolumn
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
      integer(kind = kint) :: nitem_1, nitem_2, nitem_c, nrest
!
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
      character(len=1), allocatable :: textbuf(:)
!
!
      ilen_line = len_multi_6digit_line(ncolumn)
      allocate(textbuf(ilen_line))
!
      if(num .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, zbuf%gzip_buf(1), ione, textbuf(1), ilen_used)
        zbuf%ilen_gzipped = ilen_used
      else
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'infleate_element_type  start ', &
!     &      num, ilen_line, zbuf%ilen_gz, ilen_tmp
!
        do
          nitem_1 = int(min(num-ist,ncolumn+1))
          nitem_2 = int(min(num-ist,ncolumn*(huge_30/ilen_line)))
          nitem_c = int(nitem_2 - (mod(nitem_2-1,ncolumn)+1))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start loop',                  &
!     &         ist+1, ist+nitem_1, ist+nitem_2, ist+nitem_c,           &
!     &         zbuf%ilen_gzipped+1, ilen_in
          if(nitem_1 .le. ncolumn) then
            call gzip_infleat_once                                      &
     &         (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),            &
     &          len_multi_6digit_line(nitem_1), textbuf(1), ilen_used)
            call read_mul_6digit_int_line                               &
     &         (textbuf(1), nitem_1, int_dat(ist+1))
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            exit
          else
            call gzip_infleat_begin                                     &
     &         (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1), ilen_line, &
     &          textbuf(1), ilen_used)
            call read_mul_6digit_int_line                               &
     &         (textbuf(1), ncolumn, int_dat(ist+1))
!
            do i = ist+ncolumn+1, ist+nitem_c, ncolumn
              call gzip_infleat_cont(ilen_in, ilen_line,                &
     &            textbuf(1), ilen_used)
              call read_mul_6digit_int_line                             &
     &           (textbuf(1), ncolumn, int_dat(i))
            end do
!            if(my_rank .eq. 0) write(*,*) 'gzip_infleat_cont',         &
!     &                       ist+nitem_c, ilen_used
!
            nrest = nitem_2 - nitem_c
            call gzip_infleat_last                                      &
     &         (ilen_in, len_multi_6digit_line(nrest),                  &
     &          textbuf(1), ilen_used)
            call read_mul_6digit_int_line                               &
     &         (textbuf(1), nrest, int_dat(ist+nitem_c+1))
!            if(my_rank .eq. 0) write(*,*) 'gzip_infleat_last',         &
!     &                       ist+nitem_2, ilen_used
!
            zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
            ist = ist + nitem_2
            if(ist .ge. num) exit
          end if
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      deallocate(textbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_element_type
!
! -----------------------------------------------------------------------
!
      end module zlib_cvt_ascii_comm_tbl
