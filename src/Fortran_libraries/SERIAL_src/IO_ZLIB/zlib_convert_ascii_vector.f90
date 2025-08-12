!>@file   zlib_convert_ascii_vector.f90
!!@brief  module zlib_convert_ascii_vector
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Vector data compression routine
!!
!!@verbatim
!!      subroutine defleate_node_position                               &
!!     &         (nnod, numdir, id_global, xx, zbuf)
!!      subroutine infleate_node_position                               &
!!     &         (nnod, numdir, id_global, xx, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine defleate_vector_txt                                  &
!!     &         (iflag_blank, nnod, ndir, vector, zbuf)
!!      subroutine infleate_vector_txt                                  &
!!     &         (iflag_blank, nnod, ndir, vector, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module zlib_convert_ascii_vector
!
      use m_precision
      use m_constants
!
      use t_buffer_4_gzip
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine defleate_node_position                                 &
     &         (nnod, numdir, id_global, xx, zbuf)
!
      use data_IO_to_textline
      use gzip_defleate
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint_gl), intent(in) :: id_global(nnod)
      real(kind = kreal), intent(in) :: xx(nnod, numdir)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = kint_gl) ::  i, ist
!
      integer :: nline, ilen_tmp
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_int8_and_vector_textline(numdir)
      zbuf%ilen_gz                                                      &
     &      = int(dble(nnod*ilen_line)*1.01+24,KIND(zbuf%ilen_gz ))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nnod .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_char_once(ione, char(10),                     &
     &      ilen_in, zbuf, zbuf%gzip_buf(1))
      else if(nnod .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_char_once(ilen_line,                          &
     &      int8_and_vector_textline                                    &
     &         (id_global(1), numdir, xx(1,1)),                         &
     &      ilen_in, zbuf, zbuf%gzip_buf(1))
      else if(nnod .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24, KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*)                                  &
!     &     'defleate_node_position start ',                            &
!     &      nnod, ilen_line, zbuf%ilen_gz, ilen_tmp
        do
          nline = int(min((nnod - ist), huge_30/ilen_line))
          ilen_in                                                       &
     &       = int(min(zbuf%ilen_gz - zbuf%ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, zbuf%ilen_gzipped+1, ilen_in
          xx_tmp(1:numdir) = xx(ist+1,1:numdir)
          call gzip_defleat_char_begin(ilen_line,                       &
     &      int8_and_vector_textline(id_global(ist+1), numdir, xx_tmp), &
     &      ilen_in, zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do i = ist+2, ist+nline-1
            xx_tmp(1:numdir) = xx(i,1:numdir)
            call gzip_defleat_char_cont(ilen_line,                      &
     &          int8_and_vector_textline(id_global(i), numdir, xx_tmp), &
     &          zbuf)
          end do
!
          xx_tmp(1:numdir) = xx(ist+nline,1:numdir)
          call gzip_defleat_char_last(ilen_line,                        &
     &       int8_and_vector_textline                                   &
     &          (id_global(ist+nline), numdir, xx_tmp),                 &
     &       zbuf)
!
          ist = ist + nline
          if(ist .ge. nnod) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      end subroutine defleate_node_position
!
! -----------------------------------------------------------------------
!
      subroutine infleate_node_position                                 &
     &         (nnod, numdir, id_global, xx, zbuf)
!
      use data_IO_to_textline
      use gzip_infleate
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint_gl), intent(inout) :: id_global(nnod)
      real(kind = kreal), intent(inout) :: xx(nnod, numdir)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = kint_gl) :: i, ist
!
      integer :: nline, ilen_tmp
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_int8_and_vector_textline(numdir)
      call alloc_textbuffer_for_zlib(ilen_line, zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nnod .le. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ione, zbuf%textbuf(1), zbuf)
      else if(nnod .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ilen_line, zbuf%textbuf, zbuf)
        call read_int8_and_vector_textline                              &
     &     (zbuf%textbuf(1), id_global(1), numdir, xx(1,1))
      else if(nnod .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24, KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'all start ',                    &
!     &      nnod, ilen_line, zbuf%ilen_gz, ilen_tmp
!
        do
          nline = int(min((nnod - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call link_pointer_for_zlib_buffer                             &
     &       (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),              &
     &        ilen_line, zbuf%textbuf, zbuf)
          call gzip_infleat_char_begin(zbuf)
          call read_int8_and_vector_textline                            &
     &       (zbuf%textbuf(1), id_global(ist+1), numdir, xx_tmp)
          xx(ist+1,1:numdir) = xx_tmp(1:numdir)
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_char_cont(zbuf)
            call read_int8_and_vector_textline                          &
     &         (zbuf%textbuf(1), id_global(i), numdir, xx_tmp)
            xx(i,1:numdir) = xx_tmp(1:numdir)
          end do
!
          call gzip_infleat_char_last(zbuf)
          call read_int8_and_vector_textline                            &
     &       (zbuf%textbuf(1), id_global(ist+nline), numdir, xx_tmp)
          call unlink_pointer_for_zlib_buffer(zbuf)
!
          xx(ist+nline,1:numdir) = xx_tmp(1:numdir)
          ist = ist + nline
          if(ist .ge. nnod) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', zbuf%ilen_gzipped
      end if
!
      call dealloc_textbuffer_for_zlib(zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_node_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_vector_txt                                    &
     &         (iflag_blank, nnod, ndir, vector, zbuf)
!
      use field_data_IO
      use data_IO_to_textline
      use gzip_defleate
!
      integer(kind = kint), intent(in) :: iflag_blank
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      real(kind = kreal) :: v1(ndir)
      integer(kind = kint_gl) :: i, ist
!
      integer :: nline, ilen_tmp
      integer :: ilen_line, ilen_in
!
!
      ilen_line = len_vector_textline(ndir)
      zbuf%ilen_gz                                                      &
     &     = int(dble(nnod*ilen_line)*1.01+24, KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nnod .le. 0 .and. iflag_blank .gt. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_char_once(ione, char(10),                     &
     &      ilen_in, zbuf, zbuf%gzip_buf(1))
!
      else if(nnod .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_char_once(ilen_line,                          &
     &      vector_textline(ndir, v1),                                  &
     &      ilen_in, zbuf, zbuf%gzip_buf(1))
!
      else if(nnod .gt. 1) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24, KIND(ilen_tmp))
        do
          nline = int(min((nnod - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          v1(1:ndir) = vector(ist+1,1:ndir)
          call gzip_defleat_char_begin(ilen_line,                       &
     &        vector_textline(ndir, v1),                                &
     &        ilen_in, zbuf, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do i = ist+2, ist+nline-1
            v1(1:ndir) = vector(i,1:ndir)
            call gzip_defleat_char_cont(ilen_line,                      &
     &          vector_textline(ndir, v1), zbuf)
          end do
!
          v1(1:ndir) = vector(ist+nline,1:ndir)
          call gzip_defleat_char_last(ilen_line,                        &
     &        vector_textline(ndir, v1), zbuf)
          ist = ist + nline
          if(ist .ge. nnod) exit
        end do
      end if
!
      end subroutine defleate_vector_txt
!
! -----------------------------------------------------------------------
!
      subroutine infleate_vector_txt                                    &
     &         (iflag_blank, nnod, ndir, vector, zbuf)
!
      use field_data_IO
      use field_data_MPI_IO
      use data_IO_to_textline
      use gzip_infleate
!
      integer(kind = kint), intent(in) :: iflag_blank
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndir
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist
!
      integer :: nline, ilen_tmp
      integer :: ilen_line, ilen_in
!
      real(kind = kreal) :: v1(ndir)
!
!
      ilen_line = len_vector_textline(ndir)
      call alloc_textbuffer_for_zlib(ilen_line, zbuf)
      zbuf%ilen_gzipped = 0
!
      if(nnod .le. 0 .and. iflag_blank .gt. 0) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once                                     &
     &    (ilen_in, zbuf%gzip_buf(1), ione, zbuf%textbuf(1), zbuf)
      else if(nnod .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_infleat_char_once(ilen_in, zbuf%gzip_buf(1),          &
     &      ilen_line, zbuf%textbuf, zbuf)
        call read_vector_textline(zbuf%textbuf(1), ndir, v1)
        vector(1,1:ndir) = v1(1:ndir)
      else if(nnod .gt. 0) then
        ist = 0
        ilen_tmp = int(dble(huge_30)*1.01+24, KIND(ilen_tmp))
!
        do
          nline = int(min((nnod - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call link_pointer_for_zlib_buffer                             &
     &       (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),              &
     &        ilen_line, zbuf%textbuf, zbuf)
          call gzip_infleat_char_begin(zbuf)
          call read_vector_textline(zbuf%textbuf(1), ndir, v1)
          vector(ist+1,1:ndir) = v1(1:ndir)
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_char_cont(zbuf)
            call read_vector_textline(zbuf%textbuf(1), ndir, v1)
            vector(i,1:ndir) = v1(1:ndir)
          end do
!
          call gzip_infleat_char_last(zbuf)
          call read_vector_textline(zbuf%textbuf(1), ndir, v1)
          call unlink_pointer_for_zlib_buffer(zbuf)
!
          vector(ist+nline,1:ndir) = v1(1:ndir)
          ist = ist + nline
          if(ist .ge. nnod) exit
        end do
      end if
!
      call dealloc_textbuffer_for_zlib(zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_vector_txt
!
! -----------------------------------------------------------------------
!
      end module zlib_convert_ascii_vector
