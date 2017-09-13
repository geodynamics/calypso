!>@file  gz_MPI_integer_list_IO.f90
!!       module gz_MPI_integer_list_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_mpi_read_element_type                             &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_read_ele_connect                              &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine gz_mpi_read_int_list(IO_param, nele, ncomp, ivect)
!!
!!      subroutine gz_mpi_write_ele_connect                             &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine gz_mpi_write_element_type                            &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_write_int_list(IO_param, nele, ncomp, ivect)
!!@endverbatim
!
      module gz_MPI_integer_list_IO
!
      use m_precision
!
      use t_calypso_mpi_IO_param
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
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
      subroutine gz_mpi_read_ele_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(inout) :: id_global(nele)
      integer(kind=kint), intent(inout) :: ie(nele, nnod_4_ele)
!
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = kint) :: i
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
      character(len=1), allocatable :: textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_int8_and_mul_int_textline(nnod_4_ele)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(nele .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(nele .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_int8_and_mul_int_textline                             &
     &     (textbuf(1), id_global(1), nnod_4_ele, ie(1,1))
      else if(nele .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_int8_and_mul_int_textline                             &
     &     (textbuf(1), id_global(1), nnod_4_ele, ie_tmp)
        ie(1,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
!
        do i = 2, nele-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_int8_and_mul_int_textline                           &
     &       (textbuf(1), id_global(i), nnod_4_ele, ie_tmp)
          ie(i,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_int8_and_mul_int_textline                             &
     &     (textbuf(1), id_global(nele), nnod_4_ele, ie_tmp)
        ie(nele,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_element_type                               &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      allocate(textbuf(len_multi_6digit_line(ncolumn)))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(num .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(num .le. ncolumn) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), len_multi_6digit_line(num),            &
     &     textbuf(1), ilen_gzipped)
        call read_mul_6digit_int_line(textbuf(1), num, int_dat(1))
      else if(num .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), len_multi_6digit_line(ncolumn),         &
     &    textbuf(1), ilen_gzipped)
        call read_mul_6digit_int_line(textbuf(1), ncolumn, int_dat(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, len_multi_6digit_line(ncolumn),                  &
     &        textbuf(1), ilen_gzipped)
          call read_mul_6digit_int_line                                 &
     &       (textbuf(1), ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_infleat_last                                          &
     &     (ilen_gz, len_multi_6digit_line(nrest),                      &
     &      textbuf(1), ilen_gzipped)
        call read_mul_6digit_int_line                                   &
     &     (textbuf(1), nrest, int_dat(num-nrest+1))
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_list(IO_param, nele, ncomp, ivect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(inout) :: ivect(nele, ncomp)
!
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = kint) :: i
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
      character(len=1), allocatable :: textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_multi_int_textline(ncomp)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(nele .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(nele .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), ncomp, ivect(1,1))
      else if(nele .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
        ivect(1,1:ncomp) = ie_tmp(1:ncomp)
!
        do i = 2, nele-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
          ivect(i,1:ncomp) = ie_tmp(1:ncomp)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
        ivect(nele,1:ncomp) = ie_tmp(1:ncomp)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_line = len_int8_and_mul_int_textline(nnod_4_ele)
      ilen_gz = int(real(nele*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nele .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_mul_int_textline                                   &
     &         (id_global(1), nnod_4_ele, ie(1,1)),                     &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .gt. 0) then
        ie_tmp(1:nnod_4_ele) = ie(1,1:nnod_4_ele)
        call gzip_defleat_begin(ilen_line,                              &
     &     int8_and_mul_int_textline(id_global(1), nnod_4_ele, ie_tmp), &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nele - 1
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          call gzip_defleat_cont(ilen_line,                             &
     &     int8_and_mul_int_textline(id_global(i), nnod_4_ele, ie_tmp), &
     &        ilen_gz, ilen_gzipped)
        end do
        ie_tmp(1:nnod_4_ele) = ie(nele,1:nnod_4_ele)
        call gzip_defleat_last(ilen_line,                               &
     &     int8_and_mul_int_textline                                    &
     &          (id_global(nele), nnod_4_ele, ie_tmp),                  &
     &      ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_type                              &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_gz = int(real(num*len_6digit_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .le. ncolumn) then
        call gzip_defleat_once(len_multi_6digit_line(num),              &
     &      mul_6digit_int_line(num, int_dat(1)),                       &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .gt. 0) then
        call gzip_defleat_begin(len_multi_6digit_line(ncolumn),         &
     &      mul_6digit_int_line(ncolumn, int_dat(1)),                   &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_defleat_cont(len_multi_6digit_line(ncolumn),        &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)),       &
     &        ilen_gz, ilen_gzipped)
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_defleat_last(len_multi_6digit_line(nrest),            &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)),           &
     &      ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_list(IO_param, nele, ncomp, ivect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(in) :: ivect(nele,ncomp)
!
      integer(kind = kint) :: i
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_line = len_multi_int_textline(ncomp)
      ilen_gz = int(real(nele*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nele .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      multi_int_textline(ncomp, ivect(1,1)),                      &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .gt. 0) then
        ie_tmp(1:ncomp) = ivect(1,1:ncomp)
        call gzip_defleat_begin(ilen_line,                              &
     &     multi_int_textline(ncomp, ie_tmp),                           &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nele - 1
          ie_tmp(1:ncomp) = ivect(i,1:ncomp)
          call gzip_defleat_cont(ilen_line,                             &
     &     multi_int_textline(ncomp, ie_tmp), ilen_gz, ilen_gzipped)
        end do
        ie_tmp(1:ncomp) = ivect(nele,1:ncomp)
        call gzip_defleat_last(ilen_line,                               &
     &     multi_int_textline(ncomp, ie_tmp), ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_int_list
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_integer_list_IO
