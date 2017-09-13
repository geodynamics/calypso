!>@file  MPI_integer_list_IO.f90
!!       module MPI_integer_list_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine mpi_read_ele_connect                                 &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine mpi_read_element_type                                &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine mpi_read_int_list(IO_param, nele, ncomp, ivect)
!!
!!      subroutine mpi_write_ele_connect                                &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine mpi_write_element_type                               &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine mpi_write_int_list(IO_param, nele, ncomp, ivect)
!!@endverbatim
!
      module MPI_integer_list_IO
!
      use m_precision
!
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
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
      subroutine mpi_read_ele_connect                                   &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(inout) :: id_global(nele)
      integer(kind=kint), intent(inout) :: ie(nele, nnod_4_ele)
!
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = kint) :: i, ilength, n_item
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          ilength = ione
        else if(n_item .gt. 0) then
          ilength = len_int8_and_mul_int_textline(nnod_4_ele) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + ilength
      end do
!
      if(nele .eq. 0) then
        ilength = ione
      else if(nele .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
        ilength = len_int8_and_mul_int_textline(nnod_4_ele)
        do i = 1, nele
          call read_int8_and_mul_int_textline                           &
     &       (calypso_mpi_seek_read_chara(IO_param%id_file,             &
     &                                    ioffset, ilength),            &
     &        id_global(i), nnod_4_ele, ie_tmp)
          ie(i,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
        end do
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_element_type                                  &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, n_item, ilength, led, loop
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          led = ione
        else if(n_item .le. ncolumn) then
          led = len_multi_6digit_line(n_item)
        else if(n_item .gt. 0) then
          nrest = mod((n_item-1),ncolumn) + 1
          loop = (n_item-1)/ncolumn
          led = len_multi_6digit_line(nrest)                            &
     &         + len_multi_6digit_line(ncolumn) * loop
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1) + led
      end do
      led = int(IO_param%istack_merged(IO_param%id_rank+1)              &
     &         -  IO_param%istack_merged(IO_param%id_rank))
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
        do i = 0, (num-1)/ncolumn - 1
          ilength = len_multi_6digit_line(ncolumn)
          call read_mul_6digit_int_line                                 &
     &       (calypso_mpi_seek_read_chara(IO_param%id_file,             &
     &                                    ioffset, ilength),            &
     &        ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        ilength = len_multi_6digit_line(nrest)
        call read_mul_6digit_int_line                                   &
     &     (calypso_mpi_seek_read_chara(IO_param%id_file,               &
     &                                  ioffset, ilength),              &
     &      nrest, int_dat(num-nrest+1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_list(IO_param, nele, ncomp, ivect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(inout) :: ivect(nele, ncomp)
!
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = kint) :: i, ilength, n_item
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          ilength = ione
        else if(n_item .gt. 0) then
          ilength = len_multi_int_textline(ncomp) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + ilength
      end do
!
      if(nele .eq. 0) then
        ilength = ione
      else if(nele .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
        ilength = len_multi_int_textline(ncomp)
        do i = 1, nele
          call read_multi_int_textline                                  &
     &       (calypso_mpi_seek_read_chara(IO_param%id_file,             &
     &                                    ioffset, ilength),            &
     &                                    ncomp, ie_tmp)
          ivect(i,1:ncomp) = ie_tmp(1:ncomp)
        end do
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_ele_connect                                  &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i, led, ilength
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_int8_and_mul_int_textline(nnod_4_ele)
!
      if(nele .le. 0) then
        led = ione
      else
        led = ilength * nele
      end if
!
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nele .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nele
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_mul_int_textline(id_global(i),                   &
     &                                  nnod_4_ele, ie_tmp))
        end do
      end if
!
      end subroutine mpi_write_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_element_type                                 &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_6digit_line(ncolumn) * loop                     &
     &       + len_multi_6digit_line(nrest)
      end if
!
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_6digit_line(ncolumn),                           &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_6digit_line(nrest),                               &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_list(IO_param, nele, ncomp, ivect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(in) :: ivect(nele,ncomp)
!
      integer(kind = kint) :: i, led, ilength
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_multi_int_textline(ncomp)
!
      if(nele .le. 0) then
        led = ione
      else
        led = ilength * nele
      end if
!
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nele .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nele
          ie_tmp(1:ncomp) = ivect(i,1:ncomp)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        multi_int_textline(ncomp, ie_tmp))
        end do
      end if
!
      end subroutine mpi_write_int_list
!
! -----------------------------------------------------------------------
!
      end module MPI_integer_list_IO
