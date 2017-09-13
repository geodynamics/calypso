!>@file   MPI_vectors_IO.f90
!!@brief  module MPI_vectors_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_node_position                               &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!      subroutine mpi_read_scalar(IO_param, nnod, scalar)
!!      subroutine mpi_read_vector(IO_param, nnod, numdir, vect)
!!
!!      subroutine mpi_write_node_position                              &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!      subroutine mpi_write_scalar(IO_param, nnod, scalar)
!!      subroutine mpi_write_vector(IO_param, nnod, numdir, vect)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_vectors_IO
!
      use m_precision
      use m_constants
!
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_node_position                                 &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(inout) :: id_global(nnod)
      real(kind=kreal), intent(inout) :: xx(nnod, numdir)
!
      real(kind = kreal) :: xx_tmp(numdir)
!
      integer(kind = kint) :: i, led, n_item, ilength
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
        else if(n_item .gt. 0) then
          led = len_int8_and_vector_textline(numdir) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + led
      end do
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        if(nnod .eq. 0) then
          led = ione
        else
          ioffset = IO_param%ioff_gl                                    &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
          ilength = len_int8_and_vector_textline(numdir)
          do i = 1, nnod
            call read_int8_and_vector_textline                          &
     &         (calypso_mpi_seek_read_chara(IO_param%id_file,           &
     &                                      ioffset, ilength),          &
     &          id_global(i), numdir, xx_tmp)
            xx(i,1:numdir) = xx_tmp(1:numdir)
          end do
        end if
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_node_position
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_scalar(IO_param, nnod, scalar)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod
      real(kind=kreal), intent(inout) :: scalar(nnod)
!
      real(kind = kreal) :: vect_tmp(1)
!
      integer(kind = kint) :: i, led, n_item, ilength
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
        else if(n_item .gt. 0) then
          led = len_vector_textline(ione) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + led
      end do
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        if(nnod .eq. 0) then
          led = ione
        else
          ioffset = IO_param%ioff_gl                                    &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
          ilength = len_vector_textline(ione)
          do i = 1, nnod
            call read_vector_textline                                   &
     &         (calypso_mpi_seek_read_chara(IO_param%id_file,           &
     &                                      ioffset, ilength),          &
     &          ione, vect_tmp)
            scalar(i) = vect_tmp(1)
          end do
        end if
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_scalar
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_vector(IO_param, nnod, numdir, vect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      real(kind=kreal), intent(inout) :: vect(nnod, numdir)
!
      real(kind = kreal) :: vect_tmp(numdir)
!
      integer(kind = kint) :: i, led, n_item, ilength
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
        else if(n_item .gt. 0) then
          led = len_vector_textline(numdir) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + led
      end do
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        if(nnod .eq. 0) then
          led = ione
        else
          ioffset = IO_param%ioff_gl                                    &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
          ilength = len_vector_textline(numdir)
          do i = 1, nnod
            call read_vector_textline                                   &
     &         (calypso_mpi_seek_read_chara(IO_param%id_file,           &
     &                                      ioffset, ilength),          &
     &          numdir, vect_tmp)
            vect(i,1:numdir) = vect_tmp(1:numdir)
          end do
        end if
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_node_position                                &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i, led, ilength
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_write_num_of_data(IO_param, nnod)
!
      ilength = len_int8_and_vector_textline(numdir)
      led = nnod * len_int8_and_vector_textline(numdir)
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nnod .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nnod
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_vector_textline(id_global(i), numdir, xx_tmp))
        end do
      end if
      call calypso_mpi_barrier
!
      end subroutine mpi_write_node_position
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_scalar(IO_param, nnod, scalar)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod
      real(kind=kreal), intent(in) :: scalar(nnod)
!
      integer(kind = kint) :: i, led, ilength
      real(kind = kreal) :: vec_tmp(1)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_write_num_of_data(IO_param, nnod)
!
      ilength = len_vector_textline(ione)
      led = nnod * len_vector_textline(ione)
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nnod .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nnod
          vec_tmp(1) = scalar(i)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        vector_textline(ione, vec_tmp))
        end do
      end if
      call calypso_mpi_barrier
!
      end subroutine mpi_write_scalar
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_vector(IO_param, nnod, numdir, vect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      real(kind=kreal), intent(in) :: vect(nnod, numdir)
!
      integer(kind = kint) :: i, led, ilength
      real(kind = kreal) :: vec_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_write_num_of_data(IO_param, nnod)
!
      ilength = len_vector_textline(numdir)
      led = nnod * len_vector_textline(numdir)
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nnod .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nnod
          vec_tmp(1:numdir) = vect(i,1:numdir)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        vector_textline(numdir, vec_tmp))
        end do
      end if
      call calypso_mpi_barrier
!
      end subroutine mpi_write_vector
!
! -----------------------------------------------------------------------
!
      end module MPI_vectors_IO
