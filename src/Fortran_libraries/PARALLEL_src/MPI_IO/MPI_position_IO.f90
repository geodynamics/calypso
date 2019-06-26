!>@file   MPI_position_IO.f90
!!@brief  module MPI_position_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_node_position                               &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!      subroutine mpi_read_radial_position                             &
!!     &         (IO_param, nri, id_r_global, rr)
!!
!!      subroutine mpi_write_node_position                              &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!      subroutine mpi_write_radial_position                            &
!!     &         (IO_param, nri, id_r_global, rr)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_position_IO
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
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint_gl), intent(inout) :: id_global(nnod)
      real(kind=kreal), intent(inout) :: xx(nnod, numdir)
!
      real(kind = kreal) :: xx_tmp(numdir)
!
      integer(kind = kint) :: i, n_item
      integer(kind = kint_gl) :: inod, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
!
      n_item = int(IO_param%nprocs_in,KIND(n_item))
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(n_item))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i),KIND(n_item))
        if(n_item .le. 0) then
          led = ione
        else if(n_item .gt. 0) then
          led = len_int8_and_vector_textline(numdir) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1) + led
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
          do inod = 1, nnod
            call read_int8_and_vector_textline                          &
     &         (calypso_mpi_seek_read_chara(IO_param%id_file,           &
     &                                      ioffset, ilength),          &
     &          id_global(inod), numdir, xx_tmp)
            xx(inod,1:numdir) = xx_tmp(1:numdir)
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
      subroutine mpi_read_radial_position                               &
     &         (IO_param, nri, id_r_global, rr)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nri
      integer(kind=kint), intent(inout) :: id_r_global(nri)
      real(kind=kreal), intent(inout) :: rr(nri)
!
      integer(kind = kint_gl) :: int8_tmp
      integer(kind = kint) :: i, led, n_item
      integer :: ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      n_item = int(IO_param%nprocs_in,KIND(led))
      call mpi_skip_read(IO_param, len_multi_int_textline(n_item))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i),KIND(n_item))
        if(n_item .le. 0) then
          led = ione
        else if(n_item .gt. 0) then
          led = len_int8_and_vector_textline(ione) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1) + led
      end do
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        if(nri .eq. 0) then
          led = ione
        else
          ioffset = IO_param%ioff_gl                                    &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
          ilength = len_int8_and_vector_textline(ione)
          do i = 1, nri
            call read_int8_and_vector_textline                          &
     &         (calypso_mpi_seek_read_chara(IO_param%id_file,           &
     &                                      ioffset, ilength),          &
     &          int8_tmp, ione, rr(i))
            id_r_global(i) = int8_tmp
          end do
        end if
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_radial_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_node_position                                &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint_gl) :: inod, led
      integer(kind = kint) :: i
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
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
     &     (IO_param%id_file, ioffset, 1, char(10))
      else
        do inod = 1, nnod
          xx_tmp(1:numdir) = xx(inod,1:numdir)
          call calypso_mpi_seek_write_chara                             &
     &      (IO_param%id_file, ioffset, ilength,                        &
     &       int8_and_vector_textline(id_global(inod), numdir, xx_tmp))
        end do
      end if
!
      end subroutine mpi_write_node_position
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_radial_position                              &
     &         (IO_param, nri, id_r_global, rr)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nri
      integer(kind=kint), intent(in) :: id_r_global(nri)
      real(kind=kreal), intent(in) :: rr(nri)
!
      integer(kind = kint_gl) :: int8_tmp
      integer(kind = kint_gl) :: led
      integer(kind = kint) :: i
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
!
      ilength = len_int8_and_vector_textline(ione)
      led = nri * len_int8_and_vector_textline(ione)
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nri .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, 1, char(10))
      else
        do i = 1, nri
          int8_tmp = id_r_global(i)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_vector_textline(int8_tmp, ione, rr(i)))
        end do
      end if
!
      end subroutine mpi_write_radial_position
!
! -----------------------------------------------------------------------
!
      end module MPI_position_IO
