!>@file  gz_MPI_spherical_model_IO.f90
!!       module gz_MPI_spherical_model_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_mpi_read_rank_4_sph(IO_param, sph_IO)
!!      subroutine gz_mpi_read_gl_reso_sph(IO_param, sph_IO)
!!      subroutine gz_mpi_read_gl_nodes_sph(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine gz_mpi_write_rank_4_sph(IO_param, sph_IO)
!!      subroutine gz_mpi_write_gl_reso_sph(IO_param, sph_IO)
!!      subroutine gz_mpi_write_gl_nodes_sph(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module gz_MPI_spherical_model_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use m_sph_modes_grid_labels
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
      subroutine gz_mpi_read_rank_4_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  num_tmp
!
!
      call gz_mpi_skip_header(IO_param, len(hd_segment()))
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call gz_mpi_read_comm_table(IO_param,                             &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine gz_mpi_read_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gl_reso_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  num_tmp
!
!
      call gz_mpi_skip_header(IO_param, len(hd_trunc()))
      call read_integer_textline                                        &
     &   (gz_mpi_read_charahead(IO_param, len_int_txt), sph_IO%ltr_gl)
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call gz_mpi_read_comm_table(IO_param,                             &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
!
      end subroutine gz_mpi_read_gl_reso_sph
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gl_nodes_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_read_num_of_data(IO_param, sph_IO%numnod_sph)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call gz_mpi_read_ele_connect                                      &
     &   (IO_param, sph_IO%numnod_sph, sph_IO%numdir_sph,               &
     &    sph_IO%inod_gl_sph, sph_IO%idx_gl_sph)
!
      end subroutine gz_mpi_read_gl_nodes_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_rank_4_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_segment()), hd_segment())
      call gz_mpi_write_comm_table(IO_param,                            &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine gz_mpi_write_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gl_reso_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_trunc()), hd_trunc())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(sph_IO%ltr_gl))
      call gz_mpi_write_comm_table(IO_param,                            &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
!
      end subroutine gz_mpi_write_gl_reso_sph
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gl_nodes_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  nvect
!
!
      call gz_mpi_write_ele_connect(IO_param,                           &
     &    sph_IO%numnod_sph, sph_IO%numdir_sph,                         &
     &    sph_IO%inod_gl_sph, sph_IO%idx_gl_sph)
!
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine gz_mpi_write_gl_nodes_sph
!
! -----------------------------------------------------------------------
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
      call gz_mpi_write_num_of_data(IO_param, nele)
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
      end module gz_MPI_spherical_model_IO
