!>@file  MPI_spherical_model_IO.f90
!!       module MPI_spherical_model_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine mpi_read_rank_4_sph(IO_param, sph_IO)
!!      subroutine mpi_read_gl_reso_sph(IO_param, sph_IO)
!!      subroutine mpi_read_gl_nodes_sph(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine mpi_write_rank_4_sph(IO_param, sph_IO)
!!      subroutine mpi_write_gl_reso_sph(IO_param, sph_IO)
!!      subroutine mpi_write_gl_nodes_sph(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module MPI_spherical_model_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use m_sph_modes_grid_labels
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
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
      subroutine mpi_read_rank_4_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  num_tmp
!
!
      call mpi_skip_read(IO_param, len(hd_segment()))
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_comm_table(IO_param,                                &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine mpi_read_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_reso_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  num_tmp
!
!
      call mpi_skip_read(IO_param, len(hd_trunc()))
      call read_integer_textline                                        &
     &   (mpi_read_charahead(IO_param, len_int_txt), sph_IO%ltr_gl)
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_comm_table(IO_param,                                &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
!
      end subroutine mpi_read_gl_reso_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_nodes_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_read_num_of_data(IO_param, sph_IO%numnod_sph)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call mpi_read_ele_connect                                         &
     &   (IO_param, sph_IO%numnod_sph, sph_IO%numdir_sph,               &
     &    sph_IO%inod_gl_sph, sph_IO%idx_gl_sph)
!
      end subroutine mpi_read_gl_nodes_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rank_4_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_segment()), hd_segment())
      call mpi_write_comm_table(IO_param,                               &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine mpi_write_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_reso_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_trunc()), hd_trunc())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(sph_IO%ltr_gl))
      call mpi_write_comm_table(IO_param,                               &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
!
      end subroutine mpi_write_gl_reso_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_nodes_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_ele_connect(IO_param,                              &
     &    sph_IO%numnod_sph, sph_IO%numdir_sph,                         &
     &    sph_IO%inod_gl_sph, sph_IO%idx_gl_sph)
!
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine mpi_write_gl_nodes_sph
!
! -----------------------------------------------------------------------
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
      call mpi_write_num_of_data(IO_param, nele)
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
      end module MPI_spherical_model_IO
