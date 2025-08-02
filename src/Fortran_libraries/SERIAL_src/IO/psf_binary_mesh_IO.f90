!>@file  psf_binary_mesh_IO.f90
!!       module psf_binary_mesh_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine read_psf_node_data_bin                               &
!!     &         (np_read, nnod, inod_global, xx, bbuf)
!!      subroutine read_psf_ele_num_bin(np_read, nele, nnod_ele, bbuf)
!!      subroutine read_psf_ele_connect_bin                             &
!!     &         (np_read, nele, nnod_ele, iele_global, ie_psf, bbuf)
!!        integer, intent(in) :: np_read
!!        integer(kind = kint), intent(in) :: nnod_ele
!!        integer(kind = kint_gl), intent(in) :: nele
!!        integer(kind = kint_gl), intent(inout) :: iele_global(nele)
!!        integer(kind = kint_gl), intent(inout) :: ie_psf(nele,nnod_ele)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine write_sgl_psf_node_data_bin(nnod, xx, bbuf)
!!        integer(kind=kint_gl), intent(in) :: nnod
!!        real(kind = kreal), intent(in) :: xx(nnod,3)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine write_sgl_psf_ele_num_bin(nele, nnod_ele, bbuf)
!!      subroutine write_sgl_psf_ele_connect_bin                        &
!!     &         (nele, nnod_ele, ie_psf, bbuf)
!!        integer(kind = kint), intent(in) :: nnod_ele
!!        integer(kind = kint_gl), intent(in) :: nele
!!        integer(kind = kint_gl), intent(in) :: ie_psf(nele,nnod_ele)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module psf_binary_mesh_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_binary_IO_buffer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_data_bin                                 &
     &         (np_read, nnod, inod_global, xx, bbuf)
!
      use psf_binary_data_IO
!
      integer, intent(in) :: np_read
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind=kint_gl), intent(inout) :: inod_global(nnod)
      real(kind = kreal), intent(inout) :: xx(nnod,3)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: i
!
!
      call read_psf_phys_data_bin(np_read, nnod, ithree, xx, bbuf)
!
!$omp parallel do
      do i = 1, nnod
        inod_global(i) = i
      end do
!$omp end parallel do
!
      end subroutine read_psf_node_data_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_num_bin(np_read, nele, nnod_ele, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
!
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: nele_lc(:)
      integer(kind = kint_gl) :: eletype(1), nnod_ele_b(1)
!
!
      call read_mul_int8_b(bbuf, cast_long(ione), nnod_ele_b)
      call read_mul_int8_b(bbuf, cast_long(ione), eletype)
!
      allocate(nele_lc(np_read))
      call read_mul_int8_b(bbuf, cast_long(np_read), nele_lc)
      call read_mul_int8_b(bbuf, cast_long(np_read), nele_lc)
!
      nnod_ele = int(nnod_ele_b(1),KIND(nnod_ele))
      nele = sum(nele_lc)
!
!      write(*,*) 'nele_lc', nele_lc
!      write(*,*) 'nnod_ele_b', nnod_ele_b
!      write(*,*) 'eletype', eletype
!
      deallocate(nele_lc)
!
      end subroutine read_psf_ele_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_connect_bin                               &
     &         (np_read, nele, nnod_ele, iele_global, ie_psf, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint_gl), intent(inout) :: iele_global(nele)
      integer(kind = kint_gl), intent(inout) :: ie_psf(nele,nnod_ele)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: itmp1_mp(:)
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: nd
!
!
      allocate(itmp1_mp(np_read))
      do nd = 1, nnod_ele
        call read_mul_int8_b(bbuf, cast_long(np_read), itmp1_mp)
        call read_mul_int8_b(bbuf, nele, ie_psf(1,nd))
      end do
      deallocate(itmp1_mp)
!
!$omp parallel do
      do i = 1, nele
        iele_global(i) = i
      end do
!$omp end parallel do
!
      end subroutine read_psf_ele_connect_bin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sgl_psf_node_data_bin(nnod, xx, bbuf)
!
      use psf_binary_data_IO
!
      integer(kind=kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_sgl_psf_phys_data_bin(nnod, ithree, xx, bbuf)
!
      end subroutine write_sgl_psf_node_data_bin
!
! -----------------------------------------------------------------------
!
      subroutine write_sgl_psf_ele_num_bin(nele, nnod_ele, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nele64(1)
!
!
      nele64(1) = cast_long(8*nnod_ele)
      call write_mul_int8_b(cast_long(ione), nele64, bbuf)
      nele64(1) = cast_long(nnod_ele)
      call write_mul_int8_b(cast_long(ione), nele64, bbuf)
!
      nele64(1) = 8*nele
      call write_mul_int8_b(cast_long(ione), nele64(1), bbuf)
      nele64(1) = nele
      call write_mul_int8_b(cast_long(ione), nele64(1), bbuf)
!
      end subroutine write_sgl_psf_ele_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine write_sgl_psf_ele_connect_bin                          &
     &         (nele, nnod_ele, ie_psf, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint_gl), intent(in) :: ie_psf(nele,nnod_ele)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nnod64(1)
      integer(kind = kint) :: nd
!
!
      nnod64(1) = 8*nele
      do nd = 1, nnod_ele
        call write_mul_int8_b(cast_long(ione), nnod64, bbuf)
        call write_mul_int8_b(nele, ie_psf(1,nd), bbuf)
      end do
!
      end subroutine write_sgl_psf_ele_connect_bin
!
! -----------------------------------------------------------------------
!
      end module psf_binary_mesh_IO
