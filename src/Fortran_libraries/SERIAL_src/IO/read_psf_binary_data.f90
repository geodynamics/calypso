!>@file  read_psf_binary_data.f90
!!       module read_psf_binary_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine read_psf_phys_num_bin(np_read, nnod, num_field, bbuf)
!!      subroutine read_psf_phys_name_bin                               &
!!     &         (num_field, ntot_comp, ncomp_field, field_name, bbuf)
!!      subroutine read_psf_phys_data_bin                               &
!!     &         (np_read, nnod, ntot_comp, d_nod, bbuf)
!!
!!      subroutine read_psf_node_num_bin(np_read, nnod, bbuf)
!!      subroutine read_psf_node_data_bin                               &
!!     &         (np_read, nnod, inod_global, xx, bbuf)
!!      subroutine read_psf_ele_num_bin(np_read, nele, nnod_ele, bbuf)
!!      subroutine read_psf_ele_connect_bin                             &
!!     &         (np_read, nele, nnod_ele, iele_global, ie_psf, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module read_psf_binary_data
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
!  ---------------------------------------------------------------------
!
      subroutine read_psf_phys_num_bin(np_read, nnod, num_field, bbuf)
!
      use binary_IO
!
      integer, intent(in) :: np_read
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind=kint), intent(inout) :: num_field
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nnod_gl
!
!
      call read_psf_node_num_bin(np_read, nnod_gl, bbuf)
      if(nnod .ne. nnod_gl) stop 'Grid and field data are inconsistent'
!
      call read_one_integer_b(bbuf, num_field)
!
      end subroutine read_psf_phys_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_name_bin                                 &
     &         (num_field, ntot_comp, ncomp_field, field_name, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint), intent(in) :: num_field
!
      integer(kind=kint), intent(inout) :: ntot_comp
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_mul_integer_b(bbuf, cast_long(num_field), ncomp_field)
      call read_mul_character_b(bbuf, num_field, field_name)
      ntot_comp = sum(ncomp_field)
!
      end subroutine read_psf_phys_name_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_data_bin                                 &
     &         (np_read, nnod, ntot_comp, d_nod, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: itmp1_mp(:)
      integer(kind = kint) :: nd
!
!
      allocate(itmp1_mp(np_read))
      do nd = 1, ntot_comp
        call read_mul_int8_b(bbuf, cast_long(np_read), itmp1_mp)
        call read_1d_vector_b(bbuf, nnod, d_nod(1,nd))
      end do
      deallocate(itmp1_mp)
!
      end subroutine read_psf_phys_data_bin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_num_bin(np_read, nnod, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
!
      integer(kind=kint_gl), intent(inout) :: nnod
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(np_read))
      call read_mul_int8_b(bbuf, cast_long(np_read), n_inter)
      call read_mul_int8_b(bbuf, cast_long(np_read), n_inter)
      nnod = sum(n_inter)
!
!      write(*,*) 'n_inter', n_inter
      deallocate(n_inter)
!
      end subroutine read_psf_node_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_data_bin                                 &
     &         (np_read, nnod, inod_global, xx, bbuf)
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
!
      end module read_psf_binary_data
