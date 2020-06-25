!>@file  read_psf_binary_data.f90
!!       module read_psf_binary_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine read_psf_phys_num_bin                                &
!!     &         (nprocs, nnod, num_field, itmp1_mp, bbuf)
!!      subroutine read_psf_phys_name_bin                               &
!!     &         (num_field, ntot_comp, ncomp_field, field_name, bbuf)
!!      subroutine read_psf_phys_data_bin                               &
!!     &         (nprocs, nnod, ntot_comp, d_nod, itmp1_mp, bbuf)
!!
!!      subroutine read_psf_node_num_bin(nprocs, nnod, itmp1_mp, bbuf)
!!      subroutine read_psf_node_data_bin                               &
!!     &         (nprocs, nnod, inod_global, xx, itmp1_mp, bbuf)
!!      subroutine read_psf_ele_num_bin                                 &
!!     &         (nprocs, nele, nnod_ele, itmp1_mp, bbuf)
!!      subroutine read_psf_ele_connect_bin                             &
!!     &         (nprocs, nele, nnod_ele, iele_global, ie_psf,          &
!!     &          itmp1_mp, bbuf)
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
      subroutine read_psf_phys_num_bin                                  &
     &         (nprocs, nnod, num_field, itmp1_mp, bbuf)
!
      use binary_IO
!
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nnod_gl
!
!
      call read_psf_node_num_bin(nprocs, nnod_gl, itmp1_mp, bbuf)
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
     &         (nprocs, nnod, ntot_comp, d_nod, itmp1_mp, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, ntot_comp
        call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
        call read_1d_vector_b(bbuf, nnod, d_nod(1,nd))
      end do
!
      end subroutine read_psf_phys_data_bin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_num_bin(nprocs, nnod, itmp1_mp, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind=kint_gl), intent(inout) :: nnod
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(nprocs))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), n_inter)
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
     &         (nprocs, nnod, inod_global, xx, itmp1_mp, bbuf)
!
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind=kint_gl), intent(inout) :: inod_global(nnod)
      real(kind = kreal), intent(inout) :: xx(nnod,3)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: i
!
!
      call read_psf_phys_data_bin                                       &
     &   (nprocs, nnod, ithree, xx, itmp1_mp, bbuf)
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
      subroutine read_psf_ele_num_bin                                   &
     &         (nprocs, nele, nnod_ele, itmp1_mp, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: nele_lc(:)
      integer(kind = kint_gl) :: eletype(1), nnod_ele_b(1)
!
!
      call read_mul_int8_b(bbuf, cast_long(ione), nnod_ele_b)
      call read_mul_int8_b(bbuf, cast_long(ione), eletype)
!
      allocate(nele_lc(nprocs))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), nele_lc)
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
     &         (nprocs, nele, nnod_ele, iele_global, ie_psf,            &
     &          itmp1_mp, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint_gl), intent(inout) :: iele_global(nele)
      integer(kind = kint_gl), intent(inout) :: ie_psf(nele,nnod_ele)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: nd
      integer(kind = kint_gl) :: i
!
!
      do nd = 1, nnod_ele
        call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
        call read_mul_int8_b(bbuf, nele, ie_psf(1,nd))
      end do
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
