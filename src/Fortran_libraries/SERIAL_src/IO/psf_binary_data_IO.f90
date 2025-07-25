!>@file  psf_binary_data_IO.f90
!!       module psf_binary_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine read_psf_node_num_bin(np_read, nnod, bbuf)
!!        integer, intent(in) :: np_read
!!        integer(kind=kint_gl), intent(inout) :: nnod
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine read_psf_phys_num_bin(np_read, nnod, num_field, bbuf)
!!        integer, intent(in) :: np_read
!!        integer(kind=kint_gl), intent(in) :: nnod
!!        integer(kind=kint), intent(inout) :: num_field
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine read_psf_phys_name_bin                               &
!!     &         (num_field, ntot_comp, ncomp_field, field_name, bbuf)
!!        integer(kind=kint), intent(in) :: num_field
!!        integer(kind=kint), intent(inout) :: ntot_comp
!!        integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!!        character(len=kchara), intent(inout) :: field_name(num_field)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine read_psf_phys_data_bin                               &
!!     &         (np_read, nnod, ntot_comp, d_nod, bbuf)
!!        integer, intent(in) :: np_read
!!        integer(kind=kint_gl), intent(in) :: nnod
!!        integer(kind=kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine write_sgl_psf_node_num_bin(nnod, bbuf)
!!        integer(kind=kint_gl), intent(in) :: nnod
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine write_sgl_psf_phys_num_bin(nnod, num_field, bbuf)
!!        integer(kind=kint_gl), intent(in) :: nnod
!!        integer(kind=kint), intent(in) :: num_field
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine write_sgl_psf_phys_name_bin                          &
!!     &         (num_field, ncomp_field, field_name, bbuf)
!!        integer(kind=kint), intent(in) :: num_field
!!        integer(kind=kint), intent(in) :: ncomp_field(num_field)
!!        character(len=kchara), intent(in) :: field_name(num_field)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!      subroutine write_sgl_psf_phys_data_bin(nnod, ntot_comp, d_nod,  &
!!     &                                       bbuf)
!!        integer(kind=kint_gl), intent(in) :: nnod
!!        integer(kind=kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module psf_binary_data_IO
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
      subroutine write_sgl_psf_node_num_bin(nnod, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint_gl), intent(in) :: nnod
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nnod64(1)
!
!
      nnod64(1) = 8*nnod
      call write_mul_int8_b(cast_long(ione), nnod64, bbuf)
      nnod64(1) = nnod
      call write_mul_int8_b(cast_long(ione), nnod64, bbuf)
!
      end subroutine write_sgl_psf_node_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine write_sgl_psf_phys_num_bin(nnod, num_field, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_sgl_psf_node_num_bin(nnod, bbuf)
      call write_one_integer_b(num_field, bbuf)
!
      end subroutine write_sgl_psf_phys_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine write_sgl_psf_phys_name_bin                            &
     &         (num_field, ncomp_field, field_name, bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_mul_integer_b(cast_long(num_field), ncomp_field, bbuf)
      call write_mul_character_b(num_field, field_name, bbuf)
!
      end subroutine write_sgl_psf_phys_name_bin
!
! -----------------------------------------------------------------------
!
      subroutine write_sgl_psf_phys_data_bin(nnod, ntot_comp, d_nod,    &
     &                                       bbuf)
!
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: itmp1_mp(:)
      integer(kind = kint) :: nd
!
!
      allocate(itmp1_mp(ione))
      itmp1_mp(1) = nnod
      do nd = 1, ntot_comp
        call write_mul_int8_b(cast_long(ione), itmp1_mp, bbuf)
        call write_1d_vector_b(nnod, d_nod(1,nd), bbuf)
      end do
      deallocate(itmp1_mp)
!
      end subroutine write_sgl_psf_phys_data_bin
!
! -----------------------------------------------------------------------
!
      end module psf_binary_data_IO
