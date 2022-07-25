!>@file  gz_read_psf_binary_data.f90
!!       module gz_read_psf_binary_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine read_psf_phys_num_bin_gz                             &
!!     &         (np_read, nnod, num_field, zbuf)
!!      subroutine read_psf_phys_name_bin_gz                            &
!!     &         (num_field, ntot_comp, ncomp_field, field_name, zbuf)
!!      subroutine read_psf_phys_data_bin_gz                            &
!!     &         (np_read, nnod, ntot_comp, d_nod, zbuf)
!!
!!      subroutine read_psf_node_num_bin_gz(np_read, nnod, zbuf)
!!      subroutine read_psf_node_data_bin_gz                            &
!!     &         (np_read, nnod, inod_global, xx, zbuf)
!!
!!      subroutine read_psf_ele_num_bin_gz                              &
!!     &         (np_read, nele, nnod_ele, zbuf)
!!      subroutine read_psf_ele_connect_bin_gz                          &
!!     &         (np_read, nele, nnod_ele, iele_global, ie_psf, zbuf)
!!        type(buffer_4_gzip), intent(inout)  :: zbuf
!!@endverbatim
!
      module gz_read_psf_binary_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_buffer_4_gzip
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_phys_num_bin_gz                               &
     &         (np_read, nnod, num_field, zbuf)
!
      use gz_binary_IO
!
      integer, intent(in) :: np_read
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind=kint), intent(inout) :: num_field
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl) :: nnod_gl
!
!
      call read_psf_node_num_bin_gz(np_read, nnod_gl, zbuf)
      if(nnod .ne. nnod_gl) stop 'Grid and field data are inconsistent'
!
      call gz_read_one_integer_b(zbuf, num_field)
!
      end subroutine read_psf_phys_num_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_name_bin_gz                              &
     &         (num_field, ntot_comp, ncomp_field, field_name, zbuf)
!
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint), intent(in) :: num_field
!
      integer(kind=kint), intent(inout) :: ntot_comp
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
!
      call gz_read_mul_integer_b                                        &
     &   (zbuf, cast_long(num_field), ncomp_field)
      call gz_read_mul_character_b                                      &
     &   (zbuf, cast_long(num_field), field_name)
      ntot_comp = sum(ncomp_field)
!
      end subroutine read_psf_phys_name_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_data_bin_gz                              &
     &         (np_read, nnod, ntot_comp, d_nod, zbuf)
!
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: itmp1_mp_gz(:)
      integer(kind = kint) :: nd
!
!
      allocate(itmp1_mp_gz(np_read))
      do nd = 1, ntot_comp
        call gz_read_mul_int8_b(zbuf, cast_long(np_read), itmp1_mp_gz)
        call gz_read_1d_vector_b(zbuf, nnod, d_nod(1,nd))
      end do
      deallocate(itmp1_mp_gz)
!
      end subroutine read_psf_phys_data_bin_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_num_bin_gz(np_read, nnod, zbuf)
!
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
!
      integer(kind=kint_gl), intent(inout) :: nnod
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(np_read))
      call gz_read_mul_int8_b(zbuf, cast_long(np_read), n_inter)
      call gz_read_mul_int8_b(zbuf, cast_long(np_read), n_inter)
      nnod = sum(n_inter)
!
!      write(*,*) 'n_inter', n_inter
      deallocate(n_inter)
!
      end subroutine read_psf_node_num_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_data_bin_gz                              &
     &         (np_read, nnod, inod_global, xx, zbuf)
!
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind=kint_gl), intent(inout) :: inod_global(nnod)
      real(kind = kreal), intent(inout) :: xx(nnod,3)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl) :: i
!
!
      call read_psf_phys_data_bin_gz(np_read, nnod, ithree, xx, zbuf)
!
!$omp parallel do
      do i = 1, nnod
        inod_global(i) = i
      end do
!$omp end parallel do
!
      end subroutine read_psf_node_data_bin_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_num_bin_gz                                &
     &         (np_read, nele, nnod_ele, zbuf)
!
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
!
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: nele_lc(:)
      integer(kind = kint_gl) :: eletype(1), nnod_ele_b(1)
!
!
      call gz_read_mul_int8_b(zbuf, cast_long(ione), nnod_ele_b)
      call gz_read_mul_int8_b(zbuf, cast_long(ione), eletype)
!
      allocate(nele_lc(np_read))
      call gz_read_mul_int8_b(zbuf, cast_long(np_read), nele_lc)
      call gz_read_mul_int8_b(zbuf, cast_long(np_read), nele_lc)
!
      nnod_ele = int(nnod_ele_b(1),KIND(nnod_ele))
      nele = sum(nele_lc)
!
!      write(*,*) 'nnod_ele_b', nnod_ele_b
!      write(*,*) 'nele_lc', nele_lc
!      write(*,*) 'eletype', eletype
!
      deallocate(nele_lc)
!
      end subroutine read_psf_ele_num_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_connect_bin_gz                            &
     &         (np_read, nele, nnod_ele, iele_global, ie_psf, zbuf)
!
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: np_read
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint_gl), intent(inout) :: iele_global(nele)
      integer(kind = kint_gl), intent(inout) :: ie_psf(nele,nnod_ele)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: itmp1_mp_gz(:)
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: nd
!
!
      allocate(itmp1_mp_gz(np_read))
      do nd = 1, nnod_ele
        call gz_read_mul_int8_b(zbuf, cast_long(np_read), itmp1_mp_gz)
        call gz_read_mul_int8_b(zbuf, nele, ie_psf(1,nd))
      end do
      deallocate(itmp1_mp_gz)
!
!$omp parallel do
      do i = 1, nele
        iele_global(i) = i
      end do
!$omp end parallel do
!
      end subroutine read_psf_ele_connect_bin_gz
!
! -----------------------------------------------------------------------
!
      end module gz_read_psf_binary_data
