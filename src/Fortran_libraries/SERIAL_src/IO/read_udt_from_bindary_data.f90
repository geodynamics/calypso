!>@file  read_udt_from_bindary_data.f90
!!       module read_udt_from_bindary_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!      subroutine read_psf_bin_field_data                              &
!!     &         (nprocs, ucd_b, bbuf, itmp1_mp)
!!      subroutine read_alloc_psf_bin_field_data                        &
!!     &         (nprocs, ucd_b, bbuf, itmp1_mp)
!!      subroutine read_psf_bin_grid_data(nprocs, ucd_b, bbuf, itmp1_mp)
!!      subroutine read_alloc_psf_bin_grid_data                         &
!!        type(ucd_data), intent(inout) :: ucd_b
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module read_udt_from_bindary_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_binary_IO_buffer
      use t_ucd_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_field_data                                &
     &         (nprocs, ucd_b, bbuf, itmp1_mp)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer :: nprocs2
      integer(kind = kint) :: num_field, ntot_comp
!
!
      call read_one_integer_b(bbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
!
      call read_psf_phys_num_bin                                        &
     &   (nprocs, ucd_b%nnod, num_field, itmp1_mp, bbuf)
      if(num_field .ne. ucd_b%num_field) write(*,*)                     &
     &                             'Error in number of field'
!
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ntot_comp, ucd_b%num_comp,                   &
     &    ucd_b%phys_name, bbuf)
      if(ntot_comp .ne. ucd_b%ntot_comp) write(*,*)                     &
     &                             'Error in number of total component'

      call read_psf_phys_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%ntot_comp, ucd_b%d_ucd,             &
     &    itmp1_mp, bbuf)
!
      end subroutine read_psf_bin_field_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_field_data                          &
     &         (nprocs, ucd_b, bbuf, itmp1_mp)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer :: nprocs2
!
!
      call read_one_integer_b(bbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
!
      call read_psf_phys_num_bin                                        &
     &   (nprocs, ucd_b%nnod, ucd_b%num_field, itmp1_mp, bbuf)
!
      call allocate_ucd_phys_name(ucd_b)
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ucd_b%ntot_comp, ucd_b%num_comp,             &
     &    ucd_b%phys_name, bbuf)

      call allocate_ucd_phys_data(ucd_b)
      call read_psf_phys_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%ntot_comp, ucd_b%d_ucd,             &
     &    itmp1_mp, bbuf)
!
      end subroutine read_alloc_psf_bin_field_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_grid_data(nprocs, ucd_b, bbuf, itmp1_mp)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(inout) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nnod, nele
      integer(kind = kint) :: nnod_4_ele
!
!
      call read_psf_node_num_bin(nprocs, nnod, itmp1_mp, bbuf)
      if(nnod .ne. ucd_b%nnod) write(*,*) 'Error in number of node'
!
      call read_psf_node_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%inod_global, ucd_b%xx,              &
     &    itmp1_mp, bbuf)
!
      call read_psf_ele_num_bin                                         &
     &   (nprocs, nele, nnod_4_ele, itmp1_mp, bbuf)
      if(nele .ne. ucd_b%nele) write(*,*) 'Error in number of element'
      if(nnod_4_ele .ne. ucd_b%nnod_4_ele) write(*,*)                   &
     &                       'Error in number of node in each element'
!
      call read_psf_ele_connect_bin                                     &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele,                         &
     &    ucd_b%iele_global, ucd_b%ie, itmp1_mp, bbuf)
!
      end subroutine read_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_grid_data                           &
     &         (nprocs, ucd_b, bbuf, itmp1_mp)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(inout) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_psf_node_num_bin(nprocs, ucd_b%nnod, itmp1_mp, bbuf)
!
      call allocate_ucd_node(ucd_b)
      call read_psf_node_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%inod_global, ucd_b%xx,              &
     &    itmp1_mp, bbuf)
!
      call read_psf_ele_num_bin                                         &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele, itmp1_mp, bbuf)
!
      call allocate_ucd_ele(ucd_b)
      call read_psf_ele_connect_bin                                     &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele,                         &
     &    ucd_b%iele_global, ucd_b%ie, itmp1_mp, bbuf)
!
      end subroutine read_alloc_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      end module read_udt_from_bindary_data
