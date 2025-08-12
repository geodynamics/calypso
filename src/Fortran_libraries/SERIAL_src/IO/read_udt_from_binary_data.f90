!>@file  read_udt_from_binary_data.f90
!!       module read_udt_from_binary_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!      subroutine read_psf_bin_time_data                               &
!!     &         (np_read, i_time_step_IO, time_IO, delta_t_IO, bbuf)
!!      subroutine read_psf_bin_field_data(np_read, ucd_b, bbuf)
!!      subroutine read_alloc_psf_bin_field_data(np_read, ucd_b, bbuf)
!!      subroutine read_psf_bin_grid_data(np_read, ucd_b, bbuf)
!!      subroutine read_alloc_psf_bin_grid_data(np_read, ucd_b, bbuf)
!!        type(ucd_data), intent(inout) :: ucd_b
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module read_udt_from_binary_data
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
      subroutine read_psf_bin_time_data                                 &
     &         (np_read, i_time_step_IO, time_IO, delta_t_IO, bbuf)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(inout) :: np_read
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_one_integer_b(bbuf, np_read)
      call read_one_integer_b(bbuf, i_time_step_IO)
      call read_one_real_b(bbuf, time_IO)
      call read_one_real_b(bbuf, delta_t_IO)
!
      end subroutine read_psf_bin_time_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_field_data(np_read, ucd_b, bbuf)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: num_field, ntot_comp
!
!
      call read_psf_phys_num_bin                                        &
     &   (np_read, ucd_b%nnod, num_field, bbuf)
      if(num_field .ne. ucd_b%num_field) write(*,*)                     &
     &                             'Error in number of field'
!
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ntot_comp, ucd_b%num_comp,                   &
     &    ucd_b%phys_name, bbuf)
      if(ntot_comp .ne. ucd_b%ntot_comp) write(*,*)                     &
     &                             'Error in number of total component'

      call read_psf_phys_data_bin                                       &
     &   (np_read, ucd_b%nnod, ucd_b%ntot_comp, ucd_b%d_ucd, bbuf)
!
      end subroutine read_psf_bin_field_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_field_data(np_read, ucd_b, bbuf)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_psf_phys_num_bin                                        &
     &   (np_read, ucd_b%nnod, ucd_b%num_field, bbuf)
!
      call allocate_ucd_phys_name(ucd_b)
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ucd_b%ntot_comp, ucd_b%num_comp,             &
     &    ucd_b%phys_name, bbuf)

      call allocate_ucd_phys_data(ucd_b)
      call read_psf_phys_data_bin                                       &
     &   (np_read, ucd_b%nnod, ucd_b%ntot_comp, ucd_b%d_ucd, bbuf)
!
      end subroutine read_alloc_psf_bin_field_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_psf_bin_grid_data(np_read, ucd_b, bbuf)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: nnod, nele
      integer(kind = kint) :: nnod_4_ele
!
!
      call read_psf_node_num_bin(np_read, nnod, bbuf)
      if(nnod .ne. ucd_b%nnod) write(*,*) 'Error in number of node'
!
      call read_psf_node_data_bin                                       &
     &   (np_read, ucd_b%nnod, ucd_b%inod_global, ucd_b%xx, bbuf)
!
      call read_psf_ele_num_bin(np_read, nele, nnod_4_ele, bbuf)
      if(nele .ne. ucd_b%nele) write(*,*) 'Error in number of element'
      if(nnod_4_ele .ne. ucd_b%nnod_4_ele) write(*,*)                   &
     &                       'Error in number of node in each element'
!
      call read_psf_ele_connect_bin                                     &
     &   (np_read, ucd_b%nele, ucd_b%nnod_4_ele,                        &
     &    ucd_b%iele_global, ucd_b%ie, bbuf)
!
      end subroutine read_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_grid_data(np_read, ucd_b, bbuf)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_b
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_psf_node_num_bin(np_read, ucd_b%nnod, bbuf)
!
      call allocate_ucd_node(ucd_b)
      call read_psf_node_data_bin                                       &
     &   (np_read, ucd_b%nnod, ucd_b%inod_global, ucd_b%xx, bbuf)
!
      call read_psf_ele_num_bin                                         &
     &   (np_read, ucd_b%nele, ucd_b%nnod_4_ele, bbuf)
!
      call allocate_ucd_ele(ucd_b)
      call read_psf_ele_connect_bin                                     &
     &   (np_read, ucd_b%nele, ucd_b%nnod_4_ele,                        &
     &    ucd_b%iele_global, ucd_b%ie, bbuf)
!
      end subroutine read_alloc_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      end module read_udt_from_binary_data
