!>@file  gz_read_udt_from_bin_data.f90
!!       module gz_read_udt_from_bin_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!      subroutine gz_read_psf_bin_time_data                           &
!!     &         (np_read, i_time_step_IO, time_IO, delta_t_IO, zbuf)
!!      subroutine gz_read_psf_bin_field_data(np_read, ucd_z, zbuf)
!!      subroutine gz_read_alloc_psf_bin_fld_data(np_read, ucd_z, zbuf)
!!      subroutine gz_read_psf_bin_grid_data(np_read, ucd_z, zbuf)
!!      subroutine gz_read_alloc_psf_bin_grid_data(np_read, ucd_z, zbuf)
!!        type(ucd_data), intent(inout) :: ucd_z
!!        type(binary_IO_buffer), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_read_udt_from_bin_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_buffer_4_gzip
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
      subroutine gz_read_psf_bin_time_data                             &
     &         (np_read, i_time_step_IO, time_IO, delta_t_IO, zbuf)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(inout) :: np_read
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_read_one_integer_b(zbuf, np_read)
!
      call gz_read_one_integer_b(zbuf, i_time_step_IO)
      call gz_read_one_real_b(zbuf, time_IO)
      call gz_read_one_real_b(zbuf, delta_t_IO)
!
      end subroutine gz_read_psf_bin_time_data
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_psf_bin_field_data(np_read, ucd_z, zbuf)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: num_field, ntot_comp
!
!
      call read_psf_phys_num_bin_gz                                     &
     &   (np_read, ucd_z%nnod, num_field, zbuf)
      if(num_field .ne. ucd_z%num_field) write(*,*)                     &
     &                             'Error in number of field'
!
      call read_psf_phys_name_bin_gz                                    &
     &   (ucd_z%num_field, ntot_comp, ucd_z%num_comp,                   &
     &    ucd_z%phys_name, zbuf)
      if(ntot_comp .ne. ucd_z%ntot_comp) write(*,*)                     &
     &                             'Error in number of total component'
!
      call read_psf_phys_data_bin_gz                                    &
     &   (np_read, ucd_z%nnod, ucd_z%ntot_comp, ucd_z%d_ucd, zbuf)
!
      end subroutine gz_read_psf_bin_field_data
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_fld_data(np_read, ucd_z, zbuf)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call read_psf_phys_num_bin_gz                                     &
     &   (np_read, ucd_z%nnod, ucd_z%num_field, zbuf)
!
      call allocate_ucd_phys_name(ucd_z)
      call read_psf_phys_name_bin_gz                                    &
     &   (ucd_z%num_field, ucd_z%ntot_comp, ucd_z%num_comp,             &
     &    ucd_z%phys_name, zbuf)
!
      call allocate_ucd_phys_data(ucd_z)
      call read_psf_phys_data_bin_gz                                    &
     &   (np_read, ucd_z%nnod, ucd_z%ntot_comp, ucd_z%d_ucd, zbuf)
!
      end subroutine gz_read_alloc_psf_bin_fld_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gz_read_psf_bin_grid_data(np_read, ucd_z, zbuf)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: nnod, nele
      integer(kind = kint) :: nnod_4_ele
!
!
      call read_psf_node_num_bin_gz(np_read, nnod, zbuf)
      if(nnod .ne. ucd_z%nnod) write(*,*) 'Error in number of node'
!
      call read_psf_node_data_bin_gz                                    &
     &   (np_read, ucd_z%nnod, ucd_z%inod_global, ucd_z%xx, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (np_read, nele, nnod_4_ele, zbuf)
      if(nele .ne. ucd_z%nele) write(*,*) 'Error in number of element'
      if(nnod_4_ele .ne. ucd_z%nnod_4_ele) write(*,*)                   &
     &                       'Error in number of node in each element'
!
      call read_psf_ele_connect_bin_gz                                  &
     &   (np_read, ucd_z%nele, ucd_z%nnod_4_ele,                        &
     &    ucd_z%iele_global, ucd_z%ie, zbuf)
!
      end subroutine gz_read_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_grid_data(np_read, ucd_z, zbuf)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(in) :: np_read
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call read_psf_node_num_bin_gz(np_read, ucd_z%nnod, zbuf)
!
      call allocate_ucd_node(ucd_z)
      call read_psf_node_data_bin_gz                                    &
     &   (np_read, ucd_z%nnod, ucd_z%inod_global, ucd_z%xx, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (np_read, ucd_z%nele, ucd_z%nnod_4_ele, zbuf)
!
      call allocate_ucd_ele(ucd_z)
      call read_psf_ele_connect_bin_gz                                  &
     &   (np_read, ucd_z%nele, ucd_z%nnod_4_ele,                        &
     &    ucd_z%iele_global, ucd_z%ie, zbuf)
!
      end subroutine gz_read_alloc_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      end module gz_read_udt_from_bin_data
