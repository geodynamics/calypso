!>@file  gz_read_udt_from_bin_data.f90
!!       module gz_read_udt_from_bin_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!      subroutine gz_read_psf_bin_field_data                           &
!!     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!!      subroutine gz_read_alloc_psf_bin_fld_data                       &
!!     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!!      subroutine gz_read_psf_bin_grid_data                            &
!!     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!!      subroutine gz_read_alloc_psf_bin_grid_data                      &
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
      subroutine gz_read_psf_bin_field_data                             &
     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(in) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: nprocs2
      integer(kind = kint) :: num_field, ntot_comp
!
!
      call gz_read_one_integer_b(zbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
!
      call read_psf_phys_num_bin_gz                                     &
     &   (nprocs, ucd_z%nnod, num_field, itmp1_mp, zbuf)
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
     &   (nprocs, ucd_z%nnod, ucd_z%ntot_comp, ucd_z%d_ucd,             &
     &    itmp1_mp, zbuf)
!
      end subroutine gz_read_psf_bin_field_data
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_fld_data                         &
     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(in) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer :: nprocs2
!
!
      call gz_read_one_integer_b(zbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
!
      call read_psf_phys_num_bin_gz                                     &
     &   (nprocs, ucd_z%nnod, ucd_z%num_field, itmp1_mp, zbuf)
!
      call allocate_ucd_phys_name(ucd_z)
      call read_psf_phys_name_bin_gz                                    &
     &   (ucd_z%num_field, ucd_z%ntot_comp, ucd_z%num_comp,             &
     &    ucd_z%phys_name, zbuf)
!
      call allocate_ucd_phys_data(ucd_z)
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%ntot_comp, ucd_z%d_ucd,             &
     &    itmp1_mp, zbuf)
!
      end subroutine gz_read_alloc_psf_bin_fld_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gz_read_psf_bin_grid_data                              &
     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(inout) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: nnod, nele
      integer(kind = kint) :: nnod_4_ele
!
!
      call read_psf_node_num_bin_gz(nprocs, nnod, itmp1_mp, zbuf)
      if(nnod .ne. ucd_z%nnod) write(*,*) 'Error in number of node'
!
      call read_psf_node_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%inod_global, ucd_z%xx,              &
     &    itmp1_mp, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (nprocs, nele, nnod_4_ele, itmp1_mp, zbuf)
      if(nele .ne. ucd_z%nele) write(*,*) 'Error in number of element'
      if(nnod_4_ele .ne. ucd_z%nnod_4_ele) write(*,*)                   &
     &                       'Error in number of node in each element'
!
      call read_psf_ele_connect_bin_gz                                  &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele,                         &
     &    ucd_z%iele_global, ucd_z%ie, itmp1_mp, zbuf)
!
      end subroutine gz_read_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_grid_data                        &
     &         (nprocs, ucd_z, zbuf, itmp1_mp)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      integer, intent(inout) :: nprocs
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(ucd_data), intent(inout) :: ucd_z
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call read_psf_node_num_bin_gz(nprocs, ucd_z%nnod, itmp1_mp, zbuf)
!
      call allocate_ucd_node(ucd_z)
      call read_psf_node_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%inod_global, ucd_z%xx,              &
     &    itmp1_mp, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele, itmp1_mp, zbuf)
!
      call allocate_ucd_ele(ucd_z)
      call read_psf_ele_connect_bin_gz                                  &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele,                         &
     &    ucd_z%iele_global, ucd_z%ie, itmp1_mp, zbuf)
!
      end subroutine gz_read_alloc_psf_bin_grid_data
!
!  ---------------------------------------------------------------------
!
      end module gz_read_udt_from_bin_data
