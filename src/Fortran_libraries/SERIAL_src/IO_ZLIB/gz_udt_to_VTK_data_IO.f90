!>@file  gz_udt_to_VTK_data_IO.f90
!!       module gz_udt_to_VTK_data_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief ucd data into VTK format IO
!!
!!@verbatim
!!      subroutine write_gz_ucd_field_to_VTK(ucd, zbuf)
!!      subroutine write_gz_ucd_mesh_to_VTK(ucd, zbuf)
!!        type(ucd_data), intent(in) :: ucd
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_gz_udt_field_from_VTK(ucd, zbuf)
!!      subroutine read_alloc_gz_udt_fld_from_VTK(ucd, zbuf)
!!      subroutine read_gz_ucd_grd_from_VTK(ucd, zbuf)
!!      subroutine read_alloc_gz_ucd_grd_from_VTK(ucd, zbuf)
!!        type(ucd_data), intent(inout) :: ucd
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!!
      module gz_udt_to_VTK_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ucd_data
!
      use gz_vtk_data_IO
      use vtk_data_to_buffer
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_field_to_VTK(ucd, zbuf)
!
      type(ucd_data), intent(in) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call write_gz_vtk_fields_head(ucd%nnod, zbuf)
      call write_gz_vtk_data(ucd%nnod, ucd%num_field,                   &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd, zbuf)
!
      end subroutine write_gz_ucd_field_to_VTK
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_mesh_to_VTK(ucd, zbuf)
!
      type(ucd_data), intent(in) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call write_gz_vtk_node_head(ucd%nnod, zbuf)
      call write_gz_vtk_each_field                                      &
     &   (ucd%nnod, n_vector, ucd%nnod, ucd%xx, zbuf)
!
      call write_gz_vtk_connect_head(ucd%nele, ucd%nnod_4_ele, zbuf)
      call write_gz_vtk_connect_data                                    &
     &   (ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie, zbuf)
!
      call write_gz_vtk_cell_type(ucd%nele, ucd%nnod_4_ele, zbuf)
!
      end subroutine write_gz_ucd_mesh_to_VTK
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_udt_field_from_VTK(ucd, zbuf)
!
      use skip_comment_f
      use copy_between_two_fields
!
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i_field, iflag_end, ncomp_field
      character(len=kchara)  :: field_name
      real(kind = kreal), allocatable :: d_tmp(:,:)
      integer(kind=kint_gl) :: nnod
!
!
      call read_gz_vtk_fields_head(nnod, zbuf)
      if(nnod .ne. ucd%nnod) write(*,*) 'Error in number of node'
!
      i_field = 0
      do
        call read_gz_vtk_each_field_head                                &
     &     (iflag_end, ncomp_field, field_name, zbuf)
        if(iflag_end .ne. izero) exit
!
        i_field = i_field + 1
        if(ucd%num_comp(i_field) .ne. ncomp_field) then
          write(*,*) i_field, ' field has wrong number of component'
        end if
        if(cmp_no_case(ucd%phys_name(i_field), field_name)) then
          write(*,*) i_field, ' field name is wrong'
        end if
!
        allocate(d_tmp(ucd%nnod,ncomp_field))
!
        call read_gz_vtk_each_field                                     &
     &     (ucd%nnod, ncomp_field, ucd%nnod, d_tmp(1,1), zbuf)
        call copy_fields_2_fields(ncomp_field,                          &
     &      ione, ucd%nnod, ncomp_field, d_tmp(1,1),                    &
     &      i_field, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
        deallocate(d_tmp)
!
        iflag_end = izero
      end do
      if(i_field .ne. ucd%num_field) write(*,*)                         &
     &                             'Error in number of field'
!
      end subroutine read_gz_udt_field_from_VTK
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_udt_fld_from_VTK(ucd, zbuf)
!
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(ucd_data) :: tmp
!
      integer(kind = kint) :: iflag_end, ncomp_field
      character(len=kchara)  :: field_name
      real(kind = kreal), allocatable :: d_tmp(:,:)
!
!
      call read_gz_vtk_fields_head(ucd%nnod, zbuf)
!
      tmp%nnod = ucd%nnod
      ucd%num_field = 0
      ucd%ntot_comp = 0
      call allocate_ucd_phys_name(ucd)
      call allocate_ucd_phys_data(ucd)
!
      do
        call read_gz_vtk_each_field_head                                &
     &     (iflag_end, ncomp_field, field_name, zbuf)
        if(iflag_end .ne. izero) exit
!
        tmp%num_field = ucd%num_field
        tmp%ntot_comp = ucd%ntot_comp
        call allocate_ucd_phys_name(tmp)
        call allocate_ucd_phys_data(tmp)
!
        call append_new_ucd_field_name                                  &
     &     (field_name, ncomp_field, tmp, ucd)
!
        allocate(d_tmp(ucd%nnod,ncomp_field))
!
        call read_gz_vtk_each_field                                     &
     &     (ucd%nnod, ncomp_field, ucd%nnod, d_tmp(1,1), zbuf)
        call append_new_ucd_field_data(ncomp_field, d_tmp,              &
     &      tmp, ucd)
!
        deallocate(d_tmp)
        call deallocate_ucd_data(tmp)
!
        iflag_end = izero
      end do
!
      end subroutine read_alloc_gz_udt_fld_from_VTK
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_gz_ucd_grd_from_VTK(ucd, zbuf)
!
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: nnod_ele
      integer(kind = kint_gl) :: nnod, nele
      integer(kind = kint_gl) :: inod, iele
!
!
      call read_gz_vtk_node_head(nnod, zbuf)
      if(nnod .ne. ucd%nnod) write(*,*) 'Error in number of node'
!
      call read_gz_vtk_each_field                                       &
     &   (ucd%nnod, ithree, ucd%nnod, ucd%xx, zbuf)
!
!$omp parallel do
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = inod
      end do
!$omp end parallel do
!
      call read_gz_vtk_connect_head(nele, nnod_ele, zbuf)
      if(nele .ne. ucd%nele) write(*,*) 'Error in number of element'
      if(nnod_ele .ne. ucd%nnod_4_ele) write(*,*)                       &
     &                       'Error in number of node in each element'
!
      call read_gz_vtk_connect_data                                     &
     &   (ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie, zbuf)
      call read_gz_vtk_cell_type(ucd%nele, zbuf)
!
!$omp parallel do
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = iele
      end do
!$omp end parallel do
!
      end subroutine  read_gz_ucd_grd_from_VTK
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_gz_ucd_grd_from_VTK(ucd, zbuf)
!
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: inod, iele
!
!
      call read_gz_vtk_node_head(ucd%nnod, zbuf)
      call allocate_ucd_node(ucd)
!
      call read_gz_vtk_each_field                                       &
     &   (ucd%nnod, ithree, ucd%nnod, ucd%xx, zbuf)
!
!$omp parallel do
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = inod
      end do
!$omp end parallel do
!
      call read_gz_vtk_connect_head(ucd%nele, ucd%nnod_4_ele, zbuf)
      call allocate_ucd_ele(ucd)
!
      call read_gz_vtk_connect_data                                     &
     &   (ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie, zbuf)
      call read_gz_vtk_cell_type(ucd%nele, zbuf)
!
!$omp parallel do
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = iele
      end do
!$omp end parallel do
!
      end subroutine read_alloc_gz_ucd_grd_from_VTK
!
! -----------------------------------------------------------------------
!
      end module gz_udt_to_VTK_data_IO
