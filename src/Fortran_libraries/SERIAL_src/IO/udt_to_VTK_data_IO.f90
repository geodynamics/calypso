!>@file  udt_to_VTK_data_IO.f90
!!       module udt_to_VTK_data_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief ucd data into VTK format IO
!!
!!@verbatim
!!      subroutine write_ucd_field_to_VTK(id_vtk, ucd)
!!      subroutine write_ucd_mesh_to_VTK(id_vtk, ucd)
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_udt_field_from_VTK(id_vtk, ucd)
!!      subroutine read_alloc_udt_field_from_VTK(id_vtk, ucd, iend)
!!      subroutine read_ucd_mesh_from_VTK(id_vtk, ucd)
!!      subroutine read_alloc_ucd_mesh_from_VTK(id_vtk, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
      module udt_to_VTK_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ucd_data
!
      use vtk_data_IO
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
      subroutine write_ucd_field_to_VTK(id_vtk, ucd)
!
      integer(kind = kint), intent(in) :: id_vtk
      type(ucd_data), intent(in) :: ucd
!
!
      call write_vtk_fields_head(id_vtk, ucd%nnod)
      call write_vtk_data(id_vtk, ucd%nnod, ucd%num_field,              &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      end subroutine write_ucd_field_to_VTK
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_mesh_to_VTK(id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      type(ucd_data), intent(in) :: ucd
!
!
      call write_vtk_node_head(id_vtk, ucd%nnod)
      call write_vtk_each_field                                         &
     &   (id_vtk, ucd%nnod, ithree, ucd%nnod, ucd%xx)
!
      call write_vtk_connect_data                                       &
     &   (id_vtk, ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie)
!
      end subroutine write_ucd_mesh_to_VTK
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_udt_field_from_VTK(id_vtk, ucd)
!
      use skip_comment_f
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) ::  id_vtk
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: i_field, iflag_end, ncomp_field, iend
      character(len=kchara)  :: field_name
      real(kind = kreal), allocatable :: d_tmp(:,:)
      integer(kind=kint_gl) :: nnod
!
!
      call read_vtk_fields_head(id_vtk, nnod, iend)
      if(iend .gt. 0) write(*,*) 'Error in file'
      if(nnod .ne. ucd%nnod) write(*,*) 'Error in number of node'
!
      i_field = 0
      do
        call read_vtk_each_field_head                                   &
     &    (id_vtk, iflag_end, ncomp_field, field_name)
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
        call read_vtk_each_field                                        &
     &     (id_vtk, ucd%nnod, ncomp_field, ucd%nnod, d_tmp(1,1))
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
      end subroutine read_udt_field_from_VTK
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_udt_field_from_VTK(id_vtk, ucd, iend)
!
      integer(kind = kint), intent(in) ::  id_vtk
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint), intent(inout) ::  iend
!
      type(ucd_data) :: tmp
!
      integer(kind = kint) :: iflag_end, ncomp_field
      character(len=kchara)  :: field_name
      real(kind = kreal), allocatable :: d_tmp(:,:)
!
!
      call read_vtk_fields_head(id_vtk, ucd%nnod, iend)
      if(iend .gt. 0) write(*,*) 'Error in file'
!
      tmp%nnod =      ucd%nnod
      ucd%num_field = 0
      ucd%ntot_comp = 0
      call allocate_ucd_phys_name(ucd)
      call allocate_ucd_phys_data(ucd)
!
      do
        call read_vtk_each_field_head                                   &
     &    (id_vtk, iflag_end, ncomp_field, field_name)
        if(iflag_end .ne. izero) exit
!
        tmp%num_field = ucd%num_field
        tmp%ntot_comp = ucd%ntot_comp
        call allocate_ucd_phys_name(tmp)
        call allocate_ucd_phys_data(tmp)
!
        call append_new_ucd_field_name(field_name, ncomp_field,         &
     &      tmp, ucd)
!
        allocate(d_tmp(ucd%nnod,ncomp_field))
!
        call read_vtk_each_field                                        &
     &     (id_vtk, ucd%nnod, ncomp_field, ucd%nnod, d_tmp(1,1))
        call append_new_ucd_field_data(ncomp_field, d_tmp, tmp, ucd)
!
        deallocate(d_tmp)
        call deallocate_ucd_data(tmp)
!
        iflag_end = izero
      end do
!
      end subroutine read_alloc_udt_field_from_VTK
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_from_VTK(id_vtk, ucd)
!
      integer(kind = kint), intent(in) :: id_vtk
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: nnod_ele
      integer(kind = kint_gl) :: nnod, nele
      integer(kind = kint_gl) :: inod, iele
!
!
      call read_vtk_node_head(id_vtk, nnod)
      if(nnod .ne. ucd%nnod) write(*,*) 'Error in number of node'
!
      call read_vtk_each_field                                          &
     &   (id_vtk, ucd%nnod, ithree, ucd%nnod, ucd%xx)
!
!$omp parallel do
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = inod
      end do
!$omp end parallel do
!
      call read_vtk_connect_head(id_vtk, nele, nnod_ele)
      if(nele .ne. ucd%nele) write(*,*) 'Error in number of element'
      if(nnod_ele .ne. ucd%nnod_4_ele) write(*,*)                       &
     &                       'Error in number of node in each element'
!
      call read_vtk_connect_data                                        &
     &   (id_vtk, ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie)
      call read_vtk_cell_type(id_vtk, ucd%nele)
!
!$omp parallel do
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = iele
      end do
!$omp end parallel do
!
      end subroutine  read_ucd_mesh_from_VTK
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_ucd_mesh_from_VTK(id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: inod, iele
!
!
      call read_vtk_node_head(id_vtk, ucd%nnod)
      call allocate_ucd_node(ucd)
!
      call read_vtk_each_field                                          &
     &   (id_vtk, ucd%nnod, ithree, ucd%nnod, ucd%xx)
!
!$omp parallel do
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = inod
      end do
!$omp end parallel do
!
      call read_vtk_connect_head(id_vtk, ucd%nele, ucd%nnod_4_ele)
      call allocate_ucd_ele(ucd)
!
      call read_vtk_connect_data                                        &
     &   (id_vtk, ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie)
      call read_vtk_cell_type(id_vtk, ucd%nele)
!
!$omp parallel do
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = iele
      end do
!$omp end parallel do
!
      end subroutine read_alloc_ucd_mesh_from_VTK
!
! -----------------------------------------------------------------------
!
      end module udt_to_VTK_data_IO
