!>@file  udt_type_data_IO.f90
!!       module udt_type_data_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief  UCD format data IO
!!
!!@verbatim
!!      subroutine write_udt_type_fields(id_ucd, ucd)
!!      subroutine write_ucd_type_mesh(id_ucd, ucd)
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_udt_field(id_ucd, ucd)
!!      subroutine read_alloc_udt_field(id_ucd, ucd)
!!      subroutine read_ucd_mesh_data(id_ucd, ucd)
!!      subroutine read_alloc_ucd_mesh_data(id_ucd, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!
      module udt_type_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ucd_data
!
      use udt_data_IO
      use ucd_data_to_buffer
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_udt_type_fields(id_ucd, ucd)
!
      integer(kind = kint), intent(in) :: id_ucd
      type(ucd_data), intent(in) :: ucd
!
!   =====================
!   [1] data on nodes
!   =====================
!    (A) header for fields
!     *  number of fields, number of components, ...
!   ===================================================================
!    (B) nodal data name
!   =====================
      if(ucd%num_field .gt. 0) then
        call write_udt_field_header                                     &
     &     (id_ucd, ucd%num_field, ucd%num_comp, ucd%phys_name)
!
!    (C) fields
!     *  global node ID, results
!   ===================================================
        call write_ucd_field_data(id_ucd, ucd%nnod, ucd%ntot_comp,      &
     &      ucd%nnod, ucd%inod_global, ucd%d_ucd)
      end if
!
      end subroutine write_udt_type_fields
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_type_mesh(id_ucd, ucd)
!
      integer(kind = kint), intent(in) :: id_ucd
      type(ucd_data), intent(in) :: ucd
!
!
!   =====================
!   [1] node data
!   =====================
!     * number of data
!     * num. of node, elements, nodal field components, 
!       elemental fields components, and num of model
!   ====================================================
      call write_udt_mesh_header                                        &
     &   (id_ucd, ucd%nnod, ucd%nele, ucd%ntot_comp)
!
!   =====================
!   [2] position of node
!   =====================
!     global node ID, position
!   ==================================
      call write_ucd_field_data(id_ucd, ucd%nnod, ithree, ucd%nnod,     &
     &    ucd%inod_global, ucd%xx)
!
!   =====================
!   [3] element data
!   =====================
!     * global element ID, node connection (by global ID)
!   ===========================================================
      call write_ucd_mesh_connect(id_ucd, ucd%nele, ucd%nnod_4_ele,     &
     &    ucd%nele, ucd%iele_global, ucd%ie)
!
      end subroutine write_ucd_type_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_udt_field(id_ucd, ucd)
!
      integer(kind = kint), intent(in) :: id_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
      call read_udt_field_name                                          &
     &   (id_ucd, ucd%num_field, ucd%num_comp, ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
      call read_ucd_field_data                                          &
     &   (id_ucd, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
!
      end subroutine read_udt_field
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_udt_field(id_ucd, ucd)
!
      integer(kind = kint), intent(in) :: id_ucd
      type(ucd_data), intent(inout) :: ucd
!
!
      call read_udt_field_num(id_ucd, ucd%num_field)
      call allocate_ucd_phys_name(ucd)
!
      call read_udt_field_name                                          &
     &   (id_ucd, ucd%num_field, ucd%num_comp, ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine read_alloc_udt_field
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_data(id_ucd, ucd)
!
      integer(kind = kint), intent(in) :: id_ucd
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint_gl) :: nnod, nele
!
!
      call read_ucd_mesh_header(id_ucd, nnod, nele, ucd%ntot_comp)
!
      if(nnod .ne. ucd%nnod) write(*,*) 'Error in number of node'
      if(nele .ne. ucd%nele) write(*,*) 'Error in number of element'
!
      call read_ucd_node_data                                           &
     &   (id_ucd, ucd%nnod, ucd%inod_global, ucd%xx)
      call read_ucd_ele_connect                                         &
     &   (id_ucd, ucd%nele, ucd%nnod_4_ele, ucd%iele_global, ucd%ie)
!
      end subroutine  read_ucd_mesh_data
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_ucd_mesh_data(id_ucd, ucd)
!
      integer(kind = kint), intent(in) :: id_ucd
      type(ucd_data), intent(inout) :: ucd
!
      character(len = 4096) :: textbuf
      character(len = kchara) :: eleflag
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: itmp
!
!
      call read_ucd_mesh_header                                         &
     &   (id_ucd, ucd%nnod, ucd%nele, ucd%ntot_comp)
!
      call allocate_ucd_node(ucd)
      call allocate_ucd_ele(ucd)
!
      call read_ucd_node_data                                           &
     &   (id_ucd, ucd%nnod, ucd%inod_global, ucd%xx)
!
      if(ucd%nele .le. 0) then
        ucd%nnod_4_ele = 0
        call allocate_ucd_ele(ucd)
      else
        read(id_ucd,'(a)') textbuf
        read(textbuf,*) itmp, itmp, eleflag
        ucd%nnod_4_ele = nnod_ele_by_ucd_eletype(eleflag)
!
        call allocate_ucd_ele(ucd)
        read(textbuf,*) ucd%iele_global(1), itmp,                       &
     &                 eleflag, ucd%ie(1,1:ucd%nnod_4_ele)
      end if
!
      do iele = 2, ucd%nele
        read(id_ucd,*) ucd%iele_global(iele), itmp,                     &
     &                eleflag, ucd%ie(iele,1:ucd%nnod_4_ele)
      end do
!
      end subroutine read_alloc_ucd_mesh_data
!
! -----------------------------------------------------------------------
!
      end module udt_type_data_IO
