!>@file  udt_file_IO.f90
!!       module udt_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief UCD format data IO
!!
!!@verbatim
!!      subroutine write_ucd_file(my_rank, istep, ucd)
!!      subroutine write_udt_file(my_rank, istep, ucd)
!!      subroutine write_grd_file(my_rank, ucd)
!!
!!      subroutine read_udt_file(my_rank, istep, ucd)
!!      subroutine read_and_alloc_udt_params(my_rank, istep, ucd)
!!      subroutine read_and_alloc_udt_file(my_rank, istep, ucd)
!!      subroutine read_and_alloc_ucd_file(my_rank, istep, nnod_ele, ucd)
!!      subroutine read_grd_file(my_rank, nnod_ele, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param my_rank  process ID
!!@param istep    step number for output
!!@param ucd      Structure for FEM field data IO
!
      module udt_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_field_file_format
!
      use t_ucd_data
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for UCD file
      integer(kind = kint), parameter, private :: id_ucd_file = 16
!
      private :: write_udt_fields, write_ucd_mesh
      private :: read_ucd_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_file(my_rank, istep, ucd)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_ucd,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD file: ', trim(file_name)
!
      open(id_ucd_file,file=file_name, form='formatted')
      call write_ucd_mesh(ucd)
      call write_udt_fields(ucd)
      close(id_ucd_file)
!
      end subroutine write_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_udt_file(my_rank, istep, ucd)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD field: ', trim(file_name)
!
      open(id_ucd_file,file=file_name, form='formatted')
      call write_udt_fields(ucd)
      close(id_ucd_file)
!
      end subroutine write_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_grd_file(my_rank, ucd)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd%file_prefix, iflag_udt,       &
     &    my_rank, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD mesh: ', trim(file_name)
!
      open (id_ucd_file, file=file_name, status='replace')
      call write_ucd_mesh(ucd)
      close(id_ucd_file)
!
      end subroutine write_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_udt_fields(ucd)
!
      use udt_data_IO
!
      type(ucd_data), intent(in) :: ucd
!
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
        call write_udt_field_header(id_ucd_file, ucd%num_field,         &
     &      ucd%num_comp, ucd%phys_name)
!
!    (C) fields
!     *  global node ID, results
!   ===================================================
        call write_ucd_field_data(id_ucd_file, ucd%nnod,                &
     &      ucd%ntot_comp, ucd%nnod, ucd%inod_global, ucd%d_ucd)
      end if
!
      end subroutine write_udt_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_udt_file(my_rank, istep, ucd)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      open (id_ucd_file, file=file_name, status='old')
!
      call read_udt_field_header(id_ucd_file, ucd%num_field,            &
     &    ucd%num_comp, ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
!
      call read_ucd_field_data(id_ucd_file, ucd%nnod,                   &
     &    ucd%ntot_comp, ucd%d_ucd)
!
      close(id_ucd_file)
!
      end subroutine read_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_and_alloc_udt_params(my_rank, istep, ucd)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      open (id_ucd_file, file=file_name, status='old')
      read(id_ucd_file,'(i10)') ucd%num_field
      backspace(id_ucd_file)
!
      call allocate_ucd_phys_name(ucd)
!
      call read_udt_field_header(id_ucd_file, ucd%num_field,            &
     &    ucd%num_comp, ucd%phys_name)
      close(id_ucd_file)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine read_and_alloc_udt_params
!
! -----------------------------------------------------------------------
!
      subroutine read_and_alloc_udt_file(my_rank, istep, ucd)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      open (id_ucd_file, file=file_name, status='old')
      read(id_ucd_file,'(i10)') ucd%num_field
      backspace(id_ucd_file)
!
      call allocate_ucd_phys_name(ucd)
!
      call read_udt_field_header(id_ucd_file, ucd%num_field,            &
     &    ucd%num_comp, ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_ucd_field_data(id_ucd_file, ucd%nnod,                   &
     &    ucd%ntot_comp, ucd%d_ucd)
      close(id_ucd_file)
!
      end subroutine read_and_alloc_udt_file
!
! -----------------------------------------------------------------------
!
      subroutine read_and_alloc_ucd_file(my_rank, istep, nnod_ele, ucd)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep, nnod_ele
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_ucd,       &
     &    my_rank, istep, file_name)
!
      open (id_ucd_file, file=file_name, status='old')
!
      call read_ucd_mesh(nnod_ele, ucd)
!
      read(id_ucd_file,'(i10)') ucd%num_field
      backspace(id_ucd_file)
!
      call allocate_ucd_phys_name(ucd)
!
      call read_udt_field_header(id_ucd_file, ucd%num_field,            &
     &    ucd%num_comp, ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_ucd_field_data(id_ucd_file, ucd%nnod,                   &
     &    ucd%ntot_comp, ucd%d_ucd)
      close(id_ucd_file)
!
      end subroutine read_and_alloc_ucd_file
!
! -----------------------------------------------------------------------
!
      subroutine read_grd_file(my_rank, nnod_ele, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, nnod_ele
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd%file_prefix, iflag_udt,       &
     &    my_rank, file_name)
!
      write(*,*) 'PSF grid data: ', trim(file_name)
      open(id_ucd_file, file=file_name, form='formatted',               &
     &     status='old')
!
      call read_ucd_mesh(nnod_ele, ucd)
      close(id_ucd_file)
!
      end subroutine read_grd_file
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_ucd_mesh(ucd)
!
      use udt_data_IO
!
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
      call write_udt_mesh_header(id_ucd_file, ucd%nnod,                 &
     &    ucd%nele, ucd%ntot_comp)
!
!   =====================
!   [2] position of node
!   =====================
!     global node ID, position
!   ==================================
      call write_ucd_field_data(id_ucd_file, ucd%nnod, ithree,          &
     &    ucd%nnod, ucd%inod_global, ucd%xx)
!
!   =====================
!   [3] element data
!   =====================
!     * global element ID, node connection (by global ID)
!   ===========================================================
      call write_ucd_mesh_connect(id_ucd_file, ucd%nele,                &
     &    ucd%nnod_4_ele, ucd%nele, ucd%iele_global, ucd%ie)
!
      end subroutine write_ucd_mesh
!
! -----------------------------------------------------------------------
!
      subroutine read_ucd_mesh(nnod_ele, ucd)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: nnod_ele
      type(ucd_data), intent(inout) :: ucd
!
!
      call read_ucd_mesh_header(id_ucd_file, ucd%nnod, ucd%nele,        &
     &    ucd%ntot_comp)
      ucd%nnod_4_ele = nnod_ele
!
      call allocate_ucd_node(ucd)
      call allocate_ucd_ele(ucd)
!
      call read_ucd_mesh_data(id_ucd_file, ucd%nnod, ucd%nele,          &
     &    ucd%nnod_4_ele, ucd%inod_global, ucd%iele_global,             &
     &    ucd%xx, ucd%ie)
!
      end subroutine read_ucd_mesh
!
!-----------------------------------------------------------------------
!
      end module udt_file_IO
