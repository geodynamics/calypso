!
!      module udt_file_IO
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine write_ucd_file(my_rank, istep)
!      subroutine write_udt_file(my_rank, istep)
!      subroutine write_grd_file(my_rank)
!
!      subroutine read_udt_file(my_rank, istep)
!      subroutine read_and_alloc_udt_params(my_rank, istep)
!      subroutine read_and_alloc_udt_file(my_rank, istep)
!
      module udt_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_field_file_format
      use m_ucd_data
      use set_ucd_file_names
!
      implicit none
!
      private :: write_udt_fields, write_ucd_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_ucd_file(my_rank, istep)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_ucd,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD file: ', trim(file_name)
!
      open(ucd_file_code,file=file_name, form='formatted')
      call write_ucd_mesh
      call write_udt_fields
      close(ucd_file_code)
!
      end subroutine write_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_udt_file(my_rank, istep)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD field: ', trim(file_name)
!
      open(ucd_file_code,file=file_name, form='formatted')
      call write_udt_fields
      close(ucd_file_code)
!
      end subroutine write_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_grd_file(my_rank)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd_header_name, iflag_udt,       &
     &    my_rank, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii UCD mesh: ', trim(file_name)
!
      open (ucd_file_code, file=file_name, status='replace')
      call write_ucd_mesh
      close(ucd_file_code)
!
      end subroutine write_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_udt_fields
!
      use udt_data_IO
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
      if(num_field_ucd .gt. 0) then
        call write_udt_field_header(ucd_file_code, num_field_ucd,       &
     &      num_comp_ucd, phys_name_ucd)
!
!    (C) fields
!     *  global node ID, results
!   ===================================================
        call write_single_udt_data(ucd_file_code, nnod_ucd,             &
     &      ntot_comp_ucd, inod_gl_ucd, d_nod_ucd)
      end if
!
      end subroutine write_udt_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_udt_file(my_rank, istep)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      open (ucd_file_code, file=file_name, status='old')
!
      call read_udt_field_header(ucd_file_code, num_field_ucd,          &
     &    num_comp_ucd, phys_name_ucd)
!
      call cal_istack_ucd_component
!
      call read_single_udt_data(ucd_file_code, nnod_ucd,                &
     &    ntot_comp_ucd, d_nod_ucd)
!
      close(ucd_file_code)
!
      end subroutine read_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_and_alloc_udt_params(my_rank, istep)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      open (ucd_file_code, file=file_name, status='old')
      read(ucd_file_code,*) num_field_ucd
      close(ucd_file_code)
!
      call allocate_ucd_phys_name
!
!
      open (ucd_file_code, file=file_name, status='old')
      call read_udt_field_header(ucd_file_code, num_field_ucd,          &
     &    num_comp_ucd, phys_name_ucd)
      close(ucd_file_code)
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      end subroutine read_and_alloc_udt_params
!
! -----------------------------------------------------------------------
!
      subroutine read_and_alloc_udt_file(my_rank, istep)
!
      use udt_data_IO
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt,       &
     &    my_rank, istep, file_name)
!
      open (ucd_file_code, file=file_name, status='old')
      read(ucd_file_code,*) num_field_ucd
      close(ucd_file_code)
!
      call allocate_ucd_phys_name
!
!
      open (ucd_file_code, file=file_name, status='old')
      call read_udt_field_header(ucd_file_code, num_field_ucd,          &
     &    num_comp_ucd, phys_name_ucd)
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      call read_single_udt_data(ucd_file_code, nnod_ucd,                &
     &    ntot_comp_ucd, d_nod_ucd)
      close(ucd_file_code)
!
      end subroutine read_and_alloc_udt_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_ucd_mesh
!
      use udt_data_IO
!
!
!   =====================
!   [1] node data
!   =====================
!     * number of data
!     * num. of node, elements, nodal field components, 
!       elemental fields components, and num of model
!   ====================================================
      call write_udt_mesh_header(ucd_file_code, nnod_ucd,               &
     &    nele_ucd, ntot_comp_ucd)
!
!   =====================
!   [2] position of node
!   =====================
!     global node ID, position
!   ==================================
      call write_single_udt_data(ucd_file_code, nnod_ucd, ithree,       &
     &    inod_gl_ucd, xx_ucd)
!
!   =====================
!   [3] element data
!   =====================
!     * global element ID, node connection (by global ID)
!   ===========================================================
      call write_single_grd_connect(ucd_file_code, nnod_4_ele_ucd,      &
     &   nele_ucd, iele_gl_ucd, ie_ucd)
!
      end subroutine write_ucd_mesh
!
! -----------------------------------------------------------------------
!
      end module udt_file_IO
