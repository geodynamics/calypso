!>@file  gz_udt_type_data_IO.f90
!!       module gz_udt_type_data_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief gzipped UCD format data IO
!!
!!@verbatim
!!      subroutine write_gz_udt_type_fields(FPz_f, ucd, zbuf)
!!      subroutine write_gz_ucd_type_mesh(FPz_f, ucd, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(ucd_data), intent(in) :: ucd
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_gz_udt_field(FPz_f, ucd, zbuf)
!!      subroutine read_alloc_gz_udt_field(FPz_f, ucd, zbuf)
!!      subroutine read_gz_ucd_mesh_data(FPz_f, ucd, zbuf)
!!      subroutine read_alloc_gz_ucd_mesh_data(FPz_f, ucd, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(ucd_data), intent(inout) :: ucd
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!!
!
      module gz_udt_type_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ucd_data
!
      use gz_ucd_data_IO
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
      subroutine write_gz_udt_type_fields(FPz_f, ucd, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(ucd_data), intent(in) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(ucd%num_field .gt. 0) then
        call write_gz_udt_field_header(FPz_f, ucd%num_field,            &
     &      ucd%num_comp, ucd%phys_name, zbuf)
        call write_gz_ucd_field_data(FPz_f, ucd%nnod, ucd%ntot_comp,    &
     &      ucd%nnod, ucd%inod_global, ucd%d_ucd, zbuf)
      end if
!
      end subroutine write_gz_udt_type_fields
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_type_mesh(FPz_f, ucd, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(ucd_data), intent(in) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call write_gz_udt_mesh_header                                     &
     &   (FPz_f, ucd%nnod, ucd%nele, ucd%ntot_comp, zbuf)
!
      call write_gz_ucd_field_data(FPz_f, ucd%nnod, ithree, ucd%nnod,   &
     &    ucd%inod_global, ucd%xx, zbuf)
      call write_gz_ucd_mesh_connect(FPz_f, ucd%nele, ucd%nnod_4_ele,   &
     &    ucd%nele, ucd%iele_global, ucd%ie, zbuf)
!
      end subroutine write_gz_ucd_type_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_udt_field(FPz_f, ucd, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: num
!
!
      call read_gz_udt_field_num(FPz_f, num, zbuf)
      if(num .ne. ucd%num_field) write(*,*) 'Error in number of field'
!
      call read_gz_udt_field_name                                       &
     &   (FPz_f, ucd%num_field, ucd%num_comp, ucd%phys_name, zbuf)
      call cal_istack_ucd_component(ucd)
!
      call read_gz_udt_field_data                                       &
     &   (FPz_f, ucd%nnod, ucd%ntot_comp, ucd%d_ucd, zbuf)
!
      end subroutine read_gz_udt_field
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_udt_field(FPz_f, ucd, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call read_gz_udt_field_num(FPz_f, ucd%num_field, zbuf)
      call allocate_ucd_phys_name(ucd)
!
      call read_gz_udt_field_name                                       &
     &   (FPz_f, ucd%num_field, ucd%num_comp, ucd%phys_name, zbuf)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine read_alloc_gz_udt_field
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_gz_ucd_mesh_data(FPz_f, ucd, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind=kint_gl) :: nnod, nele
!
!
      call read_gz_udt_mesh_header                                      &
     &   (FPz_f, nnod, nele, ucd%ntot_comp, zbuf)
!
      if(nnod .ne. ucd%nnod) write(*,*) 'Error in number of node'
      if(nele .ne. ucd%nele) write(*,*) 'Error in number of element'
!
      call read_gz_ucd_node_data                                        &
     &   (FPz_f, ucd%nnod, ucd%inod_global, ucd%xx, zbuf)
      call read_gz_ucd_ele_connect(FPz_f, ucd%nele, ucd%nnod_4_ele,     &
     &    ucd%iele_global, ucd%ie, zbuf)
!
      end subroutine  read_gz_ucd_mesh_data
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_gz_ucd_mesh_data(FPz_f, ucd, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(ucd_data), intent(inout) :: ucd
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: itmp
      character(len=kchara) :: eleflag
!
!
      call read_gz_udt_mesh_header                                      &
     &   (FPz_f, ucd%nnod, ucd%nele, ucd%ntot_comp, zbuf)
!
      call allocate_ucd_node(ucd)
      call read_gz_ucd_node_data                                        &
     &   (FPz_f, ucd%nnod, ucd%inod_global, ucd%xx, zbuf)
!
      if(ucd%nele .le. 0) then
        ucd%nnod_4_ele = 0
        call allocate_ucd_ele(ucd)
      else
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) itmp, itmp, eleflag
        ucd%nnod_4_ele = nnod_ele_by_ucd_eletype(eleflag)
!
        call allocate_ucd_ele(ucd)
        read(zbuf%fixbuf(1),*) ucd%iele_global(1), itmp,                &
     &                        eleflag, ucd%ie(1,1:ucd%nnod_4_ele)
      end if
!
      do iele = 2, ucd%nele
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) ucd%iele_global(iele), itmp,             &
     &                        eleflag, ucd%ie(iele,1:ucd%nnod_4_ele)
      end do
!
      end subroutine read_alloc_gz_ucd_mesh_data
!
! -----------------------------------------------------------------------
!
      end module gz_udt_type_data_IO
