!>@file  gz_udt_file_IO.f90
!!       module gz_udt_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief gzipped UCD format data IO
!!
!!@verbatim
!!      subroutine write_gz_ucd_file(id_rank, gzip_name, ucd)
!!      subroutine write_gz_udt_file(id_rank, gzip_name, ucd)
!!      subroutine write_gz_grd_file(id_rank, gzip_name, ucd)
!!
!!      subroutine read_gz_udt_file(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_udt_head(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_udt_file(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_ucd_file                               &
!!     &         (id_rank, gzip_name, nnod_ele, ucd)
!!      subroutine read_gz_ucd_grd(id_rank, gzip_name, nnod_ele, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank  process ID
!!@param gzip_name    file name
!!@param ucd      Structure for FEM field data IO
!
      module gz_udt_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_field_file_format
!
      use t_ucd_data
!
      use set_ucd_file_names
      use skip_gz_comment
      use gz_ucd_data_IO
!
      implicit none
!
      private :: write_gz_udt_type_fields, write_gz_ucd_type_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_file(id_rank, gzip_name, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.le.0) write(*,*)                     &
     &      'Write gzipped ucd file: ', trim(gzip_name)
      call open_wt_gzfile_f(gzip_name)
!
      call write_gz_ucd_type_mesh(ucd)
      call write_gz_udt_type_fields(ucd)
      call close_gzfile_f
!
      end subroutine write_gz_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_udt_file(id_rank, gzip_name, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.le.0) write(*,*)                     &
     &      'Write gzipped ucd file: ', trim(gzip_name)
      call open_wt_gzfile_f(gzip_name)
!
      call write_gz_udt_type_fields(ucd)
      call close_gzfile_f
!
      end subroutine write_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_grd_file(id_rank, gzip_name, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.le.0) write(*,*)                     &
     &      'Write gzipped ucd grid file: ', trim(gzip_name)
      call open_wt_gzfile_f(gzip_name)
!
      call write_gz_ucd_type_mesh(ucd)
      call close_gzfile_f
!
      end subroutine write_gz_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_udt_file(id_rank, gzip_name, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_udt_field_header(ucd%num_field, ucd%num_comp,        &
     &    ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
!
      call read_gz_udt_field_data(ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      call close_gzfile_f
!
      end subroutine read_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_udt_head(id_rank, gzip_name, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_udt_field_num(ucd%num_field)
      call allocate_ucd_phys_name(ucd)
!
      call read_gz_udt_field_name(ucd%num_field, ucd%num_comp,          &
     &    ucd%phys_name)
!
      call close_gzfile_f
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine read_alloc_gz_udt_head
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_udt_file(id_rank, gzip_name, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_udt_field_num(ucd%num_field)
      call allocate_ucd_phys_name(ucd)
!
      call read_gz_udt_field_name(ucd%num_field, ucd%num_comp,          &
     &    ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_gz_udt_field_data(ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      call close_gzfile_f
!
      end subroutine read_alloc_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_ucd_file                                 &
     &         (id_rank, gzip_name, nnod_ele, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod_ele
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped UCD data file: ', trim(gzip_name)
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_udt_mesh_header(ucd%nnod, ucd%nele, ucd%ntot_comp)
      ucd%nnod_4_ele = nnod_ele
!
      call allocate_ucd_node(ucd)
      call allocate_ucd_ele(ucd)
!
      call read_gz_ucd_mesh_data(ucd%nnod, ucd%nele, ucd%nnod_4_ele,    &
     &    ucd%inod_global, ucd%iele_global, ucd%xx, ucd%ie)
!
      call read_gz_udt_field_num(ucd%num_field)
      call allocate_ucd_phys_name(ucd)
!
      call read_gz_udt_field_name(ucd%num_field, ucd%num_comp,          &
     &    ucd%phys_name)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_gz_udt_field_data(ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      call close_gzfile_f
!
      end subroutine read_alloc_gz_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_ucd_grd(id_rank, gzip_name, nnod_ele, ucd)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod_ele
      type(ucd_data), intent(inout) :: ucd
!
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped UCD grid file: ', trim(gzip_name)
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_udt_mesh_header(ucd%nnod, ucd%nele, ucd%ntot_comp)
      ucd%nnod_4_ele = nnod_ele
!
      call allocate_ucd_node(ucd)
      call allocate_ucd_ele(ucd)
!
      call read_gz_ucd_mesh_data(ucd%nnod, ucd%nele, ucd%nnod_4_ele,    &
     &    ucd%inod_global, ucd%iele_global, ucd%xx, ucd%ie)
      call close_gzfile_f
!
      end subroutine read_gz_ucd_grd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_udt_type_fields(ucd)
!
      type(ucd_data), intent(in) :: ucd
!
!
      if(ucd%num_field .gt. 0) then
        call write_gz_udt_field_header(ucd%num_field,                   &
     &      ucd%num_comp, ucd%phys_name)
        call write_gz_ucd_field_data(ucd%nnod, ucd%ntot_comp,           &
     &      ucd%nnod, ucd%inod_global, ucd%d_ucd)
      end if
!
      end subroutine write_gz_udt_type_fields
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_type_mesh(ucd)
!
      type(ucd_data), intent(in) :: ucd
!
!
      call write_gz_udt_mesh_header(ucd%nnod, ucd%nele, ucd%ntot_comp)
!
      call write_gz_ucd_field_data(ucd%nnod, ithree, ucd%nnod,          &
     &    ucd%inod_global, ucd%xx)
      call write_gz_ucd_mesh_connect(ucd%nele, ucd%nnod_4_ele,          &
     &    ucd%nele, ucd%iele_global, ucd%ie)
!
      end subroutine write_gz_ucd_type_mesh
!
! -----------------------------------------------------------------------
!
      end module gz_udt_file_IO
