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
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_gz_udt_file(id_rank, gzip_name, ucd)
!!      subroutine read_gz_ucd_file(id_rank, gzip_name, ucd)
!!      subroutine read_gz_ucd_grd(id_rank, gzip_name, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine read_alloc_gz_udt_params(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_udt_file(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_ucd_file(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_ucd_grd(id_rank, gzip_name, ucd)
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
!
      use t_ucd_data
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_ucd
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.le.0) write(*,*)                     &
     &      'Write gzipped ucd file: ', trim(gzip_name)
      call open_wt_gzfile_a(gzip_name, zbuf_ucd)
!
      call write_gz_ucd_type_mesh(ucd, zbuf_ucd)
      call write_gz_udt_type_fields(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine write_gz_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_udt_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.le.0) write(*,*)                     &
     &      'Write gzipped ucd file: ', trim(gzip_name)
      call open_wt_gzfile_a(gzip_name, zbuf_ucd)
!
      call write_gz_udt_type_fields(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine write_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_grd_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.le.0) write(*,*)                     &
     &      'Write gzipped ucd grid file: ', trim(gzip_name)
      call open_wt_gzfile_a(gzip_name, zbuf_ucd)
!
      call write_gz_ucd_type_mesh(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine write_gz_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_udt_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_gz_udt_field(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_ucd_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped UCD data file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_gz_ucd_mesh_data(ucd, zbuf_ucd)
      call read_gz_udt_field(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_gz_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_ucd_grd(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped UCD grid file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_gz_ucd_mesh_data(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_gz_ucd_grd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_udt_params(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_alloc_gz_udt_field(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_alloc_gz_udt_params
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_udt_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
      use gz_ucd_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_alloc_gz_udt_field(ucd, zbuf_ucd)
      call read_gz_udt_field_data                                       &
     &   (ucd%nnod, ucd%ntot_comp, ucd%d_ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_alloc_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_ucd_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
      use gz_ucd_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped UCD data file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_alloc_gz_ucd_mesh_data(ucd, zbuf_ucd)
      call read_alloc_gz_udt_field(ucd, zbuf_ucd)
      call read_gz_udt_field_data                                       &
     &   (ucd%nnod, ucd%ntot_comp, ucd%d_ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_alloc_gz_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_ucd_grd(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
      use gz_udt_type_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
!
      if(i_debug.gt.0 .or. id_rank.eq.0) write(*,*)                     &
     &     'Read gzipped UCD grid file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_ucd)
      call read_alloc_gz_ucd_mesh_data(ucd, zbuf_ucd)
      call close_gzfile_a(zbuf_ucd)
!
      end subroutine read_alloc_gz_ucd_grd
!
!-----------------------------------------------------------------------
!
      end module gz_udt_file_IO
