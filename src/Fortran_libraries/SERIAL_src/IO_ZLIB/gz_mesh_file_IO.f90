!>@file   gz_mesh_file_IO.f90
!!@brief  module gz_mesh_file_IO
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_read_mesh                                         &
!!     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!
!!      subroutine gz_read_mesh_geometry                                &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine gz_read_node_size                                    &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine gz_read_geometry_size                                &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine gz_write_mesh_file                                   &
!!     &         (id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!@endverbatim
!!
      module gz_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use gz_mesh_data_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_m
      character, pointer, private, save :: FPz_msh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh                                           &
     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_groups), intent(inout) ::   group_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_msh, file_name, zbuf_m)
!
      call gz_read_geometry_data(FPz_msh, id_rank,                      &
     &                           mesh_IO, zbuf_m, ierr)
      call gz_read_mesh_groups(FPz_msh, group_IO, zbuf_m)
!
      call close_gzfile_a(FPz_msh, zbuf_m)
!
      end subroutine gz_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_geometry                                  &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_msh, file_name, zbuf_m)
      call gz_read_geometry_data(FPz_msh, id_rank,                      &
     &                           mesh_IO, zbuf_m, ierr)
      call close_gzfile_a(FPz_msh, zbuf_m)
!
      end subroutine gz_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_node_size                                      &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped mesh file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_msh, file_name, zbuf_m)
!
!      write(*,*) 'gz_read_domain_info'
      call gz_read_domain_info                                          &
     &   (FPz_msh, id_rank, mesh_IO%nod_comm, zbuf_m, ierr)
      if(ierr .gt. 0) go to 99
!      write(*,*) 'gz_read_number_of_node'
      call gz_read_number_of_node(FPz_msh, mesh_IO%node, zbuf_m)
      
  99  continue
      call close_gzfile_a(FPz_msh, zbuf_m)
!
      end subroutine gz_read_node_size
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_size                                  &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_msh, file_name, zbuf_m)
!
!      write(*,*) 'gz_read_domain_info'
      call gz_read_domain_info                                          &
     &   (FPz_msh, id_rank, mesh_IO%nod_comm, zbuf_m, ierr)
      if(ierr .gt. 0) go to 99
!
      call gz_read_geometry_info(FPz_msh, mesh_IO%node, zbuf_m)
!      write(*,*) 'gz_read_number_of_element'
      call gz_read_number_of_element(FPz_msh, mesh_IO%ele, zbuf_m)

  99  continue
      call close_gzfile_a(FPz_msh, zbuf_m)
!
      end subroutine gz_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_file                                     &
     &         (id_rank, file_name, mesh_IO, group_IO)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped mesh file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_msh, file_name, zbuf_m)
!
      call gz_write_geometry_data(FPz_msh, id_rank, mesh_IO, zbuf_m)
      call gz_write_mesh_groups(FPz_msh, group_IO, zbuf_m)
!
      call close_gzfile_a(FPz_msh, zbuf_m)
!
      end subroutine gz_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module gz_mesh_file_IO
