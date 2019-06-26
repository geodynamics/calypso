!> @file  mesh_file_IO.f90
!!      module mesh_file_IO
!!
!! @author  H. Matsui
!! @date Programmed in Apr., 2006
!
!> @brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine read_mesh_file                                       &
!!     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!
!!      subroutine read_mesh_geometry                                   &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!      subroutine read_node_size(id_rank, file_name, mesh_IO, ierr)
!!      subroutine read_geometry_size                                   &
!!     &         (id_rank, file_name, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine write_mesh_file                                      &
!!     &         (id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!
!!      subroutine write_node_position_sph                              &
!!     &         (id_rank, file_prefix, mesh_IO)
!!      subroutine write_node_position_cyl                              &
!!     &         (id_rank, file_prefix, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!@endverbatim
!
      module mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
!
      implicit none
!
!   mesh file code
      integer(kind = kint), parameter ::  input_file_code = 14
!
      private :: input_file_code
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_file                                         &
     &         (id_rank, file_name, mesh_IO, group_IO, ierr)
!
      use mesh_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_groups), intent(inout) ::   group_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
!
      call read_geometry_data(input_file_code, id_rank, mesh_IO, ierr)
      call read_mesh_groups(input_file_code, group_IO)
      close(input_file_code)
!
      end subroutine read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry                                     &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use mesh_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call read_geometry_data(input_file_code, id_rank,                 &
     &   mesh_IO, ierr)
      close(input_file_code)
!
!
      end subroutine read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine read_node_size(id_rank, file_name, mesh_IO, ierr)
!
      use mesh_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call read_num_node(input_file_code, id_rank, mesh_IO, ierr)
      close(input_file_code)
!
!
      end subroutine read_node_size
!
!------------------------------------------------------------------
!
      subroutine read_geometry_size                                     &
     &         (id_rank, file_name, mesh_IO, ierr)
!
      use mesh_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call read_num_node_ele                                            &
     &   (input_file_code, id_rank, mesh_IO, ierr)
      close(input_file_code)
!
      end subroutine read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file                                        &
     &         (id_rank, file_name, mesh_IO, group_IO)
!
      use mesh_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
!
      call write_geometry_data(input_file_code, id_rank, mesh_IO)
      call write_mesh_groups(input_file_code, group_IO)
!
      close(input_file_code)
!
      end subroutine write_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_node_position_sph                                &
     &         (id_rank, file_prefix, mesh_IO)
!
      use mesh_data_IO
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_prefix
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = add_process_id(id_rank, file_prefix)
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call output_node_sph_geometry                                     &
         (input_file_code, id_rank, mesh_IO)
      close(input_file_code)
!
      call dealloc_node_geometry_IO(mesh_IO)
!
      end subroutine write_node_position_sph
!
!  ---------------------------------------------------------------------
!
      subroutine write_node_position_cyl                                &
     &         (id_rank, file_prefix, mesh_IO)
!
      use mesh_data_IO
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_prefix
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = add_process_id(id_rank, file_prefix)
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call output_node_cyl_geometry                                     &
         (input_file_code, id_rank, mesh_IO)
      close(input_file_code)
!
      call dealloc_node_geometry_IO(mesh_IO)
!
      end subroutine write_node_position_cyl
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO
