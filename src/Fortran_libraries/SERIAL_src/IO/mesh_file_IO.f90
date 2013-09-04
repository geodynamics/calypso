!mesh_file_IO.f90
!      module mesh_file_IO
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine set_mesh_fname(my_rank)
!
!      subroutine read_mesh_file(my_rank)
!      subroutine read_mesh_geometry(my_rank)
!
!      subroutine read_node_size(my_rank)
!      subroutine read_geometry_size(my_rank)
!
!      subroutine write_mesh_file(my_rank)
!
      module mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use m_read_mesh_data
      use set_parallel_file_name
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_fname(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      if(iflag_mesh_file_ext.gt.0) then
        call add_int_suffix(my_rank, mesh_file_head, fname_tmp)
        call add_gfm_extension(fname_tmp, mesh_file_name)
      else
        call add_int_suffix(my_rank, mesh_file_head, mesh_file_name)
      end if
!
      end subroutine set_mesh_fname
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_mesh_file(my_rank)
!
      use m_machine_parameter
      use mesh_data_IO
      use boundary_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call read_geometry_data
      call read_boundary_data_a(input_file_code)
      close(input_file_code)
!
      end subroutine read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry(my_rank)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_geometry_data
      close(input_file_code)
!
!
      end subroutine read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine read_node_size(my_rank)
!
      use domain_data_IO
      use node_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_domain_info(input_file_code)
      call read_number_of_node(input_file_code)
      close(input_file_code)
!
!
      end subroutine read_node_size
!
!------------------------------------------------------------------
!
      subroutine read_geometry_size(my_rank)
!
      use domain_data_IO
      use node_geometry_IO
      use element_connect_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call read_domain_info(input_file_code)
      call read_number_of_node(input_file_code)
      call read_geometry_info(input_file_code)
!
!  ----  read element data -------
!
      call read_number_of_element(input_file_code)
      close(input_file_code)
!
      end subroutine read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file(my_rank)
!
      use m_machine_parameter
      use mesh_data_IO
      use boundary_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!       if (iflag_debug.gt.0) write(*,*) 'write_geometry_data'
      call write_geometry_data
!       if (iflag_debug.gt.0) write(*,*) 'write_boundary_data_a'
      call write_boundary_data_a(input_file_code)
      close(input_file_code)
!
      end subroutine write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO
