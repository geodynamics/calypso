!>@file   tracer_restart_file_IO.f90
!!@brief  module tracer_restart_file_IO
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine output_tracer_restart(tracer_file_prm, istep_rst,    &
!!     &                                 time_d, rst_step, fln_tce)
!!        integer(kind = kint), intent(in) :: istep_rst
!!        type(field_IO_params), intent(in) :: tracer_file_prm
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(each_fieldline_trace), intent(in) :: fln_tce
!!      subroutine input_tracer_restart(tracer_file_prm, init_d,        &
!!     &                                rst_step, fln_tce)
!!        type(field_IO_params), intent(in) :: tracer_file_prm
!!        type(time_data), intent(inout) :: init_d
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module tracer_restart_file_IO
!
      use m_precision
      use t_time_data
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_read_mesh_data
      use t_field_data_IO
      use t_tracing_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_tracer_restart(tracer_file_prm, istep_rst,      &
     &                                 time_d, fln_tce)
!
      use set_sph_restart_IO
      use particle_MPI_IO_select
      use local_fline_restart_IO
      use const_global_element_ids
!
      integer(kind = kint), intent(in) :: istep_rst
      type(field_IO_params), intent(in) :: tracer_file_prm
      type(time_data), intent(in) :: time_d
      type(each_fieldline_trace), intent(in) :: fln_tce
!
      type(surf_edge_IO_file) :: particle_IO
      type(time_data) :: time_IO
!
!
!      call check_tracer_restart(fln_tce)
!
      call copy_time_step_size_data(time_d, time_IO)
      call copy_restart_tracer_to_IO(fln_tce, particle_IO)

      call sel_mpi_write_particle_file(tracer_file_prm, istep_rst,      &
     &                                 time_IO, particle_IO)
      call calypso_mpi_barrier()
      call dealloc_neib_id(particle_IO%comm)
      call dealloc_ele_connect(particle_IO%ele)
      call dealloc_node_geometry_base(particle_IO%node)
!
      end subroutine output_tracer_restart
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine input_tracer_restart(tracer_file_prm, istep_rst,       &
     &                                init_d, fln_tce)
!
      use set_sph_restart_IO
      use local_fline_restart_IO
      use particle_MPI_IO_select
!
      type(field_IO_params), intent(in) :: tracer_file_prm
!
      integer(kind = kint), intent(in) :: istep_rst
      type(time_data), intent(in) :: init_d
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      type(surf_edge_IO_file) :: particle_IO
      type(time_data) :: time_IO
!
!
      call sel_mpi_read_particle_file(tracer_file_prm, istep_rst,       &
     &                                time_IO, particle_IO)
      call calypso_mpi_barrier()
      call copy_restart_tracer_from_IO(particle_IO, fln_tce)
      call dealloc_neib_id(particle_IO%comm)
      call dealloc_ele_connect(particle_IO%ele)
      call dealloc_node_geometry_base(particle_IO%node)
!
!      call check_tracer_restart(fln_tce)
!
      if(my_rank .ne. 0) return
      if(init_d%i_time_step .ne. time_IO%i_time_step) then
        write(*,*) 'Time step in particle restart does not match ',     &
     &             'with field restaart data. But ignore.'
      end if
      if(init_d%time .ne. time_IO%time) then
        write(*,*) 'Time in particle restart does not match ',          &
     &             'with field restaart data. But ignore.'
      end if
      if(init_d%dt .ne. time_IO%dt) then
        write(*,*) 'Delta t in particle restart does not match ',       &
     &             'with field restaart data. But ignore.'
      end if
!
      end subroutine input_tracer_restart
!
! -----------------------------------------------------------------------
!
      end module tracer_restart_file_IO
