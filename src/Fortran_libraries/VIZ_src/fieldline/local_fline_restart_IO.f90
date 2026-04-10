!>@file   local_fline_restart_IO.f90
!!@brief  module local_fline_restart_IO
!!
!!@author H.Matsui
!!@date      Programmed in June, 2024
!
!>@brief  tracer or field line data in each domain
!!
!!@verbatim
!!      character(len=ilen_hd_particle_connect)                         &
!!     &                         function hd_particle_connect()
!!      character(len=ilen_hd_particle_velocity)                        &
!!     &                         function hd_particle_velocity()
!!      character(len=ilen_hd_particle_marker)                          &
!!     &                         function hd_particle_marker()
!!
!!      subroutine copy_restart_tracer_to_IO(fln_tce, particle_IO)
!!        type(each_fieldline_trace), intent(in) :: fln_tce
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!      subroutine copy_restart_tracer_from_IO(particle_IO, fln_tce)
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine check_tracer_restart(fln_tce)
!!        type(each_fieldline_trace), intent(in) :: fln_tce
!!@endverbatim
!
      module local_fline_restart_IO
!
      use m_precision
      use m_constants
      use t_read_mesh_data
      use calypso_mpi
!
      implicit  none
!
!>      length of hd_particle_connect
      integer(kind = kint), parameter                                   &
     &                   :: ilen_hd_particle_connect = 1+25+25+33+1+5 
!>      length of ilen_hd_particle_marker
      integer(kind = kint), parameter                                   &
     &                   :: ilen_hd_particle_velocity = 1+25+1+3
!>      length of hd_particle_marker
      integer(kind = kint), parameter                                   &
     &                   :: ilen_hd_particle_marker = 1+22+1+3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      character(len=ilen_hd_particle_connect)                           &
     &                         function hd_particle_connect()
!
      hd_particle_connect                                               &
     &      = '!' // char(10)                                           &
     &     // '!  2    Particle indexing' // char(10)                   &
     &     // '!  2.1   local element ID' // char(10)                   &
     &     // '!     and surface ID in elememnt' // char(10)            &
     &     // '!' // char(10)
!
      end function hd_particle_connect
!
!------------------------------------------------------------------
!
      character(len=ilen_hd_particle_velocity)                          &
     &                         function hd_particle_velocity()
!
      hd_particle_velocity                                              &
     &      = '!' // char(10)                                           &
     &     // '!  3.1  particle velocity' // char(10)                   &
     &     // '!' // char(10)
!
      end function hd_particle_velocity
!
!------------------------------------------------------------------
!
      character(len=ilen_hd_particle_marker)                            &
     &                         function hd_particle_marker()
!
      hd_particle_marker                                                &
     &      = '!' // char(10)                                           &
     &     // '!  3.2   scalar marker' // char(10)                      &
     &     // '!' // char(10)
!
      end function hd_particle_marker
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_restart_tracer_to_IO(fln_tce, particle_IO)
!
      use t_tracing_data
      use set_nnod_4_ele_by_type
!
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(surf_edge_IO_file), intent(inout) :: particle_IO
!
      integer(kind = kint) :: i
!
!
      particle_IO%comm%num_neib = 0
      call alloc_neighbouring_id(particle_IO%comm)
!
      particle_IO%node%numnod =        fln_tce%num_current_fline
      particle_IO%node%internal_node = fln_tce%num_current_fline
      particle_IO%ele%numele =         fln_tce%num_current_fline
      particle_IO%ele%nnod_4_ele = 2
!
      call alloc_node_geometry_base(particle_IO%node)
      call alloc_element_types(particle_IO%ele)
      call alloc_ele_connectivity(particle_IO%ele)
!
!$omp parallel do
      do i = 1, fln_tce%num_current_fline
        particle_IO%node%inod_global(i) = fln_tce%iline_original(i)
        particle_IO%node%xx(i,1) = fln_tce%xx_fline_start(1,i)
        particle_IO%node%xx(i,2) = fln_tce%xx_fline_start(2,i)
        particle_IO%node%xx(i,3) = fln_tce%xx_fline_start(3,i)
!
        particle_IO%ele%iele_global(i) = my_rank
        particle_IO%ele%nodelm(i) = particle_IO%ele%nnod_4_ele
        particle_IO%ele%elmtyp(i)                                       &
     &       = linear_eletype_from_num(particle_IO%ele%nnod_4_ele)
!
        particle_IO%ele%ie(i,1) = fln_tce%isf_dbl_start(2,i)
        particle_IO%ele%ie(i,2) = fln_tce%isf_dbl_start(3,i)
      end do
!$omp end parallel do
!
      end subroutine copy_restart_tracer_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_restart_tracer_from_IO(particle_IO, fln_tce)
!
      use t_tracing_data
!
      type(surf_edge_IO_file), intent(in) :: particle_IO
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i
!
!
      call count_parallel_current_fline(particle_IO%node%numnod,        &
     &                                  fln_tce)
      call resize_line_start_fline(fln_tce%num_current_fline, fln_tce)
!
!$omp parallel do
      do i = 1, fln_tce%num_current_fline
        fln_tce%iline_original(i) =   particle_IO%node%inod_global(i)
        fln_tce%xx_fline_start(1,i) = particle_IO%node%xx(i,1)
        fln_tce%xx_fline_start(2,i) = particle_IO%node%xx(i,2)
        fln_tce%xx_fline_start(3,i) = particle_IO%node%xx(i,3)
        fln_tce%xx_fline_start(4,i) = one
!
        fln_tce%isf_dbl_start(1,i) = my_rank
        fln_tce%isf_dbl_start(2,i) = particle_IO%ele%ie(i,1)
        fln_tce%isf_dbl_start(3,i) = particle_IO%ele%ie(i,2)
      end do
!$omp end parallel do
!
      end subroutine copy_restart_tracer_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_tracer_restart(fln_tce)
!
      use t_tracing_data
!
      type(each_fieldline_trace), intent(in) :: fln_tce
!
      integer(kind = kint) :: ip, i
!
      do ip = 1, nprocs
        call calypso_mpi_barrier()
        if((ip-1) .ne. my_rank) cycle
!
        write(*,*) my_rank, 'fln_tce%num_current_fline',                &
     &            fln_tce%num_current_fline
        do i = 1, fln_tce%num_current_fline
          write(*,*) 'xx_fline_start', i, fln_tce%iline_original(i),    &
     &              fln_tce%xx_fline_start(1:3,i),                      &
     &              fln_tce%isf_dbl_start(1:3,i)
        end do
      end do
      call calypso_mpi_barrier()
!
      end subroutine check_tracer_restart
!
!  ---------------------------------------------------------------------
!
      end module local_fline_restart_IO
