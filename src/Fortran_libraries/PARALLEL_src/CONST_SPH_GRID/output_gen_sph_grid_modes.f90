!>@file   output_gen_sph_grid_modes.f90
!!@brief  module output_gen_sph_grid_modes
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine load_local_rj_mesh_4_merge                           &
!!     &         (sph_file_param, num_pe, sph_mesh)
!!        integer, intent(in) ::  num_pe
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!!      subroutine para_output_sph_mode_grids                           &
!!     &         (sph_file_param, num_pe, sph_mesh)
!!      subroutine para_output_sph_rj_modes                             &
!!     &         (sph_file_param, num_pe, sph_mesh)
!!        integer(kind = kint), intent(in) :: num_pe
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!!@endverbatim
!
      module output_gen_sph_grid_modes
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_SPH_mesh_field_data
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_file_IO_parameter
      use t_spheric_data_IO
!
      implicit none
!
      type(sph_file_data_type), save, private :: sph_file_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_local_rj_mesh_4_merge                             &
     &         (sph_file_param, num_pe, sph_mesh)
!
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use load_data_for_sph_IO
      use count_num_sph_smp
!
      integer, intent(in) ::  num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      type(field_IO_params) :: file_param
      integer :: id_rank, ip
      integer(kind = kint) :: iloop, ierr
!
!  Read index data
      ierr = 0
      call set_sph_mesh_file_fmt_prefix                                 &
     &   (sph_file_param%iflag_format, sph_file_param%file_prefix,      &
     &    file_param)
      do iloop = 0, (num_pe-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_spectr_rj_file                                &
     &     (num_pe, id_rank, file_param, sph_file_m)
!
        if(id_rank .lt. num_pe) then
          write(*,*) 'load original data for  ', id_rank,               &
     &             ' on ', my_rank, 'at loop ', iloop
          call copy_sph_trans_rj_from_IO(sph_file_m,                    &
     &       sph_mesh(ip)%sph%sph_rj, sph_mesh(ip)%sph_comms%comm_rj,   &
     &       sph_mesh(ip)%sph_grps, sph_mesh(ip)%sph%sph_params)
          call count_num_rj_smp(sph_mesh(ip)%sph%sph_rj, ierr)
          call dealloc_rj_mode_IO(sph_file_m)
        end if
      end do
!
      end subroutine load_local_rj_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine para_output_sph_mode_grids                             &
     &         (sph_file_param, num_pe, sph_mesh)
!
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
      use parallel_load_data_4_sph
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      integer :: ip, id_rank
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rj_to_IO(sph_mesh(ip)%sph%sph_params,       &
     &      sph_mesh(ip)%sph%sph_rj, sph_mesh(ip)%sph_comms%comm_rj,    &
     &      sph_mesh(ip)%sph_grps, sph_file_m)
        call sel_mpi_write_spectr_rj_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rj_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical modes for domain',               &
     &          id_rank, ' is done.'
!
        call copy_sph_trans_rlm_to_IO(sph_mesh(ip)%sph%sph_params,      &
     &      sph_mesh(ip)%sph%sph_rlm, sph_mesh(ip)%sph_comms%comm_rlm,  &
     &      sph_file_m)
        call sel_mpi_write_modes_rlm_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rlm_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &                      id_rank, ' is done.'
!
        call copy_sph_trans_rtm_to_IO(sph_mesh(ip)%sph%sph_params,      &
     &      sph_mesh(ip)%sph%sph_rtm, sph_mesh(ip)%sph_comms%comm_rtm,  &
     &      sph_file_m)
        call sel_mpi_write_geom_rtm_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rtm_grid_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &                      id_rank, ' is done.'
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'copy_sph_trans_rtp_to_IO', id_rank
        call copy_sph_trans_rtp_to_IO(sph_mesh(ip)%sph%sph_params,      &
     &      sph_mesh(ip)%sph%sph_rtp, sph_mesh(ip)%sph_comms%comm_rtp,  &
     &      sph_mesh(ip)%sph_grps, sph_file_m)
        call sel_mpi_write_geom_rtp_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rtp_grid_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical grids for domain',               &
     &          id_rank, ' is done.'
!
        call dealloc_sph_modes                                          &
     &     (sph_mesh(ip)%sph, sph_mesh(ip)%sph_comms,                   &
     &      sph_mesh(ip)%sph_grps)
      end do
!
      end subroutine para_output_sph_mode_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_output_sph_rj_modes                               &
     &         (sph_file_param, num_pe, sph_mesh)
!
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      integer :: ip, id_rank
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rj_to_IO(sph_mesh(ip)%sph%sph_params,       &
     &      sph_mesh(ip)%sph%sph_rj, sph_mesh(ip)%sph_comms%comm_rj,    &
     &      sph_mesh(ip)%sph_grps, sph_file_m)
        call sel_mpi_write_spectr_rj_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rj_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical modes for domain',               &
     &          id_rank, ' is done.'
      end do
!
      end subroutine para_output_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      end module output_gen_sph_grid_modes
