!>@file   output_gen_sph_grid_modes.f90
!!@brief  module output_gen_sph_grid_modes
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine load_local_rj_mesh_4_merge(sph_file_param, sph_array)
!!      subroutine para_output_sph_mode_grids(sph_file_param, sph_array)
!!      subroutine para_output_sph_rj_modes(sph_file_param, sph_array)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_mesh_array), intent(inout) :: sph_array
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
      use t_SPH_mesh_field_array
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
      subroutine load_local_rj_mesh_4_merge(sph_file_param, sph_array)
!
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use check_sph_file_access
      use load_data_for_sph_IO
      use count_num_sph_smp
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_mesh_array), intent(inout) :: sph_array
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
      do iloop = 0, (sph_array%num_pe-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_spectr_rj_file                                &
     &     (sph_array%num_pe, id_rank, file_param, sph_file_m)
!
        if(id_rank .lt. sph_array%num_pe) then
          call copy_sph_trans_rj_from_IO(sph_file_m,                    &
     &       sph_array%sph(ip)%sph_rj, sph_array%comms(ip)%comm_rj,     &
     &       sph_array%sph_grps(ip), sph_array%sph(ip)%sph_params)
          call count_num_rj_smp(sph_array%sph(ip)%sph_rj, ierr)
          call dealloc_rj_mode_IO(sph_file_m)
        end if
      end do
!
      end subroutine load_local_rj_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine para_output_sph_mode_grids(sph_file_param, sph_array)
!
      use sph_file_IO_select
      use load_data_for_sph_IO
      use parallel_load_data_4_sph
!
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_mesh_array), intent(inout) :: sph_array
!
      integer :: ip, id_rank
      integer(kind = kint) ::  ierr = 0
!
!
      do ip = 1, sph_array%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rj_to_IO(sph_array%sph(ip)%sph_params,      &
     &      sph_array%sph(ip)%sph_rj, sph_array%comms(ip)%comm_rj,      &
     &      sph_array%sph_grps(ip), sph_file_m)
        if(id_rank .lt. sph_array%num_pe) then
          call sel_write_spectr_modes_rj_file                           &
     &       (id_rank, sph_file_param, sph_file_m, ierr)
        end if
        call dealloc_rj_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical modes for domain',               &
     &          id_rank, ' is done.'
!
        call copy_sph_trans_rlm_to_IO(sph_array%sph(ip)%sph_params,     &
     &      sph_array%sph(ip)%sph_rlm, sph_array%comms(ip)%comm_rlm,    &
     &      sph_file_m)
        if(id_rank .lt. sph_array%num_pe) then
          call sel_write_modes_rlm_file                                 &
     &       (id_rank, sph_file_param, sph_file_m, ierr)
        end if
        call dealloc_rlm_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &                      id_rank, ' is done.'
!
        call copy_sph_trans_rtm_to_IO(sph_array%sph(ip)%sph_params,     &
     &      sph_array%sph(ip)%sph_rtm, sph_array%comms(ip)%comm_rtm,    &
     &      sph_file_m)
        if(id_rank .lt. sph_array%num_pe) then
          call sel_write_geom_rtm_file                                  &
     &       (id_rank, sph_file_param, sph_file_m, ierr)
        end if
        call dealloc_rtm_grid_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &                      id_rank, ' is done.'
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'copy_sph_trans_rtp_to_IO', id_rank
        call copy_sph_trans_rtp_to_IO(sph_array%sph(ip)%sph_params,     &
     &      sph_array%sph(ip)%sph_rtp, sph_array%comms(ip)%comm_rtp,    &
     &      sph_array%sph_grps(ip), sph_file_m)
        if(id_rank .lt. sph_array%num_pe) then
          call sel_write_geom_rtp_file                                  &
     &       (id_rank, sph_file_param, sph_file_m, ierr)
        end if
        call dealloc_rtp_grid_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical grids for domain',               &
     &          id_rank, ' is done.'
!
        call dealloc_sph_modes(sph_array%sph(ip), sph_array%comms(ip),  &
     &                         sph_array%sph_grps(ip))
      end do
!
      end subroutine para_output_sph_mode_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_output_sph_rj_modes(sph_file_param, sph_array)
!
      use sph_file_IO_select
      use load_data_for_sph_IO
!
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_mesh_array), intent(inout) :: sph_array
!
      integer :: ip, id_rank
      integer(kind = kint) ::  ierr = 0
!
!
      do ip = 1, sph_array%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rj_to_IO(sph_array%sph(ip)%sph_params,      &
     &      sph_array%sph(ip)%sph_rj, sph_array%comms(ip)%comm_rj,      &
     &      sph_array%sph_grps(ip), sph_file_m)
        if(id_rank .lt. sph_array%num_pe) then
          call sel_write_spectr_modes_rj_file                           &
     &       (id_rank, sph_file_param, sph_file_m, ierr)
        end if
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
