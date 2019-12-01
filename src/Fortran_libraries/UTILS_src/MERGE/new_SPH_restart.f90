!>@file   new_SPH_restart.f90
!!@brief  module new_SPH_restart
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine alloc_sph_mesh_parallel_merge
!!      subroutine dealloc_sph_mesh_4_merge
!!
!!      subroutine set_local_rj_mesh_4_merge                            &
!!     &         (sph_mesh_file, num_pe, sph_mesh)
!!        type(field_IO_params), intent(in) :: sph_mesh_file
!!        type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!!      subroutine load_field_name_assemble_sph(istep_start, np_sph_org,&
!!     &          org_fst_param, org_phys, new_phys, t_IO)
!!      subroutine load_org_sph_data(id_rank, istep, np_sph_org,        &
!!     &          org_fst_param, org_sph, init_d, org_phys)
!!      subroutine load_old_fmt_sph_data(id_rank, istep, np_sph_org,    &
!!     &          org_fst_param, org_sph, org_phys)
!!        type(sph_grids), intent(in) :: org_sph
!!        type(time_data), intent(inout) :: time_d
!!        type(phys_data), intent(inout) :: org_phys
!!      subroutine set_assembled_sph_data                               &
!!     &         (org_sph_mesh, new_sph_mesh, j_table, r_itp,           &
!!     &          org_phys, new_phys)
!!
!!      subroutine const_assembled_sph_data(b_ratio, time_d,            &
!!     &          new_sph, r_itp, new_phys, new_fst_IO, t_IO)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: new_sph
!!        type(sph_radial_itp_data), intent(in) :: r_itp
!!        type(phys_data), intent(inout) :: new_phys
!!        type(field_IO), intent(inout) :: new_fst_IO
!!        type(time_data), intent(in) :: t_IO
!!@endverbatim
!
      module new_SPH_restart
!
      use m_precision
!
      use calypso_mpi
      use t_time_data
      use t_SPH_mesh_field_data
      use t_file_IO_parameter
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_local_rj_mesh_4_merge                              &
     &         (sph_mesh_file, num_pe, sph_mesh)
!
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use load_data_for_sph_IO
!
      integer, intent(in) ::  num_pe
      type(field_IO_params), intent(in) :: sph_mesh_file
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      type(field_IO_params) :: sph_file_param
      type(sph_file_data_type) :: sph_file
      integer :: id_rank, ip
      integer(kind = kint) :: iloop, ierr
!
!
      call set_sph_mesh_file_fmt_prefix                                 &
     &   (sph_mesh_file%iflag_format, sph_mesh_file%file_prefix,        &
     &    sph_file_param)
      do iloop = 0, (num_pe-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_spectr_rj_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file)
!
        if(id_rank .lt. num_pe) then
          call input_modes_rj_sph_trans(sph_file,                       &
     &       sph_mesh(ip)%sph%sph_rj, sph_mesh(ip)%sph_comms%comm_rj,   &
     &       sph_mesh(ip)%sph_grps, sph_mesh(ip)%sph%sph_params, ierr)
          call dealloc_rj_mode_IO(sph_file)
        end if
      end do
!
      end subroutine set_local_rj_mesh_4_merge
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_field_name_assemble_sph(istep_start, np_sph_org,  &
     &          org_fst_param, org_phys, new_phys, t_IO)
!
      use calypso_mpi
      use t_spheric_parameter
      use copy_rj_phys_data_4_IO
      use field_IO_select
!
      integer,  intent(in) :: np_sph_org
      integer(kind = kint),  intent(in) :: istep_start
      type(field_IO_params), intent(in) :: org_fst_param
!
      type(phys_data), intent(inout) :: org_phys(np_sph_org)
      type(phys_data), intent(inout) :: new_phys
      type(time_data), intent(inout) :: t_IO
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
      integer :: ip
!
!
      call sel_read_alloc_step_SPH_file(np_sph_org, 0, istep_start,     &
     &    org_fst_param, t_IO, org_fst_IO)
!
      if(my_rank .eq. 0) then
        call copy_rj_phys_name_from_IO(org_fst_IO, new_phys)
!
        do ip = 1, np_sph_org
          call copy_field_name_type(new_phys, org_phys(ip))
        end do
      end if
!
      if(my_rank .lt. np_sph_org) then
        call dealloc_phys_data_IO(org_fst_IO)
        call dealloc_phys_name_IO(org_fst_IO)
      end if
!
      end subroutine load_field_name_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_org_sph_data(id_rank, istep, np_sph_org,          &
     &          org_fst_param, org_sph, init_d, org_phys)
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      integer, intent(in) :: np_sph_org
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(in) :: istep
      type(sph_grids), intent(in) :: org_sph
      type(field_IO_params), intent(in) :: org_fst_param
!
      type(time_data), intent(inout) :: init_d
      type(phys_data), intent(inout) :: org_phys
!
!>      Field data IO structure for original data
      type(time_data) :: org_time_IO
      type(field_IO) :: org_fst_IO
!
!
      call sel_read_alloc_step_SPH_file(np_sph_org, id_rank, istep,     &
     &    org_fst_param, org_time_IO, org_fst_IO)
!
      if(id_rank .lt. np_sph_org) then
        call copy_time_steps_from_restart(org_time_IO, init_d)
!
        call alloc_phys_data_type(org_sph%sph_rj%nnod_rj, org_phys)
        call copy_rj_phys_data_from_IO(org_fst_IO, org_phys)
!
        call dealloc_phys_data_IO(org_fst_IO)
        call dealloc_phys_name_IO(org_fst_IO)
      end if
!
      end subroutine load_org_sph_data
!
! -----------------------------------------------------------------------
!
      subroutine load_old_fmt_sph_data(id_rank, istep, np_sph_org,      &
     &          org_fst_param, org_sph, org_phys)
!
      use input_old_file_sel_4_zlib
      use copy_rj_phys_data_4_IO
!
      integer, intent(in) :: np_sph_org
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(in) :: istep
      type(sph_grids), intent(in) :: org_sph
      type(field_IO_params), intent(in) :: org_fst_param
!
      type(phys_data), intent(inout) :: org_phys
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
!
!
      call sel_read_alloc_field_file                                    &
     &   (id_rank, istep, org_fst_param, org_fst_IO)
!
      if(id_rank .lt. np_sph_org) then
        call alloc_phys_data_type(org_sph%sph_rj%nnod_rj, org_phys)
        call copy_rj_phys_data_from_IO(org_fst_IO, org_phys)
!
        call dealloc_phys_data_IO(org_fst_IO)
        call dealloc_phys_name_IO(org_fst_IO)
      end if
!
      end subroutine load_old_fmt_sph_data
!
! -----------------------------------------------------------------------
!
      subroutine set_assembled_sph_data                                 &
     &         (org_sph_mesh, new_sph_mesh, j_table, r_itp,             &
     &          org_phys, new_phys)
!
      use t_SPH_mesh_field_data
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      use parallel_assemble_sph
!
      type(sph_mesh_data), intent(in) :: org_sph_mesh
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(rj_assemble_tbl), intent(in) :: j_table
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(phys_data), intent(in) ::    org_phys
!
      type(phys_data), intent(inout) :: new_phys
!
!
      if(r_itp%iflag_same_rgrid .eq. 0) then
        call r_itp_field_data_sph_assemble                              &
     &     (org_sph_mesh%sph, new_sph_mesh%sph, r_itp, j_table,         &
     &      new_phys%ntot_phys, org_phys%d_fld, new_phys%d_fld)
      else
        call copy_field_data_sph_assemble                               &
     &     (org_sph_mesh%sph, new_sph_mesh%sph, j_table,                &
     &      new_phys%ntot_phys, org_phys%d_fld, new_phys%d_fld)
      end if
!
      call copy_field_data_sph_center                                   &
     &   (org_sph_mesh%sph, new_sph_mesh%sph, j_table,                  &
     &    new_phys%ntot_phys, org_phys%d_fld, new_phys%d_fld)
!
      end subroutine set_assembled_sph_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_assembled_sph_data(b_ratio, time_d,              &
     &          new_sph, r_itp, new_phys, new_fst_IO, t_IO)
!
      use calypso_mpi
      use m_phys_labels
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use field_IO_select
      use assemble_sph_fields
      use set_merged_restart_data
!
      real(kind=kreal ), intent(in) :: b_ratio
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: new_sph
      type(sph_radial_itp_data), intent(in) :: r_itp
!
      type(phys_data), intent(inout) :: new_phys
      type(field_IO), intent(inout) :: new_fst_IO
      type(time_data), intent(inout) :: t_IO
!
!
        if(r_itp%iflag_same_rgrid .eq. 0) then
!        write(*,*) 'extend_potential_magne'
          call extend_potential_magne(new_sph, r_itp, new_phys)
!            write(*,*) 'extend_inner_core_scalar'
          call extend_inner_core_scalar                                 &
     &      (fhd_temp, new_sph, r_itp, new_phys)
!            write(*,*) 'extend_inner_core_scalar'
          call extend_inner_core_scalar                                 &
     &        (fhd_light, new_sph, r_itp, new_phys)
        end if
!
        call rescale_4_magne(b_ratio, new_phys)
!
        call copy_time_step_size_data(time_d, t_IO)
        call copy_rj_phys_name_to_IO                                    &
     &     (new_phys%num_phys, new_phys, new_fst_IO)
!
        new_fst_IO%nnod_IO = new_sph%sph_rj%nnod_rj
        call alloc_phys_data_IO(new_fst_IO)
!
        call copy_rj_phys_data_to_IO                                    &
     &     (new_phys%num_phys, new_phys, new_fst_IO)
!
      end subroutine const_assembled_sph_data
!
! -----------------------------------------------------------------------
!
      end module new_SPH_restart
