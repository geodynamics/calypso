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
!!      subroutine load_field_name_assemble_sph(istep_start,            &
!!     &          org_fst_param, org_sph_array, new_sph_data, t_IO)
!!      subroutine load_org_sph_data(istep, org_fst_param,              &
!!     &                             init_d, org_sph_array)
!!      subroutine load_old_fmt_sph_data(istep, org_fst_param,          &
!!     &                                 org_sph_array)
!!        type(time_data), intent(inout) :: time_d
!!        type(sph_mesh_array), intent(in) :: org_sph_array
!!      subroutine set_assembled_sph_data(num_org_pe, org_sph, j_table, &
!!     &                                  r_itp, org_fld, new_sph_data)
!!        type(SPH_mesh_field_data), intent(in) :: new_sph_data
!!        type(rj_assemble_tbl), intent(in)                             &
!!     &                      :: j_table(org_sph_array%num_pe)
!!        type(sph_radial_interpolate), intent(in) :: r_itp
!!        type(sph_grids), intent(in) :: org_sph(num_org_pe)
!!        type(phys_data), intent(inout) :: org_fld(num_org_pe)
!!        type(SPH_mesh_field_data), intent(inout) :: new_sph_data
!!
!!      subroutine const_assembled_sph_data(b_ratio, time_d,            &
!!     &          r_itp, new_sph_data, new_fst_IO, t_IO)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_radial_interpolate), intent(in) :: r_itp
!!        type(SPH_mesh_field_data), intent(in) :: new_sph_data
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
      use t_SPH_mesh_field_array
      use t_SPH_mesh_field_data
      use t_file_IO_parameter
      use t_time_data
      use t_phys_data
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
      subroutine load_field_name_assemble_sph(istep_start,              &
     &          org_fst_param, org_sph_array, new_sph_data, t_IO)
!
      use calypso_mpi
      use t_spheric_parameter
      use append_phys_data
      use copy_rj_phys_data_4_IO
      use field_IO_select
!
      integer(kind = kint),  intent(in) :: istep_start
      type(field_IO_params), intent(in) :: org_fst_param
!
      type(sph_mesh_array), intent(inout) :: org_sph_array
      type(SPH_mesh_field_data), intent(inout) :: new_sph_data
      type(time_data), intent(inout) :: t_IO
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
      integer :: ip
!      integer :: i
!
!
      call sel_read_alloc_SPH_fld_head(org_sph_array%num_pe, 0,         &
     &    istep_start, org_fst_param, t_IO, org_fst_IO)
!
      if(my_rank .eq. 0) then
!        write(*,*) 'org_fst_IO%num_field_IO', org_fst_IO%num_field_IO
!        do i = 1, org_fst_IO%num_field_IO
!          write(*,*) 'org_fst_IO%fld_name', &
!     &              org_fst_IO%num_comp_IO(i), &
!     &              trim(org_fst_IO%fld_name(i))
!        end do
!
        call copy_rj_phys_name_from_IO(org_fst_IO, new_sph_data%fld)
!
        do ip = 1, org_sph_array%num_pe
          call copy_field_name(new_sph_data%fld,                        &
     &                         org_sph_array%fld(ip))
        end do
      end if
!
      if(my_rank .lt. org_sph_array%num_pe) then
        call dealloc_phys_name_IO(org_fst_IO)
      end if
!
      end subroutine load_field_name_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_org_sph_data(istep, org_fst_param,                &
     &                             init_d, org_sph_array)
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: org_fst_param
!
      type(time_data), intent(inout) :: init_d
      type(sph_mesh_array), intent(inout) :: org_sph_array
!
!>      Field data IO structure for original data
      type(time_data) :: org_time_IO
      type(field_IO) :: org_fst_IO
!
      integer(kind = kint) :: n_point
      integer :: iloop, irank_new, ip
!
!
      do iloop = 0, (org_sph_array%num_pe-1) / nprocs
        irank_new = int(my_rank + iloop * nprocs)
        ip = irank_new + 1
        call sel_read_alloc_step_SPH_file                               &
     &     (org_sph_array%num_pe, irank_new, istep,                     &
     &      org_fst_param, org_time_IO, org_fst_IO)
!
        if(irank_new .lt. org_sph_array%num_pe) then
          call copy_time_steps_from_restart(org_time_IO, init_d)
!
          n_point = org_sph_array%sph(ip)%sph_rj%nnod_rj
          call alloc_phys_data(n_point, org_sph_array%fld(ip))
          call copy_rj_phys_data_from_IO                                &
     &       (org_fst_IO, org_sph_array%fld(ip))
!
          call dealloc_phys_data_IO(org_fst_IO)
          call dealloc_phys_name_IO(org_fst_IO)
        end if
!
        call calypso_mpi_barrier
      end do

      end subroutine load_org_sph_data
!
! -----------------------------------------------------------------------
!
      subroutine load_old_fmt_sph_data(istep, org_fst_param,            &
     &                                 org_sph_array)
!
      use input_old_file_sel_4_zlib
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: org_fst_param
!
      type(sph_mesh_array), intent(inout) :: org_sph_array
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
      integer(kind = kint) :: n_point
      integer(kind = kint) :: ierr_IO = 0
      integer :: irank_new, iloop, ip
!
!
      do iloop = 0, (org_sph_array%num_pe-1) / nprocs
        irank_new = int(my_rank + iloop * nprocs)
        ip = irank_new + 1
!
        call sel_read_alloc_field_file                                  &
     &     (irank_new, istep, org_fst_param, org_fst_IO, ierr_IO)
        if(ierr_IO .gt. 0) call calypso_MPI_abort(ierr_IO,              &
     &                 'Read file error in sel_read_alloc_field_file')
!
        if(irank_new .lt. org_sph_array%num_pe) then
          n_point = org_sph_array%sph(ip)%sph_rj%nnod_rj
          call alloc_phys_data(n_point, org_sph_array%fld(ip))
          call copy_rj_phys_data_from_IO                                &
     &       (org_fst_IO, org_sph_array%fld(ip))
!
          call dealloc_phys_data_IO(org_fst_IO)
          call dealloc_phys_name_IO(org_fst_IO)
        end if
!
        call calypso_mpi_barrier
      end do
!
      end subroutine load_old_fmt_sph_data
!
! -----------------------------------------------------------------------
!
      subroutine set_assembled_sph_data(num_org_pe, org_sph, j_table,   &
     &                                  r_itp, org_fld, new_sph_data)
!
      use t_spheric_parameter
      use t_sph_radial_interpolate
!
      use parallel_assemble_sph
      use share_field_data
!
      integer, intent(in) :: num_org_pe
      type(sph_grids), intent(in) :: org_sph(num_org_pe)
      type(rj_assemble_tbl), intent(in) :: j_table(num_org_pe)
      type(sph_radial_interpolate), intent(in) :: r_itp
!
      type(phys_data), intent(inout) :: org_fld(num_org_pe)
      type(SPH_mesh_field_data), intent(inout) :: new_sph_data
!
      integer :: ip
!
!
      do ip = 1, num_org_pe
!         Bloadcast original spectr data
        call share_each_field_data(ip, org_fld(ip))
!
!         Copy spectr data to temporal array
        if(r_itp%flag_same_rgrid) then
          call copy_field_data_sph_assemble                             &
     &       (org_sph(ip), new_sph_data%sph, j_table(ip),               &
     &        org_fld(ip), new_sph_data%fld)
        else
          call r_itp_field_data_sph_assemble                            &
     &       (org_sph(ip), new_sph_data%sph, r_itp, j_table(ip),        &
     &        org_fld(ip), new_sph_data%fld)
        end if
!
        call copy_field_data_sph_center                                 &
     &     (org_sph(ip), new_sph_data%sph, j_table(ip),                 &
     &      org_fld(ip), new_sph_data%fld)
!
        call dealloc_phys_data(org_fld(ip))
      end do
!
      end subroutine set_assembled_sph_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_assembled_sph_data(b_ratio, time_d,              &
     &          r_itp, new_sph_data, new_fst_IO, t_IO)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_sph_radial_interpolate
!
      use m_base_field_labels
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
      type(sph_radial_interpolate), intent(in) :: r_itp
!
      type(SPH_mesh_field_data), intent(inout) :: new_sph_data
      type(field_IO), intent(inout) :: new_fst_IO
      type(time_data), intent(inout) :: t_IO
!
!
        if(r_itp%flag_same_rgrid .eqv. .FALSE.) then
!        write(*,*) 'extend_potential_magne'
          call extend_potential_magne(new_sph_data%sph, r_itp,          &
     &                                new_sph_data%fld)
!            write(*,*) 'extend_inner_core_scalar'
          call extend_inner_core_scalar(temperature%name,               &
     &        new_sph_data%sph, r_itp, new_sph_data%fld)
!            write(*,*) 'extend_inner_core_scalar'
          call extend_inner_core_scalar(composition%name,               &
     &        new_sph_data%sph, r_itp, new_sph_data%fld)
        end if
!
        call rescale_4_magne(b_ratio, new_sph_data%fld)
!
        call copy_time_step_size_data(time_d, t_IO)
        call copy_rj_phys_name_to_IO                                    &
     &     (new_sph_data%fld%num_phys, new_sph_data%fld, new_fst_IO)
!
        new_fst_IO%nnod_IO = new_sph_data%sph%sph_rj%nnod_rj
        call alloc_phys_data_IO(new_fst_IO)
!
        call copy_rj_phys_data_to_IO                                    &
     &     (new_sph_data%fld%num_phys, new_sph_data%fld, new_fst_IO)
!
      end subroutine const_assembled_sph_data
!
! -----------------------------------------------------------------------
!
      end module new_SPH_restart
