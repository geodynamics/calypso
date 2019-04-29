!>@file   output_4_psf.f90
!!@brief  module output_4_psf
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Output routines for surfacing module
!!
!!@verbatim
!!      subroutine output_section_mesh                                  &
!!     &         (num_psf, psf_file_IO, psf_mesh, psf_out, psf_out_m)
!!      subroutine output_section_data(num_psf, psf_file_IO, istep_psf, &
!!     &          time_d, t_IO, psf_out, psf_out_m)
!!        type(time_data), intent(in) :: time_d
!!        type(psf_local_data), intent(in) :: psf_mesh(num_psf)
!!        type(field_IO_params), intent(in) :: ucd_param(num_psf)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) ::        psf_out(num_psf)
!!        type(merged_ucd_data), intent(inout) :: psf_out_m(num_psf)
!!
!!      subroutine output_isosurface(num_iso, iso_file_IO, istep_iso,   &
!!     &          time_d, iso_mesh, t_IO, iso_out, iso_out_m)
!!        type(time_data), intent(in) :: time_d
!!        type(psf_local_data), intent(in) :: iso_mesh(num_iso)
!!        type(field_IO_params), intent(in) :: iso_file_IO(num_iso)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: iso_out(num_iso)
!!        type(merged_ucd_data), intent(inout) :: iso_out_m(num_iso)
!!@endverbatim
!
      module output_4_psf
!
      use m_precision
!
      use calypso_mpi
      use t_time_data
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
      use t_psf_patch_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_section_mesh                                    &
     &         (num_psf, psf_file_IO, psf_mesh, psf_out, psf_out_m)
!
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_psf
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
      type(field_IO_params), intent(in) :: psf_file_IO(num_psf)
!
      type(ucd_data), intent(inout) ::        psf_out(num_psf)
      type(merged_ucd_data), intent(inout) :: psf_out_m(num_psf)
!
      integer(kind= kint) :: i_psf
!
!
      do i_psf = 1, num_psf
          call link_nnod_stacks_2_ucd(nprocs, psf_mesh(i_psf)%node,     &
     &        psf_mesh(i_psf)%patch, psf_out_m(i_psf))

          call link_node_data_2_ucd                                     &
     &         (psf_mesh(i_psf)%node, psf_out(i_psf))
          call link_ele_data_2_ucd                                      &
     &         (psf_mesh(i_psf)%patch, psf_out(i_psf))
          call link_field_data_to_ucd                                   &
     &         (psf_mesh(i_psf)%field, psf_out(i_psf))
      end do
!
      do i_psf = 1, num_psf
          call sel_write_parallel_ucd_mesh                              &
     &       (psf_file_IO(i_psf), psf_out(i_psf), psf_out_m(i_psf))
      end do
      call calypso_mpi_barrier
!
      end subroutine output_section_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_section_data(num_psf, psf_file_IO, istep_psf,   &
     &          time_d, t_IO, psf_out, psf_out_m)
!
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_psf
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(field_IO_params), intent(in) :: psf_file_IO(num_psf)
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) ::        psf_out(num_psf)
      type(merged_ucd_data), intent(inout) :: psf_out_m(num_psf)
!
      integer(kind= kint) :: i_psf
!
!
      call copy_time_step_size_data(time_d, t_IO)
!
      do i_psf = 1, num_psf
        call sel_write_parallel_ucd_file(istep_psf, psf_file_IO(i_psf), &
     &      t_IO, psf_out(i_psf), psf_out_m(i_psf))
      end do
      call calypso_mpi_barrier
!
      end subroutine output_section_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine output_isosurface(num_iso, iso_file_IO, istep_iso,     &
     &          time_d, iso_mesh, t_IO, iso_out, iso_out_m)
!
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_iso
      integer(kind= kint), intent(in) :: istep_iso
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: iso_mesh(num_iso)
!
!>      Structure for isosurface output (used by master process)
      type(field_IO_params), intent(in) :: iso_file_IO(num_iso)
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: iso_out(num_iso)
      type(merged_ucd_data), intent(inout) :: iso_out_m(num_iso)
!
      integer(kind= kint) :: i_iso
!
!
      call copy_time_step_size_data(time_d, t_IO)
!
      do i_iso = 1, num_iso
          call link_nnod_stacks_2_ucd(nprocs, iso_mesh(i_iso)%node,     &
     &        iso_mesh(i_iso)%patch, iso_out_m(i_iso))
!
          call link_node_data_2_ucd                                     &
     &       (iso_mesh(i_iso)%node, iso_out(i_iso))
          call link_ele_data_2_ucd                                      &
     &       (iso_mesh(i_iso)%patch, iso_out(i_iso))
          call link_field_data_to_ucd                                   &
     &       (iso_mesh(i_iso)%field, iso_out(i_iso))
      end do
!
      do i_iso = 1, num_iso
          call sel_write_parallel_ucd_file                              &
     &       (istep_iso, iso_file_IO(i_iso), t_IO,                      &
     &        iso_out(i_iso), iso_out_m(i_iso))
          call disconnect_merged_ucd_mesh                               &
     &       (iso_out(i_iso), iso_out_m(i_iso))
      end do
!
      call calypso_mpi_barrier
!
      end subroutine output_isosurface
!
!  ---------------------------------------------------------------------
!
      end module output_4_psf
