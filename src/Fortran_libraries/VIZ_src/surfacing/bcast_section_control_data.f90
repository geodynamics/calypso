!>@file   bcast_section_control_data.f90
!!@brief  module bcast_section_control_data
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine bcast_files_4_psf_ctl(psf_ctls)
!!        type(section_controls), intent(inout) :: psf_ctls
!!      subroutine bcast_files_4_iso_ctl(iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!
!!      subroutine bcast_section_def_control(psf_def_c)
!!        type(psf_define_ctl), intent(inout) :: psf_def_c
!!@endverbatim
!
      module bcast_section_control_data
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_psf_control_data, bcast_iso_control_data
      private :: bcast_fld_on_psf_control, bcast_iso_define_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_psf_ctl(psf_ctls)
!
      use t_control_data_sections
      use t_control_data_4_psf
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(section_controls), intent(inout) :: psf_ctls
      integer (kind=kint) :: i_psf
!
!
      call calypso_mpi_bcast_character(psf_ctls%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(psf_ctls%num_psf_ctl, 0)
      if(psf_ctls%num_psf_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_psf_ctl_stract(psf_ctls)
!
      do i_psf = 1, psf_ctls%num_psf_ctl
        call bcast_psf_control_data(psf_ctls%psf_ctl_struct(i_psf))
      end do
      call calypso_mpi_bcast_character(psf_ctls%fname_psf_ctl,          &
     &    cast_long(psf_ctls%num_psf_ctl*kchara), 0)
!
      end subroutine bcast_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_iso_ctl(iso_ctls)
!
      use t_control_data_isosurfaces
      use t_control_data_4_iso
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      integer (kind=kint) :: i_iso
!
!
      call calypso_mpi_bcast_character(iso_ctls%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(iso_ctls%num_iso_ctl, 0)
      if(iso_ctls%num_iso_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_iso_ctl_stract(iso_ctls)
!
      call calypso_mpi_bcast_character(iso_ctls%fname_iso_ctl,          &
     &    cast_long(iso_ctls%num_iso_ctl*kchara), 0)
      do i_iso = 1, iso_ctls%num_iso_ctl
        call bcast_iso_control_data(iso_ctls%iso_ctl_struct(i_iso))
      end do
!
      end subroutine bcast_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_psf_control_data(psf_c)
!
      use t_control_data_4_psf
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use transfer_to_long_integers
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call calypso_mpi_bcast_one_int(psf_c%i_psf_ctl, 0)
      call calypso_mpi_bcast_one_int(psf_c%i_output_field, 0)
      call calypso_mpi_bcast_character(psf_c%fname_section_ctl,         &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(psf_c%fname_fld_on_psf,          &
     &                                 cast_long(kchara), 0)
!
      call bcast_ctl_type_c1(psf_c%psf_file_head_ctl)
      call bcast_ctl_type_c1(psf_c%psf_output_type_ctl)
!
      call bcast_section_def_control(psf_c%psf_def_c)
      call bcast_fld_on_psf_control(psf_c%fld_on_psf_c)
!
      end subroutine bcast_psf_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_iso_control_data(iso_c)
!
      use t_control_data_4_iso
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      call calypso_mpi_bcast_one_int(iso_c%i_iso_ctl, 0)
      call calypso_mpi_bcast_character(iso_c%fname_fld_on_iso,          &
     &                                 cast_long(kchara), 0)
!
      call bcast_ctl_type_c1(iso_c%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso_c%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso_c%iso_output_type_ctl)
!
      call bcast_iso_define_control(iso_c%iso_def_c)
      call bcast_fld_on_psf_control(iso_c%fld_on_iso_c)
!
      end subroutine bcast_iso_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_fld_on_psf_control(fld_on_psf_c)
!
      use t_control_data_4_fld_on_psf
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!
!
      call bcast_ctl_type_r1(fld_on_psf_c%output_value_ctl)
      call bcast_ctl_type_c1(fld_on_psf_c%output_type_ctl)
      call bcast_ctl_array_c2(fld_on_psf_c%field_output_ctl)
!
      call calypso_mpi_bcast_character(fld_on_psf_c%block_name,         &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(fld_on_psf_c%i_iso_result, 0)
!
      end subroutine bcast_fld_on_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine bcast_section_def_control(psf_def_c)
!
      use t_control_data_4_psf_def
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
!
      call calypso_mpi_bcast_character(psf_def_c%block_name,            &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(psf_def_c%i_surface_define, 0)
!
      call bcast_ctl_array_cr(psf_def_c%psf_coefs_ctl)
      call bcast_ctl_array_cr(psf_def_c%psf_center_ctl)
      call bcast_ctl_array_cr(psf_def_c%psf_normal_ctl)
      call bcast_ctl_array_cr(psf_def_c%psf_axis_ctl)
!
      call bcast_ctl_array_c1(psf_def_c%psf_area_ctl)
!
      call bcast_ctl_type_r1(psf_def_c%radius_psf_ctl)
!
      call bcast_ctl_type_c1(psf_def_c%section_method_ctl)
      call bcast_ctl_type_c1(psf_def_c%psf_group_name_ctl)
!
      end subroutine bcast_section_def_control
!
!   --------------------------------------------------------------------
!
      subroutine bcast_iso_define_control(iso_def_c)
!
      use t_control_data_4_iso_def
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(iso_define_ctl), intent(inout) :: iso_def_c
!
!
      call bcast_ctl_type_c1(iso_def_c%isosurf_comp_ctl)
      call bcast_ctl_array_c1(iso_def_c%iso_area_ctl)
      call bcast_ctl_type_r1(iso_def_c%isosurf_value_ctl)
      call bcast_ctl_type_c1(iso_def_c%isosurf_data_ctl)
!
      call calypso_mpi_bcast_character(iso_def_c%block_name,            &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(iso_def_c%i_iso_define, 0)
!
      end subroutine bcast_iso_define_control
!
!   --------------------------------------------------------------------
!
      end module bcast_section_control_data
