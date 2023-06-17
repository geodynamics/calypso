!>@file   bcast_maps_control_data.f90
!!@brief  module bcast_maps_control_data
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine bcast_files_4_map_ctl(map_ctls)
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!@endverbatim
!
      module bcast_maps_control_data
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_map_control_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_map_ctl(map_ctls)
!
      use t_control_data_maps
      use t_control_data_4_map
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      integer (kind=kint) :: i_psf
!
!
      call calypso_mpi_bcast_one_int(map_ctls%num_map_ctl, 0)
      if(map_ctls%num_map_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_map_ctl_stract(map_ctls)
!
      do i_psf = 1, map_ctls%num_map_ctl
        call bcast_map_control_data(map_ctls%map_ctl_struct(i_psf))
      end do
      call calypso_mpi_bcast_character(map_ctls%fname_map_ctl,          &
     &    cast_long(map_ctls%num_map_ctl*kchara), 0)
!
      end subroutine bcast_files_4_map_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_map_control_data(map_c)
!
      use t_control_data_4_map
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use bcast_ctl_data_pvr_surfaces
      use bcast_ctl_data_view_trans
      use bcast_pvr_color_ctl
      use transfer_to_long_integers
!
      type(map_ctl), intent(inout) :: map_c
!
!
      call calypso_mpi_bcast_one_int(map_c%i_map_ctl, 0)
      call calypso_mpi_bcast_one_int(map_c%i_output_field, 0)
      call calypso_mpi_bcast_character(map_c%fname_mat_ctl,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(map_c%fname_cmap_cbar_c,         &
     &                                 cast_long(kchara), 0)
!
      call bcast_view_transfer_ctl(map_c%mat)
      call bcast_pvr_colorbar_ctl(map_c%cmap_cbar_c%cbar_ctl)
      call bcast_pvr_colordef_ctl(map_c%cmap_cbar_c%color)
      call bcast_pvr_section_ctl(map_c%map_define_ctl)
!
      call bcast_ctl_type_c1(map_c%map_image_prefix_ctl)
      call bcast_ctl_type_c1(map_c%map_image_fmt_ctl)
      call bcast_ctl_type_c1(map_c%map_field_ctl)
      call bcast_ctl_type_c1(map_c%map_comp_ctl)
      call bcast_ctl_type_c1(map_c%isoline_field_ctl)
      call bcast_ctl_type_c1(map_c%isoline_comp_ctl)
!
      end subroutine bcast_map_control_data
!
!   --------------------------------------------------------------------
!
      end module bcast_maps_control_data
