!>@file   bcast_ctl_data_pvr_surfaces.f90
!!@brief  module bcast_ctl_data_pvr_surfaces
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine bcast_pvr_sections_ctl(pvr_scts_c)
!!        type(pvr_section_ctl), intent(inout) :: pvr_scts_c
!!      subroutine bcast_pvr_isosurfs_ctl(pvr_isos_c)
!!        type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!!      subroutine bcast_pvr_section_ctl(pvr_scts_c)
!!        type(pvr_section_ctl), intent(inout) :: pvr_scts_c
!!@endverbatim
!
      module bcast_ctl_data_pvr_surfaces
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_sections_ctl(pvr_scts_c)
!
      use t_control_data_pvr_sections
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
      integer(kind = kint) :: i
!
!
      call calypso_mpi_bcast_one_int(pvr_scts_c%num_pvr_sect_ctl, 0)
      if(pvr_scts_c%num_pvr_sect_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(pvr_scts_c%pvr_sect_ctl(pvr_scts_c%num_pvr_sect_ctl))
      end if
!
      do i = 1, pvr_scts_c%num_pvr_sect_ctl
        call bcast_pvr_section_ctl(pvr_scts_c%pvr_sect_ctl(i))
      end do
!
      end subroutine bcast_pvr_sections_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurfs_ctl(pvr_isos_c)
!
      use t_control_data_pvr_isosurfs
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
      integer(kind = kint) :: i
!
!
      call calypso_mpi_bcast_one_int(pvr_isos_c%num_pvr_iso_ctl, 0)
      if(pvr_isos_c%num_pvr_iso_ctl .gt. 0 .and. my_rank .gt. 0) then
        call alloc_pvr_isosurfs_ctl(pvr_isos_c)
      end if
      call calypso_mpi_barrier
!
      do i = 1, pvr_isos_c%num_pvr_iso_ctl
        call bcast_pvr_isosurface_ctl(pvr_isos_c%pvr_iso_ctl(i))
      end do
!
!      write(*,*) my_rank, 'pvr_isos_c%num_pvr_iso_ctl',                &
!     &                     pvr_isos_c%num_pvr_iso_ctl
!      do i = 1, pvr_isos_c%num_pvr_iso_ctl
!        write(*,*) my_rank,                                            &
!    &         'pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue',     &
!    &       i, pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%iflag,          &
!    &          pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue
!        write(*,*) my_rank,                                            &
!    &         'pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue',       &
!    &       i, pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%iflag,            &
!    &          pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue
!        write(*,*) my_rank,                                            &
!    &         'pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%realvalue',  &
!    &       i, pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%iflag,       &
!    &          pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%charavalue
!     end do
!
      end subroutine bcast_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_section_ctl(pvr_scts_c)
!
      use t_ctl_data_pvr_section
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
      use bcast_section_control_data
!
      type(pvr_section_ctl), intent(inout) :: pvr_scts_c
!
!
      call calypso_mpi_bcast_one_int(pvr_scts_c%i_pvr_sect_ctl, 0)
      call calypso_mpi_bcast_character                                  &
     &   (pvr_scts_c%fname_sect_ctl, cast_long(kchara), 0)
!
      call bcast_section_def_control(pvr_scts_c%psf_def_c)
      call bcast_ctl_type_r1(pvr_scts_c%opacity_ctl)
!
      call bcast_ctl_type_c1(pvr_scts_c%zeroline_switch_ctl)
      call bcast_ctl_type_c1(pvr_scts_c%isoline_color_mode)
      call bcast_ctl_type_i1(pvr_scts_c%isoline_number_ctl)
      call bcast_ctl_type_r2(pvr_scts_c%isoline_range_ctl)
      call bcast_ctl_type_r1(pvr_scts_c%isoline_width_ctl)
      call bcast_ctl_type_r1(pvr_scts_c%grid_width_ctl)
!
      call bcast_ctl_type_c1(pvr_scts_c%tan_cyl_switch_ctl)
      call bcast_ctl_type_r1(pvr_scts_c%tangent_cylinder_inner_ctl)
      call bcast_ctl_type_r1(pvr_scts_c%tangent_cylinder_outer_ctl)
!
      end subroutine bcast_pvr_section_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurface_ctl(pvr_iso_ctl)
!
      use t_ctl_data_pvr_isosurface
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      call calypso_mpi_bcast_one_int(pvr_iso_ctl%i_pvr_isosurf_ctl, 0)
      call bcast_ctl_type_c1                                            &
     &   (pvr_iso_ctl%isosurf_type_ctl)
      call bcast_ctl_type_r1                                            &
     &   (pvr_iso_ctl%iso_value_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%opacity_ctl)
!
      end subroutine bcast_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_pvr_surfaces
