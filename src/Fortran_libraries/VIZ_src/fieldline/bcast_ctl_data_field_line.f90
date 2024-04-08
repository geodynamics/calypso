!>@file   bcast_ctl_data_field_line.f90
!!@brief  module bcast_ctl_data_field_line
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each field line
!!
!!@verbatim
!!      subroutine bcast_files_4_fline_ctl(fline_ctls)
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!      subroutine bcast_field_line_ctl(fln)
!!        type(fline_ctl), intent(inout) :: fln
!!@endverbatim
!
      module bcast_ctl_data_field_line
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_field_line_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_fline_ctl(fline_ctls)
!
      use t_control_data_flines
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(fieldline_controls), intent(inout) :: fline_ctls
      integer (kind=kint) :: i_fline
!
!
      call calypso_mpi_bcast_character(fline_ctls%block_name,           &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(fline_ctls%num_fline_ctl, 0)
      if(fline_ctls%num_fline_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_fline_ctl_struct(fline_ctls)
!
      call calypso_mpi_bcast_character(fline_ctls%fname_fline_ctl,      &
     &    cast_long(kchara*fline_ctls%num_fline_ctl), 0)
      do i_fline = 1, fline_ctls%num_fline_ctl
        call bcast_field_line_ctl(fline_ctls%fline_ctl_struct(i_fline))
      end do
!
      end subroutine bcast_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_field_line_ctl(fln)
!
      use t_ctl_data_field_line
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(fline_ctl), intent(inout) :: fln
!
!
      call calypso_mpi_bcast_character(fln%block_name,                  &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(fln%i_vr_fline_ctl, 0)
!
      call bcast_ctl_array_c1(fln%fline_area_grp_ctl)
!
      call bcast_ctl_array_r3(fln%seed_point_ctl)
      call bcast_ctl_array_i2(fln%seed_surface_ctl)
!
!
      call bcast_ctl_type_c1(fln%fline_file_head_ctl)
      call bcast_ctl_type_c1(fln%fline_output_type_ctl)
!
      call bcast_ctl_type_c1(fln%fline_field_ctl )
      call bcast_ctl_type_c1(fln%fline_color_field_ctl )
      call bcast_ctl_type_c1(fln%fline_color_comp_ctl )
      call bcast_ctl_type_c1(fln%starting_type_ctl )
      call bcast_ctl_type_c1(fln%start_surf_grp_ctl )
      call bcast_ctl_type_c1(fln%selection_type_ctl )
      call bcast_ctl_type_c1(fln%line_direction_ctl )
!
      call bcast_ctl_type_i1(fln%num_fieldline_ctl)
      call bcast_ctl_type_i1(fln%max_line_stepping_ctl)
!
      end subroutine bcast_field_line_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_field_line
