!>@file   bcast_dynamo_viz_control.f90
!!@brief  module bcast_dynamo_viz_control
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!@n        Modified by H. Matsui on Apr., 2023
!!
!!@verbatim
!!      subroutine s_bcast_dynamo_viz_control(zm_ctls)
!!        type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!!@endverbatim
!
      module bcast_dynamo_viz_control
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_bcast_dynamo_viz_control(zm_ctls)
!
      use t_control_data_dynamo_vizs
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_section_control_data
      use bcast_maps_control_data
      use bcast_control_sph_MHD
!
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
!
      call bcast_crustal_filtering_ctl(zm_ctls%crust_filter_ctl)
      call bcast_files_4_psf_ctl(zm_ctls%zm_psf_ctls)
      call bcast_files_4_psf_ctl(zm_ctls%zRMS_psf_ctls)
      call bcast_files_4_map_ctl(zm_ctls%zm_map_ctls)
      call bcast_files_4_map_ctl(zm_ctls%zRMS_map_ctls)
!
      call calypso_mpi_bcast_character                                  &
     &   (zm_ctls%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(zm_ctls%i_viz_ctl, 0)
!
      end subroutine s_bcast_dynamo_viz_control
!
!   --------------------------------------------------------------------
!
      end module bcast_dynamo_viz_control
