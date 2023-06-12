!>@file   bcast_dynamo_sect_control.f90
!!@brief  module bcast_dynamo_sect_control
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
!!      subroutine s_bcast_dynamo_section_control(zm_sects)
!!        type(sph_dynamo_section_controls), intent(in) :: zm_sects
!!@endverbatim
!
      module bcast_dynamo_sect_control
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
      subroutine s_bcast_dynamo_section_control(zm_sects)
!
      use t_control_data_dynamo_sects
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_section_control_data
      use bcast_control_sph_MHD
!
      type(sph_dynamo_section_controls), intent(inout) :: zm_sects
!
!
      call bcast_crustal_filtering_ctl(zm_sects%crust_filter_ctl)
      call bcast_files_4_psf_ctl(zm_sects%zm_psf_ctls)
      call bcast_files_4_psf_ctl(zm_sects%zRMS_psf_ctls)
!
      end subroutine s_bcast_dynamo_section_control
!
!   --------------------------------------------------------------------
!
      end module bcast_dynamo_sect_control
