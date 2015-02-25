!>@file   m_ctl_data_sph_MHD_psf.f90
!!@brief  module m_ctl_data_sph_MHD_psf
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_control_4_sph_MHD_w_psf
!!      subroutine read_control_4_sph_snap_w_psf
!!@endverbatim
!
      module m_ctl_data_sph_MHD_psf
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: control_file_code = 11
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
      integer (kind=kint) :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      private :: MHD_ctl_name, snap_ctl_name
      private :: hd_mhd_ctl, i_mhd_ctl
!
      private :: read_sph_mhd_ctl_w_psf
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_w_psf
!
!
      ctl_file_code = control_file_code
      open ( ctl_file_code, file = MHD_ctl_name, status='old' )
!
      call load_ctl_label_and_line
      call read_sph_mhd_ctl_w_psf
!
      close(ctl_file_code)
!
      end subroutine read_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_snap_w_psf
!
!
      ctl_file_code = control_file_code
      open ( ctl_file_code, file = snap_ctl_name, status='old' )
!
      call load_ctl_label_and_line
      call read_sph_mhd_ctl_w_psf
!
      close(ctl_file_code)
!
      end subroutine read_control_4_sph_snap_w_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_w_psf
!
      use calypso_mpi
      use m_ctl_data_4_platforms
      use m_ctl_data_node_monitor
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_org_data
      use m_control_data_sections
      use read_ctl_data_sph_MHD
!
!
      if(right_begin_flag(hd_mhd_ctl) .eq. 0) return
      if (i_mhd_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_mhd_ctl, i_mhd_ctl)
        if(i_mhd_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
!
        call read_sph_mhd_model
        call read_sph_mhd_control
!
        call read_monitor_data_ctl
        call read_pickup_sph_ctl
!
        call read_sections_control_data
      end do
!
      end subroutine read_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_sph_MHD_psf
