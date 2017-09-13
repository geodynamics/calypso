!>@file   t_ctl_data_sph_MHD_psf.f90
!!@brief  module t_ctl_data_sph_MHD_psf
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
!!      subroutine read_control_4_sph_MHD_w_psf(file_name, DNS_MHD_ctl)
!!      subroutine read_control_4_sph_MHD_noviz(file_name, DNS_MHD_ctl)
!!        type(DNS_mhd_simulation_control), intent(inout) :: DNS_MHD_ctl
!!@endverbatim
!
      module t_ctl_data_sph_MHD_psf
!
      use m_precision
!
      use t_ctl_data_MHD
      use m_machine_parameter
      use m_read_control_elements
      use calypso_mpi
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: control_file_code = 11
      private :: control_file_code
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_w_psf(file_name, DNS_MHD_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: DNS_MHD_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = file_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_ctl_w_psf(DNS_MHD_ctl)
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_ctl_w_psf(DNS_MHD_ctl)
!
      if(DNS_MHD_ctl%psph_ctl%ifile_sph_shell .gt. 0) then
        call read_ctl_file_gen_shell_grids(DNS_MHD_ctl%psph_ctl)
      end if
!
      end subroutine read_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_noviz(file_name, DNS_MHD_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: DNS_MHD_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = file_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_ctl_noviz(DNS_MHD_ctl)
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_ctl_data(DNS_MHD_ctl)
!
      if(DNS_MHD_ctl%psph_ctl%ifile_sph_shell .gt. 0) then
        call read_ctl_file_gen_shell_grids(DNS_MHD_ctl%psph_ctl)
      end if
!
      end subroutine read_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_sph_MHD_psf
