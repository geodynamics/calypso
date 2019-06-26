!>@file   t_ctl_data_sph_MHD.f90
!!@brief  module t_ctl_data_sph_MHD
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
!!      subroutine read_control_4_sph_MHD(file_name, MHD_ctl)
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!@endverbatim
!
      module t_ctl_data_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use calypso_mpi
      use skip_comment_f
!
      implicit none
!
      end module t_ctl_data_sph_MHD
