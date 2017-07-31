!>@file   m_ctl_data_sph_MHD.f90
!!@brief  module m_ctl_data_sph_MHD
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
!
      module m_ctl_data_sph_MHD
!
      use m_precision
!
      use t_ctl_data_MHD
!
      implicit none
!
!
!
      integer(kind=kint), parameter :: control_file_code = 11
!
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!>      Control struture for MHD simulation
      type(DNS_mhd_simulation_control), save :: DNS_MHD_ctl1
!
      end module m_ctl_data_sph_MHD
