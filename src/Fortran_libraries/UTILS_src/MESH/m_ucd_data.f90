!>@file   m_ucd_data.f90
!!@brief  module m_ucd_data
!!
!!@author H. Matsui
!!@date  Programmed in July 2000 (ver 1.1)
!!@n      Modified in Aug., 2007
!!@n      Modified in Dec., 2015
!
!>@brief Strucures for field data output
!!
      module m_ucd_data
!
      use m_precision
      use m_constants
!
      use t_ucd_file
!
      implicit none
!
!
      type(ucd_file_data), save :: fem_ucd1
!
      end module m_ucd_data
