!>@file   m_boundary_data_sph_MHD.f90
!!@brief  module m_boundary_data_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!
      module m_boundary_data_sph_MHD
!
      use m_precision
      use t_boundary_data_sph_MHD
!
      implicit none
!
!
      type(sph_MHD_boundary_data), save :: sph_MHD_bc1
!
      end module m_boundary_data_sph_MHD
