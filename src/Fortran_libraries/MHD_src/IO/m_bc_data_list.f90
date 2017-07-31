!>@file   m_bc_data_list.f90
!!@brief  module m_bc_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief  Boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!@endverbatim
!
      module m_bc_data_list
!
      use m_precision
      use t_bc_data_list
!
      implicit  none
!
!
!>      Structure for boundary condition lists for MHD
      type(MHD_BC_lists), save :: MHD_BC1
!
      end module m_bc_data_list
