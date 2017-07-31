!>@file   m_normalize_parameter.f90
!!@brief  module m_normalize_parameter
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!>@brief  dimensionless number list for each term
!!
      module m_normalize_parameter
!
      use m_precision
      use t_normalize_parameter
!
      implicit  none
!
!
      type(coef_parameters_list), save :: MHD_coef_list
!
      end module m_normalize_parameter
