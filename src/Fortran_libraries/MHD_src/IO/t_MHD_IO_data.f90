!>@file   t_MHD_IO_data.f90
!!@brief  module t_MHD_IO_data
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module t_MHD_IO_data
!
!
      use m_precision
!
      use t_MHD_file_parameter
      use t_field_data_IO
      use t_ucd_file
      use t_cal_max_indices
!
      implicit  none
!
!
      type MHD_IO_data
        type(field_IO) :: rst_IO
!
        type(ucd_file_data) :: fem_ucd
!
        type(maximum_informations) :: range
      end type MHD_IO_data
!
      end module t_MHD_IO_data
