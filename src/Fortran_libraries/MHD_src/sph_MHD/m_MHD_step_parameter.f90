!>@file   m_MHD_step_parameter.f90
!!@brief  module m_MHD_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module m_MHD_step_parameter
!
!
      use m_precision
!
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_sph_boundary_input_data
!
      implicit  none
!
!
      type(MHD_step_param), save :: MHD_step1
!
!>      Structure of file name and format for MHD
      type(MHD_file_IO_params), save :: MHD_files1
!>      Structures for boundary conditions
      type(boundary_spectra), save :: bc_sph_IO1
!
      end module m_MHD_step_parameter
