!>@file   t_spherical_MHD.f90
!!@brief  module t_spherical_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Structures for spherical MHD dynamo model
      module t_spherical_MHD
!
      use m_precision
!
      use t_SPH_MHD_model_data
      use t_MHD_step_parameter
      use t_SPH_mesh_field_data
      use t_MHD_file_parameter
      use t_MHD_IO_data
      use t_work_SPH_MHD
      use t_mesh_SR
!
      implicit none
!
!>      Structure of the all data of program
      type spherical_MHD
!>        Parameters for spectr dynamo model
        type(SPH_MHD_model_data) :: SPH_model
!
!>        Structure of time and step informations
        type(MHD_step_param) :: MHD_step
!>        Structure of spectr grid and data
        type(SPH_mesh_field_data) :: SPH_MHD
!
!>        Structures of work area for spherical shell dynamo
        type(work_SPH_MHD) :: SPH_WK
!>        Structure of work area for mesh communications
        type(mesh_SR) :: m_SR
!
!>        Structure of file name and format for MHD
        type(MHD_file_IO_params) :: MHD_files
!>        Structure for data file IO
        type(MHD_IO_data) :: MHD_IO
      end type spherical_MHD
!
      end module t_spherical_MHD
