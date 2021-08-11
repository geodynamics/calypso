!>@file   m_SPH_MHD_model_data.f90
!!@brief  module m_SPH_MHD_model_data
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief Instace of Structures for spherical shell dynamo
!!
!!
!!@verbatim
!!@endverbatim
!
      module m_SPH_MHD_model_data
!
      use m_precision
      use t_SPH_MHD_model_data
      use t_FEM_mesh_field_data
      use t_work_SPH_MHD
      use t_SPH_MHD_zonal_mean_viz
      use t_mesh_SR
!
      implicit  none
!
!
!>      Parameters for spectr dynamo model
      type(SPH_MHD_model_data), save :: SPH_model1
!
!
!>        Structures of work area for spherical shell dynamo
      type(work_SPH_MHD), save :: SPH_WK1
!
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save :: FEM_d1
!>      Structure of work area for mesh communications
      type(mesh_SR), save :: m_SR1
!
!>      Structures of zonal mean controls
      type(sph_zonal_mean_sectioning), save :: zmeans1
!
      end module m_SPH_MHD_model_data
