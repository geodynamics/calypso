!>@file   t_sph_MHD_w_vizs.f90
!!@brief  module t_sph_MHD_w_vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Structures for visualizers in spherical MHD dynamo
      module t_sph_MHD_w_vizs
!
      use m_precision
!
      use t_FEM_mesh_field_data
      use t_SPH_MHD_zonal_mean_viz
      use t_four_visualizers
      use t_VIZ_mesh_field
!
      implicit none
!
!>      Structure for visualization in spherical MHD
      type sph_MHD_w_vizs
!>        Structure of FEM mesh and field structures
        type(FEM_mesh_field_data) :: FEM_DAT
!>        Structure of geometry informations for visualization
        type(VIZ_mesh_field) :: VIZ_DAT
!>        Structure of sectioning and isosurfaceing modules
        type(four_visualize_modules) :: VIZ4s
!>        Structures of zonal mean controls
        type(sph_zonal_mean_viz) :: zmeans
      end type sph_MHD_w_vizs
!
      end module t_sph_MHD_w_vizs
