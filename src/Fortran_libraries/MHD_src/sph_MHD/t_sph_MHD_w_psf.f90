!>@file   t_sph_MHD_w_psf.f90
!!@brief  module t_sph_MHD_w_psf
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Structures for visualizers in spherical MHD dynamo
      module t_sph_MHD_w_psf
!
      use m_precision
!
      use t_FEM_mesh_field_data
      use t_SPH_MHD_zonal_mean_viz
      use t_viz_sections
      use t_comm_table
!
      implicit none
!
!>      Structure for visualization in spherical MHD
      type sph_MHD_w_psf
!>        Structure of FEM mesh and field structures
        type(FEM_mesh_field_data) :: FEM_DAT
!>        Structure of edge communication table
        type(communication_table) :: edge_comm
!>        Structure of sectioning and isosurfaceing modules
        type(surfacing_modules) :: PSFs
!>        Structures of zonal mean controls
        type(sph_zonal_mean_sectioning) :: zmeans
      end type sph_MHD_w_psf
!
      end module t_sph_MHD_w_psf
