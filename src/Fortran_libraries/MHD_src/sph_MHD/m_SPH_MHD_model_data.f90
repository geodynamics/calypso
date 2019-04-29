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
!!***********************************************************************
!!*
!!*     rot_e(k,j) : rotation of earth  (output)
!!*     rot_e(k,j) : d \Omega / dr
!!*     rot_e(k,j) : d^2 \Omega / dr^2
!!*
!!*                       1
!!*         rot_e(k,j) = --- r^2
!!*                       2
!!*
!!*                     dom(k,0)
!!*       drot_e(k,j) = ---------
!!*                        dr
!!*                   = r(k)
!!*
!!*                      dom(k,0)
!!*       d2rot_e(k,j) = ---------
!!*                         dr
!!*                    = 1.0
!!*
!!*        ref_temp%t_rj(kr,0) ... T_0
!!*        ref_temp%t_rj(kr,1) ... d T_0 / dr
!!*
!!***********************************************************************
!!@endverbatim
!
      module m_SPH_MHD_model_data
!
      use m_precision
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_FEM_mesh_field_data
      use t_work_SPH_MHD
      use t_viz_sections
      use t_SPH_MHD_zonal_mean_viz
!
      implicit  none
!
!
!>      Parameters for spectr dynamo model
      type(SPH_MHD_model_data), save :: SPH_model1
!
!>      Structure of spetr grid and data
      type(SPH_mesh_field_data), save :: SPH_MHD1
!
!>        Structures of work area for spherical shell dynamo
      type(work_SPH_MHD), save :: SPH_WK1
!
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save :: FEM_d1
!
!>      Structure of sectioning and isosurfaceing modules
      type(surfacing_modules), save :: viz_psfs1
!
!>      Structures of zonal mean controls
      type(sph_zonal_mean_sectioning), save :: zmeans1
!
      end module m_SPH_MHD_model_data
