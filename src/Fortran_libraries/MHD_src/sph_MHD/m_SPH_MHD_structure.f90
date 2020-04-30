!>@file   m_SPH_MHD_structure.f90
!!@brief  module m_SPH_MHD_structure
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!@endverbatim
!
      module m_SPH_MHD_structure
!
      use m_precision
      use t_SPH_mesh_field_data
      use t_viz_sections
!
      implicit none
!
!
!>      Structure of spetr grid and data
      type(SPH_mesh_field_data), save :: SPH_MHD1
!
!>      Structure of sectioning and isosurfaceing modules
      type(surfacing_modules), save :: viz_psfs1
!
      end module m_SPH_MHD_structure
