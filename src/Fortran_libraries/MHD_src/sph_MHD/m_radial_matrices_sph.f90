!>@file   m_radial_matrices_sph.f90
!!@brief  module m_radial_matrices_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief Radial band matrix for time evolutions
!!
      module m_radial_matrices_sph
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrices
      use t_radial_matrices_sph_MHD
!
      implicit none
!
!
!
!>        Structure of band matrices for dynamo simulation
      type(MHD_radial_matrices), save :: sph_MHD_mat1
!
!
      end module m_radial_matrices_sph
