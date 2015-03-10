!>@file   m_error_IDs.f90
!!@brief  module m_error_IDs
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief List of Error IDs
!
      module m_error_IDs
!
      use m_precision
!
      implicit none
!
!
!>     Error ID for MPI related
      integer(kind = kint), parameter :: ierr_P_MPI = 1
!
!>     Error ID for missing file
      integer(kind = kint), parameter :: ierr_file = 100
!>     Error ID for FEM mesh
      integer(kind = kint), parameter :: ierr_mesh = 101
!>     Error ID for linear solver
      integer(kind = kint), parameter :: ierr_CG =  102
!
!>     Error ID for Spherical harmonics transform method
      integer(kind = kint), parameter :: ierr_sph =  111
!>     Error ID for FEM
      integer(kind = kint), parameter :: ierr_FEM =  112
!
!>     Error ID for field definition
      integer(kind = kint), parameter :: ierr_fld = 201
!>     Error ID for field definition
      integer(kind = kint), parameter :: ierr_BC =  211
!
!>     Error ID for force definition
      integer(kind = kint), parameter :: ierr_force = 251
!
!>     Error ID for normalization
      integer(kind = kint), parameter :: ierr_dless = 301
!
!>     Error ID for Time integration
      integer(kind = kint), parameter :: ierr_evo = 401
!
!>     Error ID for SGS model
      integer(kind = kint), parameter :: ierr_SGS = 501
!
!>     Error ID in visualizer module
      integer(kind = kint), parameter :: ierr_VIZ = 601
!
!>     Error ID in PVR module
      integer(kind = kint), parameter :: ierr_PVR = 621
!
!
!
!>     Error ID for FEM element
      integer(kind = kint), parameter :: ierr_ele =   1001
!>     Error ID for FEM number of groups
      integer(kind = kint), parameter :: ierr_ngrp =  1002
!>     Error ID for FEM grouping
      integer(kind = kint), parameter :: ierr_grp =   1003
!>     Error ID for FEM element type
      integer(kind = kint), parameter :: ierr_etype = 1004
!>     Error ID for FEM element connectivity
      integer(kind = kint), parameter :: ierr_econ =  1005
!
!>     Error ID for overflow of FEM node
      integer(kind = kint), parameter :: ierr_ovnod =   2001
!>     Error ID for overflow of FEM group
      integer(kind = kint), parameter :: ierr_ov_grp =  2002
!>     Error ID for surface group
      integer(kind = kint), parameter :: ierr_sf_grp =  2003
!
!>     Error ID for MeTiS input
      integer(kind = kint), parameter :: ierr_MeTISfile =  7001
!>     Error ID for MeTiS input
      integer(kind = kint), parameter :: ierr_MeTISData =  7002

      end module m_error_IDs
