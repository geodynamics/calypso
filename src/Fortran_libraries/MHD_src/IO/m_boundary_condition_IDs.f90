!>@file   m_boundary_condition_IDs.f90
!!@brief  module m_boundary_condition_IDs
!!
!!@author H. Matsui and H. Okuda
!!@date Modified by H. Matsui in Feb., 2009
!
!> @brief boundary condition flag list
!
      module m_boundary_condition_IDs
!
      use m_precision
!
      implicit  none
!
!
!>      base flag for fixed boundary
      integer(kind = kint), parameter :: iflag_bc_fixed =   0
!>      fixed scalar defined by control data
      integer(kind = kint), parameter :: iflag_bc_fix_s =   1
!>      fixed x-component defined by control data
      integer(kind = kint), parameter :: iflag_bc_fix_x =   1
!>      fixed y-component defined by control data
      integer(kind = kint), parameter :: iflag_bc_fix_y =   2
!>      fixed z-component defined by control data
      integer(kind = kint), parameter :: iflag_bc_fix_z =   3
!>      fixed scalar defined by external file
      integer(kind = kint), parameter :: iflag_bc_file_s = -1
!>      fixed x-component defined by external file
      integer(kind = kint), parameter :: iflag_bc_file_x = -1
!>      fixed y-component defined by external file
      integer(kind = kint), parameter :: iflag_bc_file_y = -2
!>      fixed z-component defined by external file
      integer(kind = kint), parameter :: iflag_bc_file_z = -3
!
!>      flag for fixed flux
      integer(kind = kint), parameter :: iflag_bc_fix_flux =   11
!>      flag for fixed flux by external file
      integer(kind = kint), parameter :: iflag_bc_file_flux = -11
!
!>      base flag for fixed boundary with SGS model
      integer(kind = kint), parameter :: iflag_bc_sgs =            5
!>      fixed scalar defined by control data with SGS model
      integer(kind = kint), parameter :: iflag_bc_sgs_s =          6
!>      fixed x-component defined by control data with SGS model
      integer(kind = kint), parameter :: iflag_bc_sgs_x =          6
!>      fixed y-component defined by control data with SGS model
      integer(kind = kint), parameter :: iflag_bc_sgs_y =          7
!>      fixed z-component defined by control data with SGS model
      integer(kind = kint), parameter :: iflag_bc_sgs_z =          8
!
!>      fixed rotation boundary
      integer(kind = kint), parameter :: iflag_bc_rot =   10
!>      fixed rotation around x-axis boundary
      integer(kind = kint), parameter :: iflag_bc_rot_x = 11
!>      fixed rotation around y-axis boundary
      integer(kind = kint), parameter :: iflag_bc_rot_y = 12
!>      fixed rotation around z-axis boundary
      integer(kind = kint), parameter :: iflag_bc_rot_z = 13
!
!>      free slip boundary
      integer(kind = kint), parameter :: iflag_free_sph =        100
!>      non slip boundary
      integer(kind = kint), parameter :: iflag_non_slip_sph =    200
!>      eliminate radial flow
      integer(kind = kint), parameter :: iflag_no_vr =           201
!>      rotatable inner core
      integer(kind = kint), parameter :: iflag_rotatable_icore = 301
!
!>      insulated magnetic boundary
      integer(kind = kint), parameter :: iflag_insulator =       100
!>      boundary to connect center field
      integer(kind = kint), parameter :: iflag_sph_2_center =    501
!>      boundary to fix center field
      integer(kind = kint), parameter :: iflag_sph_clip_center = 502
!
!>      pseudo vacuum boundary
      integer(kind = kint), parameter :: iflag_pseudo_vacuum =   400
!
!>      flag for special boundary condition
      integer(kind = kint), parameter :: iflag_bc_special =      999
!
!
!>      base flag for fixed normal gradient
      integer(kind = kint), parameter :: iflag_fixed_grad =      10
!>      fixed normal gradient of scalar
      integer(kind = kint), parameter :: iflag_fixed_grad_s =    11
!>      fixed normal gradient of x-component
      integer(kind = kint), parameter :: iflag_fixed_grad_x =    11
!>      fixed normal gradient of y-component
      integer(kind = kint), parameter :: iflag_fixed_grad_y =    12
!>      fixed normal gradient of z-component
      integer(kind = kint), parameter :: iflag_fixed_grad_z =    13
!
!>      base flag for fixed normal field
      integer(kind = kint), parameter :: iflag_fixed_norm =   20
!
!>      base flag to evaluate normal gradient
      integer(kind = kint), parameter :: iflag_lead_grad =       100
!>      evaluate normal gradient for scalar
      integer(kind = kint), parameter :: iflag_lead_grad_s =     101
!>      evaluate normal gradient for x-component
      integer(kind = kint), parameter :: iflag_lead_grad_x =     101
!>      evaluate normal gradient for y-component
      integer(kind = kint), parameter :: iflag_lead_grad_y =     102
!>      evaluate normal gradient for z-component
      integer(kind = kint), parameter :: iflag_lead_grad_z =     103
!
!>      surface flag for reflectio
      integer(kind = kint), parameter :: iflag_surf_wall =      70
!>      surface flag for inner boundary of spheical shell
      integer(kind = kint), parameter :: iflag_surf_sph_in =    71
!>      surface flag for outer boundary of spheical shell
      integer(kind = kint), parameter :: iflag_surf_sph_out =   72
!
!>      surface flag for free slip on outer spherical boundary
      integer(kind = kint), parameter :: iflag_surf_free_sph_in =  401
!>      surface flag for free slip on inner spherical boundary
      integer(kind = kint), parameter :: iflag_surf_free_sph_out = 402
!
!>      surface flag for pseudo vacuum on outer spherical boundary
      integer(kind = kint), parameter :: iflag_surf_qvc_sph_in =   401
!>      surface flag for pseudo vacuum on inner spherical boundary
      integer(kind = kint), parameter :: iflag_surf_qvc_sph_out =  402
!
!>      surface flag for infinite radius
      integer(kind = kint), parameter :: iflag_surf_infty =      0
!
      end module m_boundary_condition_IDs
