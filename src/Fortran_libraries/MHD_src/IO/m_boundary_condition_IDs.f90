!m_boundary_condition_IDs.f90
!     module m_boundary_condition_IDs
!
!     Written by H. Matsui on Feb., 2009
!
      module m_boundary_condition_IDs
!
      use m_precision
!
      implicit  none
!
!
!
      integer(kind = kint), parameter :: iflag_bc_fixed =   0
      integer(kind = kint), parameter :: iflag_bc_fix_s =   1
      integer(kind = kint), parameter :: iflag_bc_fix_x =   1
      integer(kind = kint), parameter :: iflag_bc_fix_y =   2
      integer(kind = kint), parameter :: iflag_bc_fix_z =   3
      integer(kind = kint), parameter :: iflag_bc_file_x = -1
      integer(kind = kint), parameter :: iflag_bc_file_y = -2
      integer(kind = kint), parameter :: iflag_bc_file_z = -3
!
      integer(kind = kint), parameter :: iflag_bc_fix_flux = 11
!
      integer(kind = kint), parameter :: iflag_bc_sgs =            4
      integer(kind = kint), parameter :: iflag_bc_sgs_s =          5
      integer(kind = kint), parameter :: iflag_bc_sgs_x =          5
      integer(kind = kint), parameter :: iflag_bc_sgs_y =          6
      integer(kind = kint), parameter :: iflag_bc_sgs_z =          7
!
      integer(kind = kint), parameter :: iflag_bc_rot =   10
      integer(kind = kint), parameter :: iflag_bc_rot_x = 11
      integer(kind = kint), parameter :: iflag_bc_rot_y = 12
      integer(kind = kint), parameter :: iflag_bc_rot_z = 13
!
      integer(kind = kint), parameter :: iflag_free_sph =        100
      integer(kind = kint), parameter :: iflag_non_slip_sph =    200
      integer(kind = kint), parameter :: iflag_no_vr =           200
      integer(kind = kint), parameter :: iflag_rotatable_icore = 301
!
      integer(kind = kint), parameter :: iflag_insulator =       100
      integer(kind = kint), parameter :: iflag_sph_2_center =    200
!
      integer(kind = kint), parameter :: iflag_pseudo_vacuum =   400
!
      integer(kind = kint), parameter :: iflag_bc_special =      999
!
!
!
      integer(kind = kint), parameter :: iflag_fixed_grad =      0
      integer(kind = kint), parameter :: iflag_fixed_grad_s =    1
      integer(kind = kint), parameter :: iflag_fixed_grad_x =    1
      integer(kind = kint), parameter :: iflag_fixed_grad_y =    2
      integer(kind = kint), parameter :: iflag_fixed_grad_z =    3
!
      integer(kind = kint), parameter :: iflag_fixed_norm =   10
!
      integer(kind = kint), parameter :: iflag_surf_fixed =   10
      integer(kind = kint), parameter :: iflag_surf_fix_s =   11
      integer(kind = kint), parameter :: iflag_surf_fix_x =   11
      integer(kind = kint), parameter :: iflag_surf_fix_y =   12
      integer(kind = kint), parameter :: iflag_surf_fix_z =   13
!
      integer(kind = kint), parameter :: iflag_bc_sgs_commute =   15
      integer(kind = kint), parameter :: iflag_bc_sgs_commute_s=  16
      integer(kind = kint), parameter :: iflag_bc_sgs_commute_x=  16
      integer(kind = kint), parameter :: iflag_bc_sgs_commute_y=  17
      integer(kind = kint), parameter :: iflag_bc_sgs_commute_z=  18
!
      integer(kind = kint), parameter :: iflag_lead_grad =       100
      integer(kind = kint), parameter :: iflag_lead_grad_s =     101
      integer(kind = kint), parameter :: iflag_lead_grad_x =     101
      integer(kind = kint), parameter :: iflag_lead_grad_y =     102
      integer(kind = kint), parameter :: iflag_lead_grad_z =     103
!
      integer(kind = kint), parameter :: iflag_surf_wall =      70
      integer(kind = kint), parameter :: iflag_surf_sph_in =    71
      integer(kind = kint), parameter :: iflag_surf_sph_out =   72
!
      integer(kind = kint), parameter :: iflag_surf_infty =      0

!
      end module m_boundary_condition_IDs
