!>@file   t_SPH_mesh_field_data.f90
!!@brief  module t_SPH_mesh_field_data
!!
!!@author H. Matsui
!!@date Programmed on Sep., 2017
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@n @param   l          Sphrical harmonics degree
!!@n @param   m          Sphrical harmonics order
!!
!!@verbatim
!!      integer(kind = kint) function find_local_sph_mode_address(l, m)
!!        j_lc = find_local_sph_mode_address(l, m)
!!          Return local spherical harmonics mode address j_lc for Y(l,m)
!!          If requested mode does not exist in the process, 0 is set
!!      integer(kind = kint) function local_sph_data_address(kr, j_lc)
!!        inod = local_sph_data_address(k, j_lc)
!!          Return address of sphectrum data
!!        inod = inod_rj_center()
!!          If spectrum data have center, inod_rj_center 
!!          returns this address.
!!
!!      real(kind = kreal) function radius_1d_rj_r(kr)
!!        nidx_rj(1) :: Number of radial grids
!!        rr = radius_1d_rj_r(k)
!!          Return radius at global grid address k
!!
!!      real(kind = kreal) function r_ICB()
!!      real(kind = kreal) function r_CMB()
!!      integer(kind = kint) function nlayer_ICB()
!!      integer(kind = kint) function nlayer_CMB()
!!      integer(kind = kint) function inod_rj_center()
!!      integer(kind = kint) function nidx_rj(nd)
!!        nidx_rj(1) :: Number of radial grids
!!        nlayer_ICB() :: radial ID for ICB
!!        nlayer_CMB() :: radial ID for CMB
!!        r_ICB() :: ICB radius
!!        r_CMB() :: CMB radius
!!@endverbatim
!
!
      module t_SPH_mesh_field_data
!
      use m_precision
      use m_spheric_constants
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_phys_address
      use t_phys_data
!
      implicit none
!
!
!
!> Structure of spherical transform mesh information
      type sph_mesh_data
!>         spherical harmonics indexing data
        type(sph_grids) ::       sph
!>         communication tables for spherical transform
        type(sph_comm_tables) :: sph_comms
!>         grouping data for harmonics indices
        type(sph_group_data) ::  sph_grps
      end type sph_mesh_data
!
!
!>      Structure of spetr grid and data
      type SPH_mesh_field_data
!>         Structure of grid and spectr data for spherical spectr method
        type(sph_grids) :: sph
!>        Structure for communication table for spherical transform
        type(sph_comm_tables) :: comms
!>        Structure for grid and comm table for spherical transform
        type(sph_group_data) :: groups
!
!
!>        address for spectr data (poloidal component for vector)
        type(phys_address) :: ipol
!>        address for radial gradient for poloidal component
        type(phys_address) :: idpdr
!>        address for toroidal component
        type(phys_address) :: itor
!
!>        Structure for field data
        type(phys_data) :: fld
      end type SPH_mesh_field_data
!
!
      end module t_SPH_mesh_field_data
