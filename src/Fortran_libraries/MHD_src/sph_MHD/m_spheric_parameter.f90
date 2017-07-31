!>@file   m_spheric_parameter.f90
!!@brief  module m_spheric_parameter
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@n @param  my_rank     Running rank ID
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
      module m_spheric_parameter
!
      use m_precision
      use m_spheric_constants
      use t_spheric_mesh
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!.
      implicit none
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph1
!
!>  Structure for communication table for spherical transform
      type(sph_comm_tables), save :: comms_sph1
!
!> Structure for grid and comm table for spherical transform
      type(sph_group_data), save :: sph_grps1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!!   Wrapper routines from mesh strucutres. Do not edit.
!-----------------------------------------------------------------------
!
      integer(kind = kint) function find_local_sph_mode_address(l, m)
!
      integer(kind = 4), intent(in) :: l, m
!
!
      find_local_sph_mode_address                                       &
     &      = find_local_sph_address(sph1%sph_rj, l, m)
!
      end function find_local_sph_mode_address
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function local_sph_data_address(kr, j_lc)
!
      integer(kind = kint), intent(in) :: kr, j_lc
!
!
      local_sph_data_address                                            &
     &      = local_sph_node_address(sph1%sph_rj, kr, j_lc)
!
      end function local_sph_data_address
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function radius_1d_rj_r(kr)
!
      integer(kind = kint), intent(in) :: kr
!
      radius_1d_rj_r = sph1%sph_rj%radius_1d_rj_r(kr)
!
      end function radius_1d_rj_r
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function r_CMB()
!
      r_CMB = sph1%sph_params%radius_CMB
!
      end function r_CMB
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function r_ICB()
!
      r_ICB = sph1%sph_params%radius_ICB
!
      end function r_ICB
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nlayer_CMB()
!
      nlayer_CMB = sph1%sph_params%nlayer_CMB
!
      end function nlayer_CMB
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nlayer_ICB()
!
      nlayer_ICB = sph1%sph_params%nlayer_ICB
!
      end function nlayer_ICB
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function inod_rj_center()
!
      inod_rj_center = sph1%sph_rj%inod_rj_center
!
      end function inod_rj_center
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nidx_rj(nd)
!
      integer(kind = kint), intent(in) :: nd
!
      nidx_rj = sph1%sph_rj%nidx_rj(nd)
!
      end function nidx_rj
!
!-----------------------------------------------------------------------
!
      end module m_spheric_parameter
