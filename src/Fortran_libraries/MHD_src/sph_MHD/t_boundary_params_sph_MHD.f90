!>@file   t_boundary_params_sph_MHD.f90
!!@brief  module t_boundary_params_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine cal_fdm_coefs_4_BCs(sph_rj, sph_bc)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(inout) :: sph_bc
!!      subroutine check_fdm_coefs_4_BC2(id_file, label, sph_bc)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: label
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!!@n @param j_rj    local spherical harmionics modes for f(r,j)
!
      module t_boundary_params_sph_MHD
!
      use m_precision
!
      implicit none
!
!
!>      integer flag for undefined boundary
      integer(kind = kint), parameter :: iflag_undefined_bc = -1
!>      integer flag for fixed boundary
      integer(kind = kint), parameter :: iflag_fixed_field = 0
!>      integer flag for fixed flux boundary
      integer(kind = kint), parameter :: iflag_fixed_flux =  1
!>      integer flag for whole sphere model
      integer(kind = kint), parameter :: iflag_sph_fill_center =   41
!>      integer flag for fixed value at center
      integer(kind = kint), parameter :: iflag_sph_fix_center =    42
!>      integer flag for filtering toward center
      integer(kind = kint), parameter :: iflag_sph_filter_center = 43
!>      integer flag for fixed drifting boundary
      integer(kind = kint), parameter :: iflag_evolve_field =      50
!>      integer flag for fixed drifting flux
      integer(kind = kint), parameter :: iflag_evolve_flux =       51
!
!
!>      integer flag for fixed velocity boundary at inner core
      integer(kind = kint), parameter :: iflag_fixed_velo = 100
!>      integer flag for free-slip boundary at inner core
      integer(kind = kint), parameter :: iflag_free_slip =  101
!>      integer flag for rotatable inner core
      integer(kind = kint), parameter :: iflag_rotatable_ic = 110
!
!>      integer flag for insulated magnetic boundary
      integer(kind = kint), parameter :: iflag_sph_insulator =  200
!>      integer flag for pseudo vacuum magnetic boundary
      integer(kind = kint), parameter :: iflag_radial_magne =   201
!
!
!>      Structure for basic boundary condition parameters
      type sph_boundary_type
!>        boundary condition flag at ICB
        integer(kind = kint) :: iflag_icb = iflag_undefined_bc
!>        boundary condition flag at CMB
        integer(kind = kint) :: iflag_cmb = iflag_undefined_bc
!
!>        Start radial group name of fluid shell for @f$ f(r,j) @f$
        character(len = kchara) :: icb_grp_name
!>        End radial group name of fluid shell for @f$ f(r,j) @f$
        character(len = kchara) :: cmb_grp_name
!
!>        Start radial address of fluid shell for @f$ f(r,j) @f$
        integer(kind = kint) :: kr_in =   1
!>        End radial address of fluid shell for @f$ f(r,j) @f$
        integer(kind = kint) :: kr_out =  1
!
!>        Radius at ICB
!!         (r(0) = r_ICB, r(1) = 1/r_ICB, and r(2) = 1/r_ICB^2)
        real(kind= kreal) :: r_ICB(0:2)
!>        radius at CMB
!!         (r(0) = r_CMB, r(1) = 1/r_CMB, and r(2) = 1/r_ICB^2)
        real(kind= kreal) :: r_CMB(0:2)
!
!>        Fixed data at center
        real(kind= kreal) :: CTR_fld
!
!>        Matrix to evaluate radial derivative at ICB with fixed field
        real(kind = kreal) :: fdm2_fix_fld_ICB(0:2,3)
!>        Matrix to evaluate field at ICB with fixed radial derivative
        real(kind = kreal) :: fdm2_fix_dr_ICB(-1:1,3)
!>        Matrix to evaluate radial derivative at CMB with fixed field
        real(kind = kreal) :: fdm2_fix_fld_CMB(-2:0,3)
!>        Matrix to evaluate field at CMB with fixed radial derivative
        real(kind = kreal) :: fdm2_fix_dr_CMB(-1:1,3)
      end type sph_boundary_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm_coefs_4_BCs(sph_rj, sph_bc)
!
      use t_spheric_rj_data
      use t_coef_fdm2_scalar_ICB
      use t_coef_fdm2_scalar_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      integer(kind = kint) :: kr_st
!
      kr_st = sph_bc%kr_in
      call cal_fdm2_coef_fix_fld_ICB(sph_rj%radius_1d_rj_r(kr_st),      &
     &                               sph_bc%fdm2_fix_fld_ICB)
      call cal_fdm2_coef_fix_df_ICB(sph_rj%radius_1d_rj_r(kr_st),       &
     &                              sph_bc%fdm2_fix_dr_ICB)
!
      kr_st = sph_bc%kr_out-2
      call cal_fdm2_coef_fix_fld_CMB(sph_rj%radius_1d_rj_r(kr_st),      &
     &                               sph_bc%fdm2_fix_fld_CMB)
      kr_st = sph_bc%kr_out-1
      call cal_fdm2_coef_fix_df_CMB(sph_rj%radius_1d_rj_r(kr_st),       &
     &                              sph_bc%fdm2_fix_dr_CMB)
!
      end subroutine cal_fdm_coefs_4_BCs
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coefs_4_BC2(id_file, label, sph_bc)
!
      use t_coef_fdm2_scalar_ICB
      use t_coef_fdm2_scalar_CMB
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label
      type(sph_boundary_type), intent(in) :: sph_bc
!
!
      write(id_file,*) ' Boundary condition matrix for ', trim(label)
!
      call check_fdm2_coefs_ICB(id_file, sph_bc%fdm2_fix_fld_ICB,       &
     &                                   sph_bc%fdm2_fix_dr_ICB)
      call check_fdm2_coefs_CMB(id_file, sph_bc%fdm2_fix_fld_CMB,       &
     &                          sph_bc%fdm2_fix_dr_CMB)
!
      end subroutine check_fdm_coefs_4_BC2
!
! -----------------------------------------------------------------------
!
      end module t_boundary_params_sph_MHD
