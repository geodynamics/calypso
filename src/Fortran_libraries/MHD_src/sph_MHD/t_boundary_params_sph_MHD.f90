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
!!      subroutine alloc_fixed_bc_array(jmax, sph_bc)
!!      subroutine dealloc_fixed_bc_array(sph_bc)
!!
!!      subroutine cal_fdm_coefs_4_BCs(nri, radius, sph_bc)
!!      subroutine check_fdm_coefs_4_BC2(label, sph_bc)
!!      subroutine check_sph_boundary_spectra(label, jmax, j_rj, sph_bc)
!!        type(sph_boundary_type), intent(inout) :: sph_bc
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
!>      integer flag for fixed velocity boundary
      integer(kind = kint), parameter :: iflag_undefined_bc = -1
!>      integer flag for fixed velocity boundary
      integer(kind = kint), parameter :: iflag_fixed_field = 0
!>      integer flag for free-slip boundary
      integer(kind = kint), parameter :: iflag_fixed_flux =  1
!>      integer flag for whole sphere model
      integer(kind = kint), parameter :: iflag_sph_fill_center = 41
!>      integer flag for whole sphere model
      integer(kind = kint), parameter :: iflag_sph_fix_center =  42
!
!
!>      integer flag for fixed velocity boundary at inner core
      integer(kind = kint), parameter :: iflag_fixed_velo = 0
!>      integer flag for free-slip boundary at inner core
      integer(kind = kint), parameter :: iflag_free_slip =  1
!>      integer flag for rotatable inner core
      integer(kind = kint), parameter :: iflag_rotatable_ic = 10
!
!>      integer flag for insulated magnetic boundary
      integer(kind = kint), parameter :: iflag_sph_insulator =   0
!>      integer flag for pseudo vacuum magnetic boundary
      integer(kind = kint), parameter :: iflag_radial_magne =   11
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
!>        Fixed composition spectrum for ICB
        real(kind= kreal), pointer :: ICB_fld(:)
!>        Fixed composition flux spectrum for ICB
        real(kind= kreal), pointer :: ICB_flux(:)
!>        Fixed composition spectrum for CMB
        real(kind= kreal), pointer :: CMB_fld(:)
!>        Fixed composition flux spectrum for CMB
        real(kind= kreal), pointer :: CMB_flux(:)
!
!>        Matrix to evaluate radial derivative at ICB with fixed field
        real(kind = kreal) :: fdm2_fix_fld_ICB(0:2,3)
!>        Matrix to evaluate field at ICB with fixed radial derivative
        real(kind = kreal) :: fdm2_fix_dr_ICB(-1:1,3)
!>        Matrix to evaluate radial derivative at CMB with fixed field
        real(kind = kreal) :: fdm2_fix_fld_CMB(0:2,3)
!>        Matrix to evaluate field at CMB with fixed radial derivative
        real(kind = kreal) :: fdm2_fix_dr_CMB(-1:1,3)
!
!>        Matrix to evaluate radial derivative at ICB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_ICB(0:1,2)
!>        Matrix to evaluate radial derivative at CMB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_CMB(0:1,2)
      end type sph_boundary_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_fixed_bc_array(jmax, sph_bc)
!
      integer(kind= kint), intent(in) :: jmax
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      allocate(sph_bc%ICB_fld(jmax))
      allocate(sph_bc%CMB_fld(jmax))
      allocate(sph_bc%ICB_flux(jmax))
      allocate(sph_bc%CMB_flux(jmax))
      sph_bc%CTR_fld = 0.0d0
      sph_bc%ICB_fld = 0.0d0
      sph_bc%CMB_fld = 0.0d0
      sph_bc%ICB_flux = 0.0d0
      sph_bc%CMB_flux = 0.0d0
!
      end subroutine alloc_fixed_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_fixed_bc_array(sph_bc)
!
      type(sph_boundary_type), intent(inout) :: sph_bc
!
!
      deallocate(sph_bc%ICB_fld,  sph_bc%CMB_fld)
      deallocate(sph_bc%ICB_flux, sph_bc%CMB_flux)
!
      end subroutine dealloc_fixed_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm_coefs_4_BCs(nri, radius, sph_bc)
!
      use cal_fdm_coefs_4_boundaries
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius(nri)
      type(sph_boundary_type), intent(inout) :: sph_bc
!
!
      call cal_fdm1_coef_fix_fld_ICB(radius(sph_bc%kr_in),              &
     &     sph_bc%fdm1_fix_fld_ICB)
!
      call cal_fdm2_coef_fix_fld_ICB(radius(sph_bc%kr_in),              &
     &     sph_bc%fdm2_fix_fld_ICB)
      call cal_fdm2_coef_fix_df_ICB(radius(sph_bc%kr_in),               &
     &     sph_bc%fdm2_fix_dr_ICB)
!
!
      call cal_fdm1_coef_fix_fld_CMB(radius(sph_bc%kr_out-1),           &
     &     sph_bc%fdm1_fix_fld_CMB)
!
      call cal_fdm2_coef_fix_fld_CMB(radius(sph_bc%kr_out-2),           &
     &     sph_bc%fdm2_fix_fld_CMB)
      call cal_fdm2_coef_fix_df_CMB(radius(sph_bc%kr_out-1),            &
     &     sph_bc%fdm2_fix_dr_CMB)
!
      end subroutine cal_fdm_coefs_4_BCs
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coefs_4_BC2(label, sph_bc)
!
      character(len=kchara), intent(in) :: label
      type(sph_boundary_type), intent(in) :: sph_bc
!
!
      write(50,*) ' Boundary condition matrix for ', trim(label)
!
      write(50,*) ' fdm1_fix_fld_ICB'
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') sph_bc%fdm1_fix_fld_ICB(0:1,2)
!
      write(50,*) ' fdm2_fix_fld_ICB'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_fld_ICB(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_fld_ICB(0:2,3)
!
      write(50,*) ' fdm2_fix_dr_ICB'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_dr_ICB(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_dr_ICB(-1:1,3)
!
!
      write(50,*) ' fdm1_fix_fld_CMB'
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') sph_bc%fdm1_fix_fld_CMB(0:1,2)
!
      write(50,*) ' fdm2_fix_fld_CMB'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_fld_CMB(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_fld_CMB(0:2,3)
!
      write(50,*) ' fdm2_fix_dr_CMB'
      write(50,*) ' mat_fdm11,  mat_fdm12,  mat_fdm13'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_dr_CMB(-1:1,1)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') sph_bc%fdm2_fix_dr_CMB(-1:1,3)
!
      end subroutine check_fdm_coefs_4_BC2
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_boundary_spectra(label, jmax, j_rj, sph_bc)
!
      use m_spheric_parameter
      use m_bc_data_list
      use m_surf_data_list
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: j_rj(jmax,3)
      character(len=kchara), intent(in) :: label
      type(sph_boundary_type), intent(in) :: sph_bc
!
      integer(kind = kint) :: j
!
!
      write(50,*) ' Boundary condition spectra for ', trim(label)
!
      write(50,*) 'iflag_icb', sph_bc%iflag_icb
      if(sph_bc%iflag_icb .eq. iflag_fixed_field) then
        write(50,*) 'field at ICB '
        do j = 1, jmax
          write(50,*) j_rj(j,1:3), sph_bc%ICB_fld(j)
        end do
      end if
      if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        write(50,*) 'flux at ICB '
        do j = 1, jmax
          write(50,*) j_rj(j,1:3),  sph_bc%ICB_flux(j)
        end do
      end if
!
      write(50,*) 'iflag_cmb', sph_bc%iflag_cmb
      if(sph_bc%iflag_cmb .eq. iflag_fixed_field) then
        do j = 1, jmax
          write(50,*)  j_rj(j,1:3), sph_bc%CMB_fld(j)
        end do
      end if
      if(sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        write(50,*) 'flux at CMB '
        do j = 1, jmax
          write(50,*) j_rj(j,1:3), sph_bc%CMB_flux(j)
        end do
      end if
!
      end subroutine check_sph_boundary_spectra
!
! -----------------------------------------------------------------------
!
      end module t_boundary_params_sph_MHD
