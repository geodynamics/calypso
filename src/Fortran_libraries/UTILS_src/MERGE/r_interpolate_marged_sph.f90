!>@file   r_interpolate_marged_sph.f90
!!@brief  module r_interpolate_marged_sph
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief Radial interpolation for assemble program
!!
!!@verbatim
!!      subroutine const_r_interpolate_table(org_sph, new_sph, r_itp)
!!        type(sph_grids), intent(in) :: org_sph
!!        type(sph_grids), intent(inout) :: new_sph
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!      subroutine const_ICB_and_CMB_radius(org_sph_grps, new_sph_grps, &
!!     &          nlayer_ICB_org, nlayer_CMB_org,                       &
!!     &          nlayer_ICB_new, nlayer_CMB_new)
!!       type(sph_group_data), intent(in) :: org_sph_grps
!!        type(sph_group_data), intent(in) :: new_sph_grps
!!@endverbatim
!!
!!@param   nnod_org  Number of spectr data for original data
!!@param   nri_org   Number of radial grid for original data
!!@param   jmax_org  Number of harmonics modes for original data
!!@param   r_org     Position of radial grid for original data
!!@param   idx_gl_1d_j_org(jmax_org,3)
!!                  List of spherical harmonics modes for original data
!!@param   d_rj_IO   Read harmonics data data
!
      module r_interpolate_marged_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_spheric_parameter
      use t_spheric_group
      use t_sph_radial_interpolate
!
      implicit none
!
      private :: sph_radial_interpolation_coef
      private :: set_sph_boundary_4_merge
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_r_interpolate_table(org_sph, new_sph, r_itp)
!
      use calypso_mpi_int
!
      type(sph_grids), intent(in) :: org_sph
!
      type(sph_grids), intent(inout) :: new_sph
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      if(my_rank .eq. 0) then
        call sph_radial_interpolation_coef                              &
     &     (org_sph%sph_rj%nidx_rj(1), org_sph%sph_rj%radius_1d_rj_r,   &
     &      new_sph%sph_rj%nidx_rj(1), new_sph%sph_rj%radius_1d_rj_r,   &
     &      r_itp)
      end if
      call calypso_mpi_bcast_one_int                                    &
     &   (new_sph%sph_rj%nidx_rj(1), 0)
      call share_r_interpolation_tbl(new_sph%sph_rj%nidx_rj(1), r_itp)
!
      end subroutine const_r_interpolate_table
!
! -----------------------------------------------------------------------
!
      subroutine const_ICB_and_CMB_radius(org_sph_grps, new_sph_grps,   &
     &          nlayer_ICB_org, nlayer_CMB_org,                         &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
      type(sph_group_data), intent(in) :: org_sph_grps
      type(sph_group_data), intent(in) :: new_sph_grps
!
      integer(kind = kint), intent(inout) :: nlayer_ICB_org
      integer(kind = kint), intent(inout) :: nlayer_CMB_org
      integer(kind = kint), intent(inout) :: nlayer_ICB_new
      integer(kind = kint), intent(inout) :: nlayer_CMB_new
!
!
      if(my_rank .eq. 0) then
        call set_sph_boundary_4_merge                                   &
     &     (org_sph_grps, nlayer_ICB_org, nlayer_CMB_org)
        call set_sph_boundary_4_merge                                   &
     &     (new_sph_grps, nlayer_ICB_new, nlayer_CMB_new)
      end if
!
      call share_ICB_and_CMB_radius(nlayer_ICB_org, nlayer_CMB_org,     &
     &   nlayer_ICB_new, nlayer_CMB_new)
!
      end subroutine const_ICB_and_CMB_radius
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_boundary_4_merge(sph_grps,                     &
     &          nlayer_ICB, nlayer_CMB)
!
      use t_spheric_parameter
      use skip_comment_f
!
      type(sph_group_data), intent(in) ::  sph_grps
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
!
      integer(kind = kint) :: k, kk
      character(len = kchara) :: tmpchara
!
!
      do k = 1, sph_grps%radial_rj_grp%num_grp
        tmpchara = sph_grps%radial_rj_grp%grp_name(k)
        if(cmp_no_case(tmpchara, ICB_nod_grp_name)) then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_ICB = sph_grps%radial_rj_grp%item_grp(kk)
        else if(cmp_no_case(tmpchara, CMB_nod_grp_name)) then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_CMB = sph_grps%radial_rj_grp%item_grp(kk)
        end if
      end do
!
      end subroutine set_sph_boundary_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine share_ICB_and_CMB_radius                               &
     &         (nlayer_ICB_org, nlayer_CMB_org,                         &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(inout) :: nlayer_ICB_org
      integer(kind = kint), intent(inout) :: nlayer_CMB_org
      integer(kind = kint), intent(inout) :: nlayer_ICB_new
      integer(kind = kint), intent(inout) :: nlayer_CMB_new
!
!
      call calypso_mpi_bcast_one_int(nlayer_ICB_org, 0)
      call calypso_mpi_bcast_one_int(nlayer_CMB_org, 0)
      call calypso_mpi_bcast_one_int(nlayer_ICB_new, 0)
      call calypso_mpi_bcast_one_int(nlayer_CMB_new, 0)
!
      if(my_rank .eq. 0) then
        write(*,*) 'nlayer_ICB_org: ', nlayer_ICB_org, nlayer_CMB_org
        write(*,*) 'nlayer_ICB_new: ', nlayer_ICB_new, nlayer_CMB_new
      end if
!
      end subroutine share_ICB_and_CMB_radius
!
! -----------------------------------------------------------------------
!
      subroutine sph_radial_interpolation_coef                          &
     &         (nri_org, r_org, nri_new, r_new, r_itp)
!
      use radial_interpolation
!
      integer(kind = kint), intent(in) :: nri_org, nri_new
      real(kind = kreal), intent(in) :: r_org(nri_org)
      real(kind = kreal), intent(in) :: r_new(nri_org)
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      r_itp%flag_same_rgrid                                             &
     &    = check_sph_same_radial_grid(nri_org, r_org, nri_new, r_new)
      if(r_itp%flag_same_rgrid) return
!
      call alloc_radial_interpolate(nri_new, r_itp)
!
      call cal_radial_interpolation_coef                                &
     &   (nri_org, r_org, nri_new, r_new,                               &
     &    r_itp%kr_inner_source, r_itp%kr_outer_source,                 &
     &    r_itp%k_old2new_in, r_itp%k_old2new_out,                      &
     &    r_itp%coef_old2new_in)
!
      call check_sph_radial_interpolate                                 &
     &   (nri_org, r_org, nri_new, r_new, r_itp)
!
      end subroutine sph_radial_interpolation_coef
!
! -----------------------------------------------------------------------
!
      end module r_interpolate_marged_sph
