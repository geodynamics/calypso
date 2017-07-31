!>@file   set_radius_4_sph_dynamo.f90
!!@brief  module set_radius_4_sph_dynamo
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in  2008
!
!> @brief Set radial information
!!
!!@verbatim
!!      Programmed by H. Matsui on June., 1994
!!      modified by H. Matsui on Apr., 2009
!!
!!      subroutine set_radius_dat_4_sph_dynamo                          &
!!     &         (nri, radius_1d_rj_r, radial_rj_grp, iflag_radial_grid,&
!!     &          nlayer_ICB, nlayer_CMB, nlayer_2_center,              &
!!     &          ar_1d_rj, r_ele_rj, ar_ele_rj, r_ICB, r_CMB, R_earth)
!!        type(group_data), intent(in) :: radial_rj_grp
!!***********************************************************************
!!*
!!*       ar_1d_rj(k,1)   : 1 / r
!!*       ar_1d_rj(k,2)   : 1 / r**2
!!*       ar_1d_rj(k,3)   : 1 / r**3
!!*
!!***********************************************************************
!!@endverbatim
!
!
      module set_radius_4_sph_dynamo
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      use t_spheric_parameter
      use t_group_data
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_radius_dat_4_sph_dynamo                            &
     &         (nri, radius_1d_rj_r, radial_rj_grp, iflag_radial_grid,  &
     &          nlayer_ICB, nlayer_CMB, nlayer_2_center,                &
     &          ar_1d_rj, r_ele_rj, ar_ele_rj, r_ICB, r_CMB, R_earth)
!
      use set_radial_grid_sph_shell
      use skip_comment_f
!
      type(group_data), intent(in) :: radial_rj_grp
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      integer(kind = kint), intent(inout) :: iflag_radial_grid
      integer(kind = kint), intent(inout) :: nlayer_ICB
      integer(kind = kint), intent(inout) :: nlayer_CMB
      integer(kind = kint), intent(inout) :: nlayer_2_center
      real(kind = kreal), intent(inout) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(inout) :: r_ele_rj(nri)
      real(kind = kreal), intent(inout) :: ar_ele_rj(nri,3)
      real(kind = kreal), intent(inout) :: R_earth(0:2)
      real(kind = kreal), intent(inout) ::r_ICB, r_CMB
!
      integer(kind = kint) :: k, kk
!
!
!* --------  radius  --------------
!
      do k = 1, radial_rj_grp%num_grp
        if(     cmp_no_case(radial_rj_grp%grp_name(k),                  &
     &                      ICB_nod_grp_name)) then
          kk = radial_rj_grp%istack_grp(k-1) + 1
          nlayer_ICB = radial_rj_grp%item_grp(kk)
        else if(cmp_no_case(radial_rj_grp%grp_name(k),                  &
     &                      CMB_nod_grp_name)) then
          kk = radial_rj_grp%istack_grp(k-1) + 1
          nlayer_CMB = radial_rj_grp%item_grp(kk)
        else if(cmp_no_case(radial_rj_grp%grp_name(k),                  &
     &                      CTR_nod_grp_name)) then
          kk = radial_rj_grp%istack_grp(k-1) + 1
          nlayer_2_center = radial_rj_grp%item_grp(kk)
        end if
      end do
!
      r_CMB = radius_1d_rj_r(nlayer_CMB)
      r_ICB = radius_1d_rj_r(nlayer_ICB)
!
      R_earth(0) = (64.0d0 / 35.0d0) * r_CMB
      R_earth(1) = one / R_earth(0)
      R_earth(2) = one / R_earth(0)**2
!
!
      call set_radial_distance_flag(nri, nlayer_ICB, nlayer_CMB,        &
     &    r_ICB, r_CMB, radius_1d_rj_r, iflag_radial_grid)
!
!
      do k = 1 ,nlayer_CMB
        ar_1d_rj(k,1) = one / radius_1d_rj_r(k)
        ar_1d_rj(k,2) = ar_1d_rj(k,1)**2
        ar_1d_rj(k,3) = ar_1d_rj(k,1)**3
      end do
!
      r_ele_rj(1) = half * radius_1d_rj_r(1)
      do k = 2 ,nlayer_CMB
        r_ele_rj(k) = half * (radius_1d_rj_r(k-1) + radius_1d_rj_r(k))
        ar_ele_rj(k,1) = one / r_ele_rj(k)
        ar_ele_rj(k,2) = ar_ele_rj(k,1)**2
        ar_ele_rj(k,3) = ar_ele_rj(k,1)**3
      end do
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'nlayer_ICB', nlayer_ICB, radius_1d_rj_r(nlayer_ICB)
        write(*,*) 'nlayer_CMB', nlayer_CMB, radius_1d_rj_r(nlayer_CMB)
        write(*,*) 'iflag_radial_grid', iflag_radial_grid
      end if
!
      end subroutine set_radius_dat_4_sph_dynamo
!
!  -------------------------------------------------------------------
!
      end module set_radius_4_sph_dynamo
