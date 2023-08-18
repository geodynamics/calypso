!>@file   get_components_from_field.f90
!!@brief  module get_components_from_field
!!
!!@author H. Matsui (UC Berkeley) and T. Kera (Tohoku University)
!!@date Programmed in Aug, 2007
!>        Modified by T. Kera in Aug., 2021
!
!>@brief  Obtain spherical and cyrindrical radius component
!!        of valocity and magnetic field
!!
!!@verbatim
!!      subroutine get_components_from_fld                              &
!!     &         (sph_rtp, leg, b_trns_base, fs_trns_cmp,               &
!!     &          ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(field_component_address), intent(in) :: fs_trns_cmp
!!        integer(kind = kint), intent(in) :: ntot_comp_fld
!!        integer(kind = kint), intent(in) :: ntot_comp_frc
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: fld_rtp(sph_rtp%nnod_rtp,ntot_comp_fld)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: frc_rtp(sph_rtp%nnod_rtp,ntot_comp_frc)
!!@endverbatim
!
      module get_components_from_field
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_schmidt_poly_on_rtm
      use t_base_field_labels
      use t_field_component_labels
!
      implicit none
!
      private :: cal_cyl_r_comp_sph_smp, cal_z_comp_sph_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine get_components_from_fld                                &
     &         (sph_rtp, leg, b_trns_base, fs_trns_cmp,                 &
     &          ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp)
!
      use copy_field_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      type(base_field_address), intent(in) :: b_trns_base
      type(field_component_address), intent(in) :: fs_trns_cmp
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_frc
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_rtp(sph_rtp%nnod_rtp,ntot_comp_fld)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,ntot_comp_frc)
!
!
!$omp parallel
      if(fs_trns_cmp%i_velo_r .gt. 0) then
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp,                      &
     &                           fld_rtp(1,b_trns_base%i_velo  ),       &
     &                           frc_rtp(1,fs_trns_cmp%i_velo_r))
      end if
!
      if(fs_trns_cmp%i_velo_t .gt. 0) then
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp,                      &
     &                           fld_rtp(1,b_trns_base%i_velo+1),       &
     &                           frc_rtp(1,fs_trns_cmp%i_velo_t))
      end if
!
      if(fs_trns_cmp%i_velo_p .gt. 0) then
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp,                      &
     &                           fld_rtp(1,b_trns_base%i_velo+2),       &
     &                           frc_rtp(1,fs_trns_cmp%i_velo_p))
      end if
!
      if(fs_trns_cmp%i_velo_s .gt. 0) then
        call cal_cyl_r_comp_sph_smp(sph_rtp, leg,                       &
     &                              fld_rtp(1,b_trns_base%i_velo),      &
     &                              frc_rtp(1,fs_trns_cmp%i_velo_s))
      end if
!
      if(fs_trns_cmp%i_velo_x .gt. 0) then
        call cal_x_comp_sph_smp(sph_rtp, leg,                           &
     &                          fld_rtp(1,b_trns_base%i_velo),          &
     &                          frc_rtp(1,fs_trns_cmp%i_velo_x))
      end if
!
      if(fs_trns_cmp%i_velo_y .gt. 0) then
        call cal_y_comp_sph_smp(sph_rtp, leg,                           &
     &                          fld_rtp(1,b_trns_base%i_velo),          &
     &                          frc_rtp(1,fs_trns_cmp%i_velo_y))
      end if
!
      if(fs_trns_cmp%i_velo_z .gt. 0) then
        call cal_z_comp_sph_smp(sph_rtp, leg,                           &
     &                          fld_rtp(1,b_trns_base%i_velo),          &
     &                          frc_rtp(1,fs_trns_cmp%i_velo_z))
      end if
!
!
      if(fs_trns_cmp%i_magne_r .gt. 0) then
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp,                      &
     &                           fld_rtp(1,b_trns_base%i_magne  ),      &
     &                           frc_rtp(1,fs_trns_cmp%i_magne_r))
      end if
!
      if(fs_trns_cmp%i_magne_t .gt. 0) then
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp,                      &
     &                           fld_rtp(1,b_trns_base%i_magne+1),      &
     &                           frc_rtp(1,fs_trns_cmp%i_magne_t))
      end if
!
      if(fs_trns_cmp%i_magne_p .gt. 0) then
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp,                      &
     &                           fld_rtp(1,b_trns_base%i_magne+2),      &
     &                           frc_rtp(1,fs_trns_cmp%i_magne_p))
      end if
!
      if(fs_trns_cmp%i_magne_s .gt. 0) then
        call cal_cyl_r_comp_sph_smp(sph_rtp, leg,                       &
     &                              fld_rtp(1,b_trns_base%i_magne),     &
     &                              frc_rtp(1,fs_trns_cmp%i_magne_s))
      end if
!
      if(fs_trns_cmp%i_magne_x .gt. 0) then
        call cal_x_comp_sph_smp(sph_rtp, leg,                           &
     &                          fld_rtp(1,b_trns_base%i_magne),         &
     &                          frc_rtp(1,fs_trns_cmp%i_magne_x))
      end if
!
      if(fs_trns_cmp%i_magne_y .gt. 0) then
        call cal_y_comp_sph_smp(sph_rtp, leg,                           &
     &                          fld_rtp(1,b_trns_base%i_magne),         &
     &                          frc_rtp(1,fs_trns_cmp%i_magne_y))
      end if
!
      if(fs_trns_cmp%i_magne_z .gt. 0) then
        call cal_z_comp_sph_smp(sph_rtp, leg,                           &
     &                          fld_rtp(1,b_trns_base%i_magne),         &
     &                          frc_rtp(1,fs_trns_cmp%i_magne_z))
      end if
!$omp end parallel
!
      end subroutine get_components_from_fld
!
!-----------------------------------------------------------------------
!
      subroutine cal_cyl_r_comp_sph_smp(sph_rtp, leg, v_sph, v_s)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: v_sph(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: v_s(sph_rtp%nnod_rtp)
!
      integer (kind=kint) :: iproc, inod, ist, ied, l
!
!
!$omp do private(l,ist,ied,inod)
      do iproc = 1, np_smp
        ist = sph_rtp%istack_inod_rtp_smp(iproc-1) + 1
        ied = sph_rtp%istack_inod_rtp_smp(iproc)
!
!cdir nodep
        do inod = ist, ied
          l = sph_rtp%idx_global_rtp(inod,2)
          v_s(inod) =      v_sph(inod,1) * sin(leg%g_colat_rtm(l))      &
     &                   + v_sph(inod,2) * cos(leg%g_colat_rtm(l))
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cyl_r_comp_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_x_comp_sph_smp(sph_rtp, leg, v_sph, v_x)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: v_sph(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: v_x(sph_rtp%nnod_rtp)
!
      integer (kind=kint) :: iproc, inod, ist, ied, l, m
      real(kind = kreal) :: amphi
!
!
      amphi = 8.0d0 * atan(one) / dble(sph_rtp%nidx_rtp(3))
!$omp do private(l,m,ist,ied,inod)
      do iproc = 1, np_smp
        ist = sph_rtp%istack_inod_rtp_smp(iproc-1) + 1
        ied = sph_rtp%istack_inod_rtp_smp(iproc)
!
!cdir nodep
        do inod = ist, ied
          l = sph_rtp%idx_global_rtp(inod,2)
          m = sph_rtp%idx_global_rtp(inod,3)
          v_x(inod) =      v_sph(inod,1) * sin(leg%g_colat_rtm(l))      &
     &                                    * cos(dble(m-1) * amphi)      &
     &                   + v_sph(inod,2) * cos(leg%g_colat_rtm(l))      &
     &                                    * cos(dble(m-1) * amphi)      &
     &                   - v_sph(inod,3)  * sin(dble(m-1) * amphi)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_x_comp_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_y_comp_sph_smp(sph_rtp, leg, v_sph, v_y)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: v_sph(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: v_y(sph_rtp%nnod_rtp)
!
      integer (kind=kint) :: iproc, inod, ist, ied, l, m
      real(kind = kreal) :: amphi
!
!
      amphi = 8.0d0 * atan(one) / dble(sph_rtp%nidx_rtp(3))
!$omp do private(l,m,ist,ied,inod)
      do iproc = 1, np_smp
        ist = sph_rtp%istack_inod_rtp_smp(iproc-1) + 1
        ied = sph_rtp%istack_inod_rtp_smp(iproc)
!
!cdir nodep
        do inod = ist, ied
          l = sph_rtp%idx_global_rtp(inod,2)
          m = sph_rtp%idx_global_rtp(inod,3)
          v_y(inod) =      v_sph(inod,1) * sin(leg%g_colat_rtm(l))      &
     &                                    * sin(dble(m-1) * amphi)      &
     &                   + v_sph(inod,2) * cos(leg%g_colat_rtm(l))      &
     &                                    * sin(dble(m-1) * amphi)      &
     &                   + v_sph(inod,3)  * cos(dble(m-1) * amphi)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_y_comp_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_z_comp_sph_smp(sph_rtp, leg, v_sph, v_z)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: v_sph(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: v_z(sph_rtp%nnod_rtp)
!
      integer (kind=kint) :: iproc, inod, ist, ied, l
!
!
!$omp do private(l,ist,ied,inod)
      do iproc = 1, np_smp
        ist = sph_rtp%istack_inod_rtp_smp(iproc-1) + 1
        ied = sph_rtp%istack_inod_rtp_smp(iproc)
!
!cdir nodep
        do inod = ist, ied
          l = sph_rtp%idx_global_rtp(inod,2)
          v_z(inod) =      v_sph(inod,1) * cos(leg%g_colat_rtm(l))      &
     &                   - v_sph(inod,2) * sin(leg%g_colat_rtm(l))
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_z_comp_sph_smp
!
! -----------------------------------------------------------------------
!
      end module get_components_from_field
