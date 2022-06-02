!> @file  cal_geomagnetic_data.f90
!!      module cal_geomagnetic_data
!!
!! @author  T. Kera (Tohoku University)
!! @date Programmed in Oct., 2021
!
!> @brief Evaluate geomagnetic field data outside of spherical shell
!!
!!@verbatim
!!      subroutine cal_geomagnetic_rtp(sph_rtp, sph_rj,                 &
!!     &          sph_bc_B, bs_trns_base, fe_trns_prod,                 &
!!     &          ntot_comp_fld, fld_rtp, ntot_comp_fmg, fmag_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(base_field_address), intent(in) :: bs_trns_base
!!        type(phys_products_address), intent(in) :: fe_trns_prod
!!        integer(kind = kint), intent(in) :: ntot_comp_fld
!!        integer(kind = kint), intent(in) :: ntot_comp_fmg
!!        real(kind = kreal), intent(in)                                &
!!       &                   :: fld_rtp(sph_rtp%nnod_rtp,ntot_comp_fld)
!!        real(kind = kreal), intent(inout)                             &
!!       &                   :: fmag_rtp(sph_rtp%nnod_rtp,ntot_comp_fmg)
!!
!!      subroutine cal_geomagnetic_data_rtp(sph_rtp, d_rtp_magne,       &
!!     &          d_total_magne, d_decrenatin, d_increnation)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        real(kind=kreal), intent(in)                                  &
!!       &                 :: d_rtp_magne(sph_rtp%nnod_rtp,3)
!!        real(kind=kreal), intent(inout)                               &
!!       &                 :: d_total_magne(sph_rtp%nnod_rtp)
!!        real(kind=kreal), intent(inout)                               &
!!       &                 :: d_decrenatin(sph_rtp%nnod_rtp)
!!        real(kind=kreal), intent(inout)                               &
!!       &                 :: d_increnation(sph_rtp%nnod_rtp)
!!      subroutine cal_vgp_location_rtp(sph_rtp, nlayer_CMB,            &
!!     &                                d_increnation, d_decrenatin,    &
!!     &                                d_vgp_latitude, d_vgp_longitude)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        integer(kind = kint), intent(in) :: nlayer_CMB
!!        real(kind=kreal), intent(in)                                  &
!!     &                 :: d_decrenatin(sph_rtp%nnod_rtp)
!!        real(kind=kreal), intent(in)                                  &
!!     &                 :: d_increnation(sph_rtp%nnod_rtp)
!!        real(kind=kreal), intent(inout)                               &
!!     &                 :: d_vgp_latitude(sph_rtp%nnod_rtp)
!!        real(kind=kreal), intent(inout)                               &
!!     &                 :: d_vgp_longitude(sph_rtp%nnod_rtp)
!!@endverbatim
!
      module cal_geomagnetic_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_base_field_labels
      use t_field_product_labels
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_geomagnetic_rtp(sph_rtp, sph_rj,                   &
     &          sph_bc_B, bs_trns_base, fe_trns_prod,                   &
     &          ntot_comp_fld, fld_rtp, ntot_comp_fmg, fmag_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(base_field_address), intent(in) :: bs_trns_base
      type(phys_products_address), intent(in) :: fe_trns_prod
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_fmg
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_rtp(sph_rtp%nnod_rtp,ntot_comp_fld)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fmag_rtp(sph_rtp%nnod_rtp,ntot_comp_fmg)
!
!
      if(sph_bc_B%kr_out .ge. sph_rj%nidx_rj(1)) return
!
      if(      fe_trns_prod%i_magnetic_intensity .eq. 0                 &
     &    .or. fe_trns_prod%i_declination .eq. 0                        &
     &    .or. fe_trns_prod%i_inclination .eq. 0) return
      call cal_geomagnetic_data_rtp(sph_rtp,                            &
     &                   fld_rtp(1,bs_trns_base%i_magne),               &
     &                   fmag_rtp(1,fe_trns_prod%i_magnetic_intensity), &
     &                   fmag_rtp(1,fe_trns_prod%i_inclination),        &
     &                   fmag_rtp(1,fe_trns_prod%i_declination))
!
!
      if(      fe_trns_prod%i_vgp_latitude .eq. 0                       &
     &    .or. fe_trns_prod%i_vgp_longigude .eq. 0) return
      call cal_vgp_location_rtp(sph_rtp,                                &
     &                        fmag_rtp(1,fe_trns_prod%i_inclination),   &
     &                        fmag_rtp(1,fe_trns_prod%i_declination),   &
     &                        fmag_rtp(1,fe_trns_prod%i_vgp_latitude),  &
     &                        fmag_rtp(1,fe_trns_prod%i_vgp_longigude))
!
      end subroutine cal_geomagnetic_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_geomagnetic_data_rtp(sph_rtp, d_rtp_magne,         &
     &          d_total_magne, d_increnation, d_decrenatin)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind=kreal), intent(in)                                      &
     &                 :: d_rtp_magne(sph_rtp%nnod_rtp,3)
!
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_total_magne(sph_rtp%nnod_rtp)
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_increnation(sph_rtp%nnod_rtp)
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_decrenatin(sph_rtp%nnod_rtp)
!
      integer(kind = kint) :: kr, l_rtp, mphi, inod
      real(kind=kreal) :: pi, b_horiz
!
!
      pi = four * atan(one)
!$omp parallel
      do kr = 1, sph_rtp%nidx_rtp(1)
        do l_rtp = 1, sph_rtp%nidx_rtp(2)
!$omp do private(mphi,inod,b_horiz)
          do mphi = 1, sph_rtp%nidx_rtp(3)
            inod = 1 + (mphi-1) *  sph_rtp%istep_rtp(3)                 &
     &               + (l_rtp-1) * sph_rtp%istep_rtp(2)                 &
     &               + (kr-1) *    sph_rtp%istep_rtp(1)
!
            b_horiz =             sqrt(d_rtp_magne(inod,2)**2           &
     &                               + d_rtp_magne(inod,3)**2)
            d_total_magne(inod) = sqrt(d_rtp_magne(inod,1)**2           &
     &                               + d_rtp_magne(inod,2)**2           &
     &                               + d_rtp_magne(inod,3)**2)
!
            if(d_total_magne(inod) .eq. zero) then
              d_increnation(inod) = 0.0d0
              d_decrenatin(inod) = 0.0d0
            else if(b_horiz .eq. zero) then
              d_increnation(inod) = -asin(d_rtp_magne(inod,1)           &
     &                                 / d_total_magne(inod))
              d_decrenatin(inod) = 0.0d0
            else
              d_increnation(inod) = -asin(d_rtp_magne(inod,1)           &
     &                                 / d_total_magne(inod))
              d_decrenatin(inod) =  atan2(d_rtp_magne(inod,3),          &
     &                                  (-d_rtp_magne(inod,2)))
            end if
          end do
!$omp end do
        end do
      end do
!$omp end parallel
!
      end subroutine cal_geomagnetic_data_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_vgp_location_rtp(sph_rtp,                          &
     &                                d_increnation, d_decrenatin,      &
     &                                d_vgp_latitude, d_vgp_longitude)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind=kreal), intent(in)                                      &
     &                 :: d_increnation(sph_rtp%nnod_rtp)
      real(kind=kreal), intent(in)                                      &
     &                 :: d_decrenatin(sph_rtp%nnod_rtp)
!
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_vgp_latitude(sph_rtp%nnod_rtp)
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_vgp_longitude(sph_rtp%nnod_rtp)
!
      integer(kind = kint) :: kr, l_rtp, mphi, inod
      real(kind=kreal) :: sin_lat, cos_lat, d_long, p, p_lat, beta, pi
!
!
      pi = four * atan(one)
!$omp parallel
      do kr = 1, sph_rtp%nidx_rtp(1)
        do l_rtp = 1, sph_rtp%nidx_rtp(2)
!$omp do private(mphi,inod,sin_lat,cos_lat,d_long,p,p_lat,beta)
          do mphi = 1, sph_rtp%nidx_rtp(3)
            inod = 1 + (mphi-1) *  sph_rtp%istep_rtp(3)                 &
     &               + (l_rtp-1) * sph_rtp%istep_rtp(2)                 &
     &               + (kr-1) *    sph_rtp%istep_rtp(1)
!
            sin_lat = sph_rtp%cos_theta_1d_rtp(l_rtp)
            cos_lat = sph_rtp%sin_theta_1d_rtp(l_rtp)
            d_long = pi*dble(2*mphi-2) / sph_rtp%nidx_rtp(3)
!
            if(d_increnation(inod) .eq. zero) then
              p = two * atan(one)
            else
              p = atan(two / tan(d_increnation(inod)))
            end if
            p_lat = asin(sin_lat * cos(p)                               &
     &              + cos_lat * sin(p) * cos(d_decrenatin(inod)))
            beta = asin(sin(p) * sin(d_decrenatin(inod))                &
     &                / cos(p_lat))
!
            if(cos(p_lat) .ge. (sin_lat * sin(p_lat)) ) then
              d_vgp_longitude(inod)                                     &
     &                = mod(d_long+beta+three*pi,(two*pi)) - pi
            else
              d_vgp_longitude(inod)                                     &
     &                = mod(d_long-beta+two*pi,(two*pi)) - pi
            end if
            d_vgp_latitude(inod) = p_lat
          end do
!$omp end do
        end do
      end do
!$omp end parallel
!
      end subroutine cal_vgp_location_rtp
!
! -----------------------------------------------------------------------
!
      end module cal_geomagnetic_data
