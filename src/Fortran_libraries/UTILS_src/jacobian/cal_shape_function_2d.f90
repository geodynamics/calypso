!
!      module cal_shape_function_2d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on Aug., 2006
!
!      subroutine s_cal_shape_function_2d_linear(ntot_int_2d, an, dnxi, &
!     &          dnei, xi, ei)
!      subroutine s_cal_shape_function_2d_quad(ntot_int_2d, an, dnxi,   &
!     &          dnei, xi, ei)
!      subroutine s_cal_shape_function_2d_lag(ntot_int_2d, an, dnxi,    &
!     &          dnei, xi, ei)
!
      module cal_shape_function_2d
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use shape_func_elements
!
      implicit none
!
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
      real (kind=kreal) :: xi_sqre, ei_sqre
!
      private :: xi_nega, ei_nega, xi_posi, ei_posi
      private :: xi_sqre, ei_sqre
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_2d_linear(ntot_int_2d, an, dnxi,  &
     &          dnei, xi, ei)
!
      use shape_func_2d_linear
!
      integer (kind=kint), intent(in) :: ntot_int_2d
      real(kind=kreal), intent(in) :: xi(ntot_int_2d)
      real(kind=kreal), intent(in) :: ei(ntot_int_2d)
!
      real(kind=kreal), intent(inout) :: an(num_linear_sf,ntot_int_2d)
      real(kind=kreal), intent(inout) :: dnxi(num_linear_sf,ntot_int_2d)
      real(kind=kreal), intent(inout) :: dnei(num_linear_sf,ntot_int_2d)
! 
      integer (kind=kint) :: ix
! 
!
      do ix = 1, ntot_int_2d
        call s_shape_elenents_aw_2d(xi_nega, ei_nega,                   &
     &      xi_posi, ei_posi, xi_sqre, ei_sqre, xi(ix), ei(ix) )
!
        call shape_function_an_sf_1( an(1,ix), xi_nega, ei_nega,        &
     &      xi_posi, ei_posi)
        call shape_function_dnxi_sf_1( dnxi(1,ix), ei_nega, ei_posi,    &
     &      one)
        call shape_function_dnei_sf_1( dnei(1,ix), xi_nega, xi_posi,    &
     &      one)
      end do
!
      end subroutine s_cal_shape_function_2d_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_2d_quad(ntot_int_2d, an, dnxi,    &
     &          dnei, xi, ei)
!
      use shape_func_2d_quad
!
      integer (kind=kint), intent(in) :: ntot_int_2d
      real(kind=kreal), intent(in) :: xi(ntot_int_2d)
      real(kind=kreal), intent(in) :: ei(ntot_int_2d)
!
      real(kind=kreal), intent(inout) :: an(num_quad_sf,ntot_int_2d)
      real(kind=kreal), intent(inout) :: dnxi(num_quad_sf,ntot_int_2d)
      real(kind=kreal), intent(inout) :: dnei(num_quad_sf,ntot_int_2d)
!
      integer (kind=kint) :: ix
!
!
      do ix = 1, ntot_int_2d
        call s_shape_elenents_aw_2d(xi_nega, ei_nega,                   &
     &      xi_posi, ei_posi, xi_sqre, ei_sqre, xi(ix), ei(ix) )
!
        call shape_function_an_sf20( an(1,ix), xi(ix), ei(ix),          &
     &      xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre)
        call shape_function_dnxi_sf20( dnxi(1,ix), xi(ix), ei(ix),      &
     &      xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre, one)
        call shape_function_dnei_sf20( dnei(1,ix), xi(ix), ei(ix),      &
     &      xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre, one)
      end do
!
      end subroutine s_cal_shape_function_2d_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_2d_lag(ntot_int_2d, an, dnxi,     &
     &          dnei, xi, ei)
!
      use shape_func_2d_lag
!
      integer (kind=kint), intent(in) :: ntot_int_2d
      real (kind=kreal), intent(in) :: xi(ntot_int_2d)
      real (kind=kreal), intent(in) :: ei(ntot_int_2d)
!
      real (kind=kreal), intent(inout) :: an(num_lag_sf,ntot_int_2d)
      real (kind=kreal), intent(inout) :: dnxi(num_lag_sf,ntot_int_2d)
      real (kind=kreal), intent(inout) :: dnei(num_lag_sf,ntot_int_2d)
!
      integer (kind=kint) :: ix
!
!
      do ix = 1, ntot_int_2d
        call s_shape_elenents_aw_2d(xi_nega, ei_nega,                   &
     &      xi_posi, ei_posi, xi_sqre, ei_sqre, xi(ix), ei(ix) )
!
        call shape_function_an_sf27( an(1,ix), xi(ix), ei(ix),          &
     &      xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre)
        call shape_function_dnxi_sf27( dnxi(1,ix), xi(ix), ei(ix),      &
     &      xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre, one)
        call shape_function_dnei_sf27( dnei(1,ix), xi(ix), ei(ix),      &
     &      xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre, one)
      end do
!
      end subroutine s_cal_shape_function_2d_lag
!
!-----------------------------------------------------------------------
!
      end module cal_shape_function_2d
