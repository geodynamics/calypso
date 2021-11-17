!
!      module cal_shape_function_3d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on Aug., 2006
!
!      subroutine s_cal_shape_function_linear(ntot_int_3d, an, dnxi,    &
!     &          dnei, dnzi, xi, ei, zi)
!      subroutine s_cal_shape_function_quad(ntot_int_3d, an, dnxi,      &
!     &          dnei, dnzi, xi, ei, zi)
!      subroutine s_cal_shape_function_lag(ntot_int_3d, an, dnxi,       &
!     &          dnei, dnzi, xi, ei, zi)
!
      module cal_shape_function_3d
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use shape_func_elements
!
      implicit none
!
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal) :: xi_sqre, ei_sqre, zi_sqre
!
      private :: xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi
      private :: xi_sqre, ei_sqre, zi_sqre
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_linear(ntot_int_3d, an, dnxi,     &
     &          dnei, dnzi, xi, ei, zi)
!
      use shape_func_3d_linear
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real (kind=kreal), intent(in) :: xi(ntot_int_3d)
      real (kind=kreal), intent(in) :: ei(ntot_int_3d)
      real (kind=kreal), intent(in) :: zi(ntot_int_3d)
!
      real (kind=kreal), intent(inout) :: an(num_t_linear,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnxi(num_t_linear,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnei(num_t_linear,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnzi(num_t_linear,ntot_int_3d)
! 
      integer (kind=kint) :: ix
! 
!
      do ix = 1, ntot_int_3d
        call s_shape_elenents_aw_3d(xi_nega, ei_nega, zi_nega,          &
     &          xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,   &
     &          xi(ix), ei(ix), zi(ix) )
!
        call shape_function_an_1( an(1,ix),                             &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi)
        call shape_function_dnxi_1( dnxi(1,ix),                         &
     &      ei_nega, zi_nega, ei_posi, zi_posi, one)
        call shape_function_dnei_1( dnei(1,ix),                         &
     &      xi_nega, zi_nega, xi_posi, zi_posi, one)
        call shape_function_dnzi_1( dnzi(1,ix),                         &
     &      xi_nega, ei_nega, xi_posi, ei_posi, one)
      end do
!
      end subroutine s_cal_shape_function_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_quad(ntot_int_3d, an, dnxi,       &
     &          dnei, dnzi, xi, ei, zi)
!
      use shape_func_3d_quad
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real (kind=kreal), intent(in) :: xi(ntot_int_3d)
      real (kind=kreal), intent(in) :: ei(ntot_int_3d)
      real (kind=kreal), intent(in) :: zi(ntot_int_3d)
!
      real (kind=kreal), intent(inout) :: an(num_t_quad,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnxi(num_t_quad,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnei(num_t_quad,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnzi(num_t_quad,ntot_int_3d)
!
      integer (kind=kint) :: ix
!
!
      do ix = 1, ntot_int_3d
        call s_shape_elenents_aw_3d(xi_nega, ei_nega, zi_nega,          &
     &          xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,   &
     &          xi(ix), ei(ix), zi(ix) )
!
        call shape_function_an_20( an(1,ix), xi(ix), ei(ix), zi(ix),    &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre)
        call shape_function_dnxi_20(dnxi(1,ix), xi(ix), ei(ix), zi(ix), &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, one)
        call shape_function_dnei_20(dnei(1,ix), xi(ix), ei(ix), zi(ix), &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, one)
        call shape_function_dnzi_20(dnzi(1,ix), xi(ix), ei(ix), zi(ix), &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, one)
      end do
!
      end subroutine s_cal_shape_function_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_lag(ntot_int_3d, an, dnxi,        &
     &          dnei, dnzi, xi, ei, zi)
!
      use shape_func_3d_lag
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real (kind=kreal), intent(in) :: xi(ntot_int_3d)
      real (kind=kreal), intent(in) :: ei(ntot_int_3d)
      real (kind=kreal), intent(in) :: zi(ntot_int_3d)
!
      real (kind=kreal), intent(inout) :: an(num_t_lag,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnxi(num_t_lag,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnei(num_t_lag,ntot_int_3d)
      real (kind=kreal), intent(inout) :: dnzi(num_t_lag,ntot_int_3d)
!
      integer (kind=kint) :: ix
!
!
      do ix = 1, ntot_int_3d
        call s_shape_elenents_aw_3d(xi_nega, ei_nega, zi_nega,          &
     &          xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,   &
     &          xi(ix), ei(ix), zi(ix) )
!
        call shape_function_an_27( an(1,ix), xi(ix), ei(ix), zi(ix),    &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre)
        call shape_function_dnxi_27(dnxi(1,ix), xi(ix), ei(ix), zi(ix), &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, one)
        call shape_function_dnei_27(dnei(1,ix), xi(ix), ei(ix), zi(ix), &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, one)
        call shape_function_dnzi_27(dnzi(1,ix), xi(ix), ei(ix), zi(ix), &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, one)
      end do
!
      end subroutine s_cal_shape_function_lag
!
!-----------------------------------------------------------------------
!
      end module cal_shape_function_3d
