!
!      module cal_shape_func_infty_3d
!
!        modified by H. Matsui on Aug., 2006
!
!      subroutine s_cal_shape_func_infty_linear(ntot_int_3d, xk,        &
!     &          an_infty, dnxi_infty, dnei_infty, dnzi_infty,          &
!     &          xi, ei, zi)
!      subroutine s_cal_shape_func_infty_quad(ntot_int_3d, xk, an_infty,&
!     &          dnxi_infty, dnei_infty, dnzi_infty, xi, ei, zi)
!      subroutine s_cal_shape_func_infty_lag(ntot_int_3d, xk, an_infty, &
!     &          dnxi_infty, dnei_infty, dnzi_infty, xi, ei, zi)
!
      module cal_shape_func_infty_3d
!
      use m_precision
!
      use m_geometry_constants
      use set_shape_elements_infty_sf
!
      implicit none
!
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal) :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal) :: xi_inf,  ei_inf,  zi_inf
      real (kind=kreal) :: dxi_inf, dei_inf, dzi_inf
!
      private :: xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi
      private :: xi_sqre, ei_sqre, zi_sqre
      private :: xi_inf,  ei_inf,  zi_inf, dxi_inf, dei_inf, dzi_inf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_infty_linear(ntot_int_3d, xk,         &
     &          an_infty, dnxi_infty, dnei_infty, dnzi_infty,           &
     &          xi, ei, zi)
!
      use shape_func_3d_linear
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi(ntot_int_3d)
      real (kind=kreal), intent(in) :: ei(ntot_int_3d)
      real (kind=kreal), intent(in) :: zi(ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_t_linear,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_t_linear,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnei_infty(num_t_linear,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnzi_infty(num_t_linear,nsurf_4_ele,ntot_int_3d)
!
      integer (kind=kint) :: isf, ix
!
!
      do isf = 1, nsurf_4_ele
        do ix = 1, ntot_int_3d
          call s_shape_elenents_inf_aw_3d(isf, xk,                      &
     &        xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,     &
     &        xi_sqre, ei_sqre, zi_sqre, xi_inf,  ei_inf,  zi_inf,      &
     &        dxi_inf, dei_inf, dzi_inf, xi(ix), ei(ix), zi(ix) )
!
          call shape_function_an_1( an_infty(1,isf,ix),                 &
     &        xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi)
          call shape_function_dnxi_1( dnxi_infty(1,isf,ix),             &
     &        ei_nega, zi_nega, ei_posi, zi_posi, dxi_inf)
          call shape_function_dnei_1( dnei_infty(1,isf,ix),             &
     &        xi_nega, zi_nega, xi_posi, zi_posi, dei_inf)
          call shape_function_dnzi_1( dnzi_infty(1,isf,ix),             &
     &        xi_nega, ei_nega, xi_posi, ei_posi, dzi_inf)
        end do
      end do
!
!
      end subroutine s_cal_shape_func_infty_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_infty_quad(ntot_int_3d, xk, an_infty, &
     &          dnxi_infty, dnei_infty, dnzi_infty, xi, ei, zi)
!
      use shape_func_3d_quad
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi(ntot_int_3d)
      real (kind=kreal), intent(in) :: ei(ntot_int_3d)
      real (kind=kreal), intent(in) :: zi(ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnei_infty(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnzi_infty(num_t_quad,nsurf_4_ele,ntot_int_3d)
!
      integer (kind=kint) :: isf, ix
!
!
      do isf = 1, nsurf_4_ele
        do ix = 1, ntot_int_3d
          call s_shape_elenents_inf_aw_3d(isf, xk,                      &
     &        xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,     &
     &        xi_sqre, ei_sqre, zi_sqre, xi_inf,  ei_inf,  zi_inf,      &
     &        dxi_inf, dei_inf, dzi_inf, xi(ix), ei(ix), zi(ix) )
!
          call shape_function_an_20( an_infty(1,isf,ix),                &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre)
          call shape_function_dnxi_20( dnxi_infty(1,isf,ix),            &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,     &
     &       dxi_inf)
          call shape_function_dnei_20( dnei_infty(1,isf,ix),            &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,     &
     &       dei_inf)
          call shape_function_dnzi_20( dnzi_infty(1,isf,ix),            &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,     &
     &       dzi_inf)
        end do
      end do
!
!
      end subroutine s_cal_shape_func_infty_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_infty_lag(ntot_int_3d, xk, an_infty,  &
     &          dnxi_infty, dnei_infty, dnzi_infty, xi, ei, zi)
!
      use shape_func_3d_lag
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi(ntot_int_3d)
      real (kind=kreal), intent(in) :: ei(ntot_int_3d)
      real (kind=kreal), intent(in) :: zi(ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_t_lag,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_t_lag,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnei_infty(num_t_lag,nsurf_4_ele,ntot_int_3d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnzi_infty(num_t_lag,nsurf_4_ele,ntot_int_3d)
!
      integer (kind=kint) :: isf, ix
!
!
      do isf = 1, nsurf_4_ele
        do ix = 1, ntot_int_3d
          call s_shape_elenents_inf_aw_3d(isf, xk,                      &
     &        xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,     &
     &        xi_sqre, ei_sqre, zi_sqre, xi_inf,  ei_inf,  zi_inf,      &
     &        dxi_inf, dei_inf, dzi_inf, xi(ix), ei(ix), zi(ix) )
!
          call shape_function_an_27( an_infty(1,isf,ix),                &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre)
          call shape_function_dnxi_27( dnxi_infty(1,isf,ix),            &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,     &
     &       dxi_inf)
          call shape_function_dnei_27( dnei_infty(1,isf,ix),            &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,     &
     &       dei_inf)
          call shape_function_dnzi_27( dnzi_infty(1,isf,ix),            &
     &        xi_inf,  ei_inf,  zi_inf,  xi_nega, ei_nega, zi_nega,     &
     &        xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,     &
     &       dzi_inf)
        end do
      end do
!
      end subroutine s_cal_shape_func_infty_lag
!
!-----------------------------------------------------------------------
!
      end module cal_shape_func_infty_3d
