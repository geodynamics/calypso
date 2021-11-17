!
!      module shape_func_infty_elements
!
!        modified by H. Matsui on Aug., 2006
!
!      subroutine s_shape_elenents_aw_inf_odd_1d(xi_inf, dxi_inf,       &
!     &          xi_nega, xi_posi, xi_sqre, xi, xk)
!      subroutine s_shape_elenents_aw_inf_even_1d(xi_inf, dxi_inf,      &
!     &          xi_nega, xi_posi, xi_sqre, xi, xk)
!
      module shape_func_infty_elements
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: s_trans_xi_4_inf_odd, s_trans_xi_4_inf_even
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_shape_elenents_aw_inf_odd_1d(xi_inf, dxi_inf,        &
     &          xi_nega, xi_posi, xi_sqre, xi, xk)
!
      use shape_func_elements
!
      real(kind = kreal), intent(in) :: xi, xk
      real(kind = kreal), intent(inout) :: xi_inf, dxi_inf
      real(kind = kreal), intent(inout) :: xi_nega, xi_posi, xi_sqre
!
!
      call s_trans_xi_4_inf_odd(xi_inf, dxi_inf, xi, xk)
!
      call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi_inf)
!
      end subroutine s_shape_elenents_aw_inf_odd_1d
!
!-----------------------------------------------------------------------
!
      subroutine s_shape_elenents_aw_inf_even_1d(xi_inf, dxi_inf,       &
     &          xi_nega, xi_posi, xi_sqre, xi, xk)
!
      use shape_func_elements
!
      real(kind = kreal), intent(in) :: xi, xk
      real(kind = kreal), intent(inout) :: xi_inf, dxi_inf
      real(kind = kreal), intent(inout) :: xi_nega, xi_posi, xi_sqre
!
!
      call s_trans_xi_4_inf_even(xi_inf, dxi_inf, xi, xk)
!
      call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi_inf)
!
      end subroutine s_shape_elenents_aw_inf_even_1d
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_trans_xi_4_inf_odd(xi_inf, dxi_inf, xi, xk)
!
      real(kind = kreal), intent(in) :: xi, xk
      real(kind = kreal), intent(inout) :: xi_inf, dxi_inf
!
!
      xi_inf =  one - (two**(xk+one) / (two**xk - 1))                   &
     &              * (one - one / (one + xi)**xk )
      dxi_inf = (two**(xk+one) / (xk*(two**xk - 1)) )                   &
     &              * (one / (one + xi)**xk )
!
      end subroutine s_trans_xi_4_inf_odd
!
!-----------------------------------------------------------------------
!
      subroutine s_trans_xi_4_inf_even(xi_inf, dxi_inf, xi, xk)
!
      real(kind = kreal), intent(in) :: xi, xk
      real(kind = kreal), intent(inout) :: xi_inf, dxi_inf
!
!
      xi_inf =  one - (two**(xk+one) / (two**xk - 1))                   &
     &              * (one - one / (one - xi)**xk )
      dxi_inf =-(two**(xk+one) / (xk*(two**xk - 1)) )                   &
     &              * (one / (one - xi)**xk )
!
      end subroutine s_trans_xi_4_inf_even
!
!-----------------------------------------------------------------------
!
      end module shape_func_infty_elements
