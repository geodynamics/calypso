!
!      module set_shape_elements_infty_sf
!
!        modified by H. Matsui on Aug., 2006
!
!      subroutine  s_shape_elenents_inf_aw_3d(isf, xk,                  &
!     &          xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,  &
!     &          xi_sqre, ei_sqre, zi_sqre, xi_inf,  ei_inf,  zi_inf,   &
!     &          dxi_inf, dei_inf, dzi_inf, xi, ei, zi)
!
!      subroutine  s_shape_elenents_inf_aw_2d(isf, xk,                  &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,  &
!     &          xi_inf,  ei_inf,  dxi_inf, dei_inf, xi, ei)
!
!      subroutine  s_shape_elenents_inf_aw_1d(isf, xk,                  &
!     &          xi_nega, xi_posi, xi_sqre, xi_inf,  dxi_inf, xi)
!
      module set_shape_elements_infty_sf
!
      use m_precision
!
      use m_constants
      use shape_func_elements
      use shape_func_infty_elements
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine  s_shape_elenents_inf_aw_3d(isf, xk,                   &
     &          xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,   &
     &          xi_sqre, ei_sqre, zi_sqre, xi_inf,  ei_inf,  zi_inf,    &
     &          dxi_inf, dei_inf, dzi_inf, xi, ei, zi)
!
!
      integer(kind = kint) :: isf
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi, ei, zi
!
      real (kind=kreal), intent(inout) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(inout) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(inout) :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(inout) :: xi_inf,  ei_inf,  zi_inf
      real (kind=kreal), intent(inout) :: dxi_inf, dei_inf, dzi_inf
!
!
      if     (isf .eq. 1) then
!
        call s_shape_elenents_aw_inf_odd_1d(xi_inf, dxi_inf,            &
     &      xi_nega, xi_posi, xi_sqre, xi, xk)
        call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
        call s_shape_elenents_aw_1d(zi_nega, zi_posi, zi_sqre, zi)
        ei_inf = ei
        zi_inf = zi
        dei_inf = one
        dzi_inf = one
!
      else if(isf .eq. 2) then
!
        call s_shape_elenents_aw_inf_even_1d(xi_inf, dxi_inf,           &
     &      xi_nega, xi_posi, xi_sqre, xi, xk)
        call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
        call s_shape_elenents_aw_1d(zi_nega, zi_posi, zi_sqre, zi)
        ei_inf = ei
        zi_inf = zi
        dei_inf = one
        dzi_inf = one
!
      else if(isf .eq. 3) then
!
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
        call s_shape_elenents_aw_inf_odd_1d(ei_inf, dei_inf,            &
     &      ei_nega, ei_posi, ei_sqre, ei, xk)
        call s_shape_elenents_aw_1d(zi_nega, zi_posi, zi_sqre, zi)
        xi_inf = xi
        zi_inf = zi
        dxi_inf = one
        dzi_inf = one
!
      else if(isf .eq. 4) then
!
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
        call s_shape_elenents_aw_inf_even_1d(ei_inf, dei_inf,           &
     &      ei_nega, ei_posi, ei_sqre, ei, xk)
        call s_shape_elenents_aw_1d(zi_nega, zi_posi, zi_sqre, zi)
        xi_inf = xi
        zi_inf = zi
        dxi_inf = one
        dzi_inf = one
!
      else if(isf .eq. 5) then
!
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
        call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
        call s_shape_elenents_aw_inf_odd_1d(zi_inf, dzi_inf,            &
     &      zi_nega, zi_posi, zi_sqre, zi, xk)
        xi_inf = xi
        ei_inf = ei
        dxi_inf = one
        dei_inf = one
!
      else if(isf .eq. 6) then
!
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
        call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
        call s_shape_elenents_aw_inf_even_1d(zi_inf, dzi_inf,           &
     &      zi_nega, zi_posi, zi_sqre, zi, xk)
        xi_inf = xi
        ei_inf = ei
        dxi_inf = one
        dei_inf = one
!
      end if
!
      end subroutine  s_shape_elenents_inf_aw_3d
!
!-----------------------------------------------------------------------
!
      subroutine  s_shape_elenents_inf_aw_2d(isf, xk,                   &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,   &
     &          xi_inf,  ei_inf,  dxi_inf, dei_inf, xi, ei)
!
!
      integer(kind = kint) :: isf
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi, ei
!
      real (kind=kreal), intent(inout) :: xi_nega, ei_nega
      real (kind=kreal), intent(inout) :: xi_posi, ei_posi
      real (kind=kreal), intent(inout) :: xi_sqre, ei_sqre
      real (kind=kreal), intent(inout) :: xi_inf,  ei_inf
      real (kind=kreal), intent(inout) :: dxi_inf, dei_inf
!
!
      if     (isf .eq. 4) then
!
        call s_shape_elenents_aw_inf_odd_1d(xi_inf, dxi_inf,            &
     &      xi_nega, xi_posi, xi_sqre, xi, xk)
        call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
        ei_inf = ei
        dei_inf = one
!
      else if(isf .eq. 2) then
!
        call s_shape_elenents_aw_inf_even_1d(xi_inf, dxi_inf,           &
     &      xi_nega, xi_posi, xi_sqre, xi, xk)
        call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
        ei_inf = ei
        dei_inf = one
!
      else if(isf .eq. 1) then
!
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
        call s_shape_elenents_aw_inf_odd_1d(ei_inf, dei_inf,            &
     &      ei_nega, ei_posi, ei_sqre, ei, xk)
        xi_inf = xi
        dxi_inf = one
!
      else if(isf .eq. 3) then
!
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
        call s_shape_elenents_aw_inf_even_1d(ei_inf, dei_inf,           &
     &      ei_nega, ei_posi, ei_sqre, ei, xk)
        xi_inf = xi
        dxi_inf = one
!
      end if
!
      end subroutine  s_shape_elenents_inf_aw_2d
!
!-----------------------------------------------------------------------
!
      subroutine  s_shape_elenents_inf_aw_1d(isf, xk,                   &
     &          xi_nega, xi_posi, xi_sqre, xi_inf,  dxi_inf, xi)
!
!
      integer(kind = kint) :: isf
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi
!
      real (kind=kreal), intent(inout) :: xi_nega, xi_posi, xi_sqre
      real (kind=kreal), intent(inout) :: xi_inf,  dxi_inf
!
!
      if     (isf .eq. 1) then
!
        call s_shape_elenents_aw_inf_odd_1d(xi_inf, dxi_inf,            &
     &      xi_nega, xi_posi, xi_sqre, xi, xk)
!
      else if(isf .eq. 2) then
!
        call s_shape_elenents_aw_inf_even_1d(xi_inf, dxi_inf,           &
     &      xi_nega, xi_posi, xi_sqre, xi, xk)
!
      end if
!
      end subroutine  s_shape_elenents_inf_aw_1d
!
!-----------------------------------------------------------------------
!
      end module set_shape_elements_infty_sf
