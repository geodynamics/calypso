!> @file  products_at_poles.f90
!!      module products_at_poles
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Evaluate products at poles
!!
!!@verbatim
!!      subroutine pole_fld_cst_dot_prod(numnod, internal_node,         &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef,                       &
!!     &          ncomp_nod, i_vec1, i_vec2, i_prod, d_nod)
!!      subroutine pole_fld_cst_cross_prod(numnod, internal_node,       &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef,                       &
!!     &          ncomp_nod, i_vec1, i_vec2, i_prod, d_nod)
!!      subroutine pole_fld_cst_vec_scalar_prod(numnod, internal_node,  &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef,                       &
!!     &          ncomp_nod, i_vec1, i_scl, i_prod, d_nod)
!!
!!      subroutine pole_sph_dot_prod_w_const(numnod, internal_node,     &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2,       &
!!     &          d_prod)
!!      subroutine pole_sph_cross_prod_w_const(numnod, internal_node,   &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2,       &
!!     &          d_prod)
!!      subroutine pole_vec_scalar_prod_w_const(numnod, internal_node,  &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_scl,        &
!!     &          d_prod)
!!@endverbatim
!!
!!@n @param numnod               number of nodes
!!@n @param internal_node        number of internal nodes
!!@n @param xx                   position
!!@n @param nnod_rtp             number of grid points
!!                               for @f$ f(r,\theta,\phi) @f$
!!@n @param nidx_rtp_r           number of radial points 
!!                               for @f$ f(r,\theta,\phi) @f$
!!@n @param coef                 coefficient
!!@n @param d_vec1(numnod,3)     vector field
!!@n @param d_vec2(numnod,3)     vector field
!!@n @param d_scl(numnod)        scalar field
!!@n @param d_prod(numnod,nd)    produced field
!
      module products_at_poles
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine pole_fld_cst_dot_prod(numnod, internal_node,           &
     &          xx, nnod_rtp, nidx_rtp_r, coef,                         &
     &          ncomp_nod, i_vec1, i_vec2, i_prod, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_vec1, i_vec2, i_prod
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      call pole_sph_dot_prod_w_const(numnod, internal_node,             &
     &          xx, nnod_rtp, nidx_rtp_r, coef,                         &
     &          d_nod(1,i_vec1), d_nod(1,i_vec2), d_nod(1,i_prod))
!
      end subroutine pole_fld_cst_dot_prod
!
! -----------------------------------------------------------------------
!
      subroutine pole_fld_cst_cross_prod(numnod, internal_node,         &
     &          xx, nnod_rtp, nidx_rtp_r, coef,                         &
     &          ncomp_nod, i_vec1, i_vec2, i_prod, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_vec1, i_vec2, i_prod
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      call pole_sph_cross_prod_w_const(numnod, internal_node,           &
     &          xx, nnod_rtp, nidx_rtp_r, coef,                         &
     &          d_nod(1,i_vec1), d_nod(1,i_vec2), d_nod(1,i_prod))
!
      end subroutine pole_fld_cst_cross_prod
!
! -----------------------------------------------------------------------
!
      subroutine pole_fld_cst_vec_scalar_prod(numnod, internal_node,    &
     &          xx, nnod_rtp, nidx_rtp_r, coef,                         &
     &          ncomp_nod, i_vec1, i_scl, i_prod, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_vec1, i_scl, i_prod
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      call pole_vec_scalar_prod_w_const(numnod, internal_node,          &
     &          xx, nnod_rtp, nidx_rtp_r, coef,                         &
     &          d_nod(1,i_vec1), d_nod(1,i_scl), d_nod(1,i_prod))
!
      end subroutine pole_fld_cst_vec_scalar_prod
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pole_sph_dot_prod_w_const(numnod, internal_node,       &
     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2,         &
     &          d_prod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: d_vec1(numnod,3)
      real(kind = kreal), intent(in) :: d_vec2(numnod,3)
      real(kind = kreal), intent(inout) :: d_prod(numnod)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod) =   coef * (d_vec1(inod,1)*d_vec2(inod,1)        &
     &                           + d_vec1(inod,2)*d_vec2(inod,2)        &
     &                           + d_vec1(inod,3)*d_vec2(inod,3)  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod) =   coef * (d_vec1(inod,1)*d_vec2(inod,1)        &
     &                           + d_vec1(inod,2)*d_vec2(inod,2)        &
     &                           + d_vec1(inod,3)*d_vec2(inod,3)  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_prod(inod) =   coef * (d_vec1(inod,1)*d_vec2(inod,1)            &
     &                       + d_vec1(inod,2)*d_vec2(inod,2)            &
     &                       + d_vec1(inod,3)*d_vec2(inod,3)  )
!
      end subroutine pole_sph_dot_prod_w_const
!
! -----------------------------------------------------------------------
!
      subroutine pole_sph_cross_prod_w_const(numnod, internal_node,     &
     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2,         &
     &          d_prod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: d_vec1(numnod,3)
      real(kind = kreal), intent(in) :: d_vec2(numnod,3)
      real(kind = kreal), intent(inout) :: d_prod(numnod,3)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * (d_vec1(inod,2)*d_vec2(inod,3)      &
     &                             - d_vec1(inod,3)*d_vec2(inod,2) )
          d_prod(inod,2) =   coef * (d_vec1(inod,3)*d_vec2(inod,1)      &
     &                             - d_vec1(inod,1)*d_vec2(inod,3) )
          d_prod(inod,3) =   coef * (d_vec1(inod,1)*d_vec2(inod,2)      &
     &                             - d_vec1(inod,2)*d_vec2(inod,1) )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * (d_vec1(inod,2)*d_vec2(inod,3)      &
     &                             - d_vec1(inod,3)*d_vec2(inod,2) )
          d_prod(inod,2) =   coef * (d_vec1(inod,3)*d_vec2(inod,1)      &
     &                             - d_vec1(inod,1)*d_vec2(inod,3) )
          d_prod(inod,3) =   coef * (d_vec1(inod,1)*d_vec2(inod,2)      &
     &                             - d_vec1(inod,2)*d_vec2(inod,1) )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_prod(inod,1) =   coef * (d_vec1(inod,2)*d_vec2(inod,3)          &
     &                         - d_vec1(inod,3)*d_vec2(inod,2) )
      d_prod(inod,2) =   coef * (d_vec1(inod,3)*d_vec2(inod,1)          &
     &                         - d_vec1(inod,1)*d_vec2(inod,3) )
      d_prod(inod,3) =   coef * (d_vec1(inod,1)*d_vec2(inod,2)          &
     &                         - d_vec1(inod,2)*d_vec2(inod,1) )
!
      end subroutine pole_sph_cross_prod_w_const
!
! -----------------------------------------------------------------------
!
      subroutine pole_vec_scalar_prod_w_const(numnod, internal_node,    &
     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_scl,          &
     &          d_prod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: d_vec1(numnod,3)
      real(kind = kreal), intent(in) :: d_scl(numnod)
      real(kind = kreal), intent(inout) :: d_prod(numnod,3)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * d_vec1(inod,1)*d_scl(inod)
          d_prod(inod,2) =   coef * d_vec1(inod,2)*d_scl(inod)
          d_prod(inod,3) =   coef * d_vec1(inod,3)*d_scl(inod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * d_vec1(inod,1)*d_scl(inod)
          d_prod(inod,2) =   coef * d_vec1(inod,2)*d_scl(inod)
          d_prod(inod,3) =   coef * d_vec1(inod,3)*d_scl(inod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_prod(inod,1) =   coef * d_vec1(inod,1)*d_scl(inod)
      d_prod(inod,2) =   coef * d_vec1(inod,2)*d_scl(inod)
      d_prod(inod,3) =   coef * d_vec1(inod,3)*d_scl(inod)
!
      end subroutine pole_vec_scalar_prod_w_const
!
! -----------------------------------------------------------------------
!
      end module products_at_poles
