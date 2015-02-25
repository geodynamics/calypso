!> @file  copy_pole_field_sph_trans.f90
!!      module copy_pole_field_sph_trans
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief copy field data from spherical transform for poles
!!
!!@verbatim
!!      subroutine copy_pole_vec_fld_from_trans(numnod, internal_node,  &
!!     &          xx, v_pole, ntot_phys, i_fld, d_nod)
!!      subroutine copy_pole_scl_fld_from_trans(numnod, internal_node,  &
!!     &          xx, v_pole, ntot_phys, i_fld, d_nod)
!!      subroutine copy_pole_tsr_fld_from_trans(numnod, internal_node,  &
!!     &          xx, v_pole, ntot_phys, i_fld, d_nod)
!!@endverbatim
!!
!!
!
      module copy_pole_field_sph_trans
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use calypso_mpi
      use m_work_pole_sph_trans
!
      implicit  none
! 
      private :: copy_pole_vector_from_trans
      private :: copy_pole_scalar_from_trans
      private :: copy_pole_tensor_from_trans
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_pole_vec_fld_from_trans(numnod, internal_node,    &
     &          xx, v_pole, ntot_phys, i_fld, d_nod)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: i_fld
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
      if(i_fld .le. 0) return
      call copy_pole_vector_from_trans(numnod, internal_node, xx,       &
     &    ntot_phys, nnod_rtp, nidx_rtp(1), nidx_global_rtp(1),         &
     &    idx_gl_1d_rtp_r, v_pole, i_fld, d_nod)
!
      end subroutine copy_pole_vec_fld_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_scl_fld_from_trans(numnod, internal_node,    &
     &          xx, v_pole, ntot_phys, i_fld, d_nod)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: i_fld
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
      if(i_fld .le. 0) return
      call copy_pole_scalar_from_trans(numnod, internal_node, xx,       &
     &    ntot_phys, nnod_rtp, nidx_rtp(1),  nidx_global_rtp(1),        &
     &    idx_gl_1d_rtp_r, v_pole, i_fld, d_nod)
!
      end subroutine copy_pole_scl_fld_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_tsr_fld_from_trans(numnod, internal_node,    &
     &          xx, v_pole, ntot_phys, i_fld, d_nod)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: i_fld
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,6)
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
!
!
      if(i_fld .le. 0) return
      call copy_pole_tensor_from_trans(numnod, internal_node, xx,       &
     &    ntot_phys, nnod_rtp, nidx_rtp(1), nidx_global_rtp(1),         &
     &    idx_gl_1d_rtp_r, v_pole, i_fld, d_nod)
!
      end subroutine copy_pole_tsr_fld_from_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_pole_vector_from_trans(numnod, internal_node, xx, &
     &         ntot_phys, nnod_rtp, nidx_rtp_r, nidx_rtp_gl_r,          &
     &         idx_gl_rtp_r, v_pole, i_fld, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r, nidx_rtp_gl_r
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: idx_gl_rtp_r(nidx_rtp_gl_r)
!
      integer(kind = kint), intent(in) :: i_fld
!
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
!
      integer(kind = kint) :: inod, jnod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = idx_gl_rtp_r(kr)
          d_nod(inod,i_fld  ) = v_pole(jnod,1)
          d_nod(inod,i_fld+1) = v_pole(jnod,2)
          d_nod(inod,i_fld+2) = v_pole(jnod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = idx_gl_rtp_r(kr) + nidx_rtp_gl_r
          d_nod(inod,i_fld  ) = v_pole(jnod,1)
          d_nod(inod,i_fld+1) = v_pole(jnod,2)
          d_nod(inod,i_fld+2) = v_pole(jnod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      jnod = 2 * nidx_rtp_gl_r + 1
      d_nod(inod,i_fld:i_fld+2) = v_pole(jnod,1:3)
!
      end subroutine copy_pole_vector_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_scalar_from_trans(numnod, internal_node, xx, &
     &         ntot_phys, nnod_rtp, nidx_rtp_r, nidx_rtp_gl_r,          &
     &         idx_gl_rtp_r, v_pole, i_fld, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r, nidx_rtp_gl_r
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: idx_gl_rtp_r(nidx_rtp_gl_r)
!
      integer(kind = kint), intent(in) :: i_fld
!
      real(kind = kreal), intent(in) :: v_pole(nnod_pole)
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
!
      integer(kind = kint) :: inod, jnod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = idx_gl_rtp_r(kr)
          d_nod(inod,i_fld  ) = v_pole(jnod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = idx_gl_rtp_r(kr) + nidx_rtp_gl_r
          d_nod(inod,i_fld  ) = v_pole(jnod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      jnod = 2 * nidx_rtp_gl_r + 1
      d_nod(inod,i_fld) = v_pole(jnod)
!
      end subroutine copy_pole_scalar_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_tensor_from_trans(numnod, internal_node, xx, &
     &         ntot_phys, nnod_rtp, nidx_rtp_r, nidx_rtp_gl_r,          &
     &         idx_gl_rtp_r, v_pole, i_fld, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r, nidx_rtp_gl_r
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: idx_gl_rtp_r(nidx_rtp_gl_r)
!
      integer(kind = kint), intent(in) :: i_fld
!
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,6)
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
!
      integer(kind = kint) :: inod, jnod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = idx_gl_rtp_r(kr)
          d_nod(inod,i_fld  ) = v_pole(jnod,1)
          d_nod(inod,i_fld+1) = v_pole(jnod,2)
          d_nod(inod,i_fld+2) = v_pole(jnod,3)
          d_nod(inod,i_fld+3) = v_pole(jnod,4)
          d_nod(inod,i_fld+4) = v_pole(jnod,5)
          d_nod(inod,i_fld+5) = v_pole(jnod,6)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = idx_gl_rtp_r(kr) + nidx_rtp_gl_r
          d_nod(inod,i_fld  ) = v_pole(jnod,1)
          d_nod(inod,i_fld+1) = v_pole(jnod,2)
          d_nod(inod,i_fld+2) = v_pole(jnod,3)
          d_nod(inod,i_fld+3) = v_pole(jnod,4)
          d_nod(inod,i_fld+4) = v_pole(jnod,5)
          d_nod(inod,i_fld+5) = v_pole(jnod,6)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      jnod = 2 * nidx_rtp_gl_r + 1
      d_nod(inod,i_fld:i_fld+5) = v_pole(jnod,1:6)
!
      end subroutine copy_pole_tensor_from_trans
!
! -----------------------------------------------------------------------
!
      end module copy_pole_field_sph_trans
