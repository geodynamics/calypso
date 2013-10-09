!copy_pole_field_sph_trans.f90
!     module copy_pole_field_sph_trans
!
!      subroutine copy_pole_vec_fld_from_trans(numnod, internal_node,   &
!     &          xx, ntot_phys, nscalar_trans, i_fld, i_trns, d_nod)
!      subroutine copy_pole_scl_fld_from_trans(numnod, internal_node,   &
!     &          xx, ntot_phys, nscalar_trans, i_fld, i_trns, d_nod)
!      subroutine copy_pole_tsr_fld_from_trans(numnod, internal_node,   &
!     &          xx, ntot_phys, ntensor_trans, i_fld, i_trns, d_nod)
!
!      subroutine copy_pole_vector_from_trans(numnod, internal_node, xx,&
!     &         ntot_phys, nnod_rtp, nidx_rtp_r, nvector_trans,         &
!     &         idx_gl_rtp_r, v_n_pole, v_s_pole, v_center,             &
!     &         i_fld, i_trns, d_nod)
!      subroutine copy_pole_scalar_from_trans(numnod, internal_node, xx,&
!     &         ntot_phys, nnod_rtp, nidx_rtp_r, nvector_trans,         &
!     &         idx_gl_rtp_r, i_fld, i_trns, d_nod)
!      subroutine copy_pole_tensor_from_trans(numnod, internal_node, xx,&
!     &         ntot_phys, nnod_rtp, nidx_rtp_r, ntensor_trans,         &
!     &         idx_gl_rtp_r, i_fld, i_trns, d_nod)
!
!      Written by H. Matsui on Feb., 2008
!
      module copy_pole_field_sph_trans
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use calypso_mpi
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
     &          xx, ntot_phys, nscalar_trans, i_fld, i_trns, d_nod)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: i_fld, i_trns
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
      if( (i_fld*i_trns) .le. 0) return
      call copy_pole_vector_from_trans(numnod, internal_node, xx,       &
     &    ntot_phys, nnod_rtp, nidx_rtp(1), nscalar_trans,              &
     &    idx_gl_1d_rtp_r, i_fld, i_trns, d_nod)
!
      end subroutine copy_pole_vec_fld_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_scl_fld_from_trans(numnod, internal_node,    &
     &          xx, ntot_phys, nscalar_trans, i_fld, i_trns, d_nod)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: i_fld, i_trns
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ntot_phys)
!
!
      if( (i_fld*i_trns) .le. 0) return
      call copy_pole_scalar_from_trans(numnod, internal_node, xx,       &
     &    ntot_phys, nnod_rtp, nidx_rtp(1), nscalar_trans,              &
     &    idx_gl_1d_rtp_r, i_fld, i_trns, d_nod)
!
      end subroutine copy_pole_scl_fld_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_tsr_fld_from_trans(numnod, internal_node,    &
     &          xx, ntot_phys, ntensor_trans, i_fld, i_trns, d_nod)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: ntensor_trans
      integer(kind = kint), intent(in) :: i_fld, i_trns
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod, ntot_phys)
!
!
      if( (i_fld*i_trns) .le. 0) return
      call copy_pole_tensor_from_trans(numnod, internal_node, xx,       &
     &    ntot_phys, nnod_rtp, nidx_rtp(1), ntensor_trans,              &
     &    idx_gl_1d_rtp_r, i_fld, i_trns, d_nod)
!
      end subroutine copy_pole_tsr_fld_from_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_pole_vector_from_trans(numnod, internal_node, xx, &
     &         ntot_phys, nnod_rtp, nidx_rtp_r, nvector_trans,          &
     &         idx_gl_rtp_r, i_fld, i_trns, d_nod)
!
      use m_work_pole_sph_trans
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp, nvector_trans
      integer(kind = kint), intent(in) :: idx_gl_rtp_r(nidx_rtp_r)
!
      integer(kind = kint), intent(in) :: i_fld, i_trns
!
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
          jnod = i_trns + (idx_gl_rtp_r(kr) - 1) * nvector_trans
          d_nod(inod,i_fld  ) = v_n_pole(3*jnod-2)
          d_nod(inod,i_fld+1) = v_n_pole(3*jnod-1)
          d_nod(inod,i_fld+2) = v_n_pole(3*jnod  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = i_trns + (idx_gl_rtp_r(kr) - 1) * nvector_trans
          d_nod(inod,i_fld  ) =  v_s_pole(3*jnod-2)
          d_nod(inod,i_fld+1) =  v_s_pole(3*jnod-1)
          d_nod(inod,i_fld+2) =  v_s_pole(3*jnod  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_nod(inod,i_fld  ) = v_center(3*i_trns-2)
      d_nod(inod,i_fld+1) = v_center(3*i_trns-1)
      d_nod(inod,i_fld+2) = v_center(3*i_trns  )
!
      end subroutine copy_pole_vector_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_scalar_from_trans(numnod, internal_node, xx, &
     &         ntot_phys, nnod_rtp, nidx_rtp_r, nvector_trans,          &
     &         idx_gl_rtp_r, i_fld, i_trns, d_nod)
!
      use m_work_pole_sph_trans
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp, nvector_trans
      integer(kind = kint), intent(in) :: idx_gl_rtp_r(nidx_rtp_r)
!
      integer(kind = kint), intent(in) :: i_fld, i_trns
!
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
          jnod = i_trns + (idx_gl_rtp_r(kr) - 1) * nvector_trans
          d_nod(inod,i_fld  ) = v_n_pole(jnod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = i_trns + (idx_gl_rtp_r(kr) - 1) * nvector_trans
          d_nod(inod,i_fld  ) = v_s_pole(jnod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_nod(inod,i_fld) = v_center(i_trns)
!
      end subroutine copy_pole_scalar_from_trans
!
! -----------------------------------------------------------------------
!
      subroutine copy_pole_tensor_from_trans(numnod, internal_node, xx, &
     &         ntot_phys, nnod_rtp, nidx_rtp_r, ntensor_trans,          &
     &         idx_gl_rtp_r, i_fld, i_trns, d_nod)
!
      use m_work_pole_sph_trans
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_phys
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp, ntensor_trans
      integer(kind = kint), intent(in) :: idx_gl_rtp_r(nidx_rtp_r)
!
      integer(kind = kint), intent(in) :: i_fld, i_trns
!
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
          jnod = i_trns + (idx_gl_rtp_r(kr) - 1) * ntensor_trans
          d_nod(inod,i_fld  ) = v_n_pole(6*jnod-5)
          d_nod(inod,i_fld+1) = v_n_pole(6*jnod-4)
          d_nod(inod,i_fld+2) = v_n_pole(6*jnod-3)
          d_nod(inod,i_fld+3) = v_n_pole(6*jnod-2)
          d_nod(inod,i_fld+4) = v_n_pole(6*jnod-1)
          d_nod(inod,i_fld+5) = v_n_pole(6*jnod  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          jnod = i_trns + (idx_gl_rtp_r(kr) - 1) * ntensor_trans
          d_nod(inod,i_fld  ) = v_s_pole(6*jnod-5)
          d_nod(inod,i_fld+1) = v_s_pole(6*jnod-4)
          d_nod(inod,i_fld+2) = v_s_pole(6*jnod-3)
          d_nod(inod,i_fld+3) = v_s_pole(6*jnod-2)
          d_nod(inod,i_fld+4) = v_s_pole(6*jnod-1)
          d_nod(inod,i_fld+5) = v_s_pole(6*jnod  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_nod(inod,i_fld  ) = v_center(6*i_trns-5)
      d_nod(inod,i_fld+1) = v_center(6*i_trns-4)
      d_nod(inod,i_fld+2) = v_center(6*i_trns-3)
      d_nod(inod,i_fld+3) = v_center(6*i_trns-2)
      d_nod(inod,i_fld+4) = v_center(6*i_trns-1)
      d_nod(inod,i_fld+5) = v_center(6*i_trns  )
!
      end subroutine copy_pole_tensor_from_trans
!
! -----------------------------------------------------------------------
!
      end module copy_pole_field_sph_trans
