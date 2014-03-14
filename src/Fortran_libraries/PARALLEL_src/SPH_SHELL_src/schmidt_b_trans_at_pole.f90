!schmidt_b_trans_at_pole.f90
!      module schmidt_b_trans_at_pole
!
!     Written by H. Matsui on July, 2007
!
!
!      subroutine schmidt_b_trans_pole_scalar(nb)
!      subroutine schmidt_b_trans_pole_vect(nb)
!      subroutine schmidt_b_trans_pole_grad(nb)
!
!------------------------------------------------------------------
!
!      vr =  l*(l+1)*Y(l,0)* S(l,0) / r**2
!      vt =  (dYdt(l,1s)*dSdr(l,1s)
!           + dYdt(l,1c)*dSdr(l,1c))  / r
!         + cos(theta) * (d2Ydtdp(l,1s)*T(l,1s) 
!                       + d2Ydtdp(l,1c)*T(l,1c))  / r
!      vp = cos(theta) * (d2Ydtdp(l,1s)*dSdr(l,1s)
!                       + d2Ydtdp(l,1c)*dSdr(l,1c))  / r
!           -(dYdt(l,1s)*T(l,1s)
!           + dYdt(l,1c)*T(l,1c))  / r
!
!  if phi = 0
!
!      vr =  l*(l+1)*P(l,0)* S(l,0) / r**2
!      vt =  dPdt(l,1)*dSdr(l,1c)  / r
!         + cos(theta) * dPdt(l,1)*T(l,1s) / r
!      vp = cos(theta) * dPdt(l,1)*dSdr(l,1s) / r
!           - dPdt(l,1)*T(l,1c)  / r
!
! if z > 0 (North pole)
!
!      vx = vt
!      vy = vp
!      vz = vr
!
! if z < 0 (South pole)
!
!      vx = -vt
!      vy =  vp
!      vz = -vr
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
! if r= 0 (Center)
!
!      vz =  2 * P(1,0) * S(1,0) / r_c**2
!         =  2 * S(1,0) / r_c**2
!      vx =  2 * dPdt(l,1) * S(1,1c) / r_c**2
!         = - 2 * S(1,1c) / r_c**2
!      vy =  2 * dPdt(l,1) * S(1,1s) / r_c**2
!         = - 2 * S(1,1s) / r_c**2
!------------------------------------------------------------------
!
      module schmidt_b_trans_at_pole
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
!
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_pole_sph_trans
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine schmidt_b_trans_pole_scalar(nb)
!
      use calypso_mpi
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rlm, nd, kr_nd
      integer(kind = kint) :: jst, jed, j_rlm, i_rlm
!
!
      v_np_local = zero
      v_sp_local = zero
!
      if(ist_rtm_order_zero .le. 0) return
!!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
      do k_rlm = 1, nidx_rlm(1)
        do nd = 1, nb
          kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
          jst = ist_rtm_order_zero
          jed = ist_rtm_order_zero + l_truncation
          do j_rlm = jst, jed
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rlm-1) * nb * nidx_rlm(2)
            v_np_local(kr_nd) = v_np_local(kr_nd)                       &
     &                        + sp_rlm(i_rlm) * P_pole_rtm(1,j_rlm)
            v_sp_local(kr_nd) = v_sp_local(kr_nd)                       &
     &                        + sp_rlm(i_rlm) * P_pole_rtm(2,j_rlm)
          end do
        end do
      end do
!!$omp end parallel do
!
      end subroutine schmidt_b_trans_pole_scalar
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_pole_vect(nb)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rlm, nd, kr_nd
      integer(kind = kint) :: jst, jed, j_rlm, i_rlm, iflag
!
!
      v_np_local = zero
      v_sp_local = zero
      v_n_pole =   zero
      v_s_pole =   zero
!
      if(ist_rtm_order_zero .gt. 0) then
!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
            jst = ist_rtm_order_zero
            jed = ist_rtm_order_zero + l_truncation
            do j_rlm = jst, jed
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                   + (k_rlm-1) * nb * nidx_rlm(2)
              v_np_local(3*kr_nd-2) = v_np_local(3*kr_nd-2)             &
     &                           + sp_rlm(3*i_rlm-2)                    &
     &                            * P_pole_rtm(1,j_rlm)                 &
     &                            * g_sph_rlm(j_rlm,3)
              v_sp_local(3*kr_nd-2) = v_sp_local(3*kr_nd-2)             &
     &                           + sp_rlm(3*i_rlm-2)                    &
     &                            * P_pole_rtm(2,j_rlm)                 &
     &                            * g_sph_rlm(j_rlm,3)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      if(ist_rtm_order_1s .gt. 0) then
!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
            jst = ist_rtm_order_1s
            jed = ist_rtm_order_1s + l_truncation
            do j_rlm = jst, jed
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                   + (k_rlm-1) * nb * nidx_rlm(2)
              v_np_local(3*kr_nd-1) = v_np_local(3*kr_nd-1)             &
     &                              + sp_rlm(3*i_rlm  )                 &
     &                               * dPdt_pole_rtm(1,j_rlm)
              v_np_local(3*kr_nd  ) = v_np_local(3*kr_nd  )             &
     &                              + sp_rlm(3*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(1,j_rlm)
!
              v_sp_local(3*kr_nd-1) = v_sp_local(3*kr_nd-1)             &
     &                              - sp_rlm(3*i_rlm  )                 &
     &                               * dPdt_pole_rtm(2,j_rlm)
              v_sp_local(3*kr_nd  ) = v_sp_local(3*kr_nd  )             &
     &                              + sp_rlm(3*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      if(ist_rtm_order_1c .gt. 0) then
!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
            jst = ist_rtm_order_1c
            jed = ist_rtm_order_1c + l_truncation
            do j_rlm = jst, jed
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                   + (k_rlm-1) * nb * nidx_rlm(2)
              v_np_local(3*kr_nd-1) = v_np_local(3*kr_nd-1)             &
     &                              + sp_rlm(3*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(1,j_rlm)
              v_np_local(3*kr_nd  ) = v_np_local(3*kr_nd  )             &
     &                              - sp_rlm(3*i_rlm  )                 &
     &                               * dPdt_pole_rtm(1,j_rlm)
!
              v_sp_local(3*kr_nd-1) = v_sp_local(3*kr_nd-1)             &
     &                              + sp_rlm(3*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(2,j_rlm)
              v_sp_local(3*kr_nd  ) = v_sp_local(3*kr_nd  )             &
     &                              - sp_rlm(3*i_rlm  )                 &
     &                               * dPdt_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      iflag = ist_rtm_order_zero+ist_rtm_order_1s+ist_rtm_order_1c
      if(iflag .gt. 0) then
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
            v_np_local(3*kr_nd-2) = v_np_local(3*kr_nd-2)               &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
            v_np_local(3*kr_nd-1) = v_np_local(3*kr_nd-1)               &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_np_local(3*kr_nd  ) = v_np_local(3*kr_nd  )               &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_sp_local(3*kr_nd-2) = v_sp_local(3*kr_nd-2)               &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
            v_sp_local(3*kr_nd-1) = v_sp_local(3*kr_nd-1)               &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_sp_local(3*kr_nd  ) = v_sp_local(3*kr_nd  )               &
     &                      * a_r_1d_rlm_r(k_rlm)
          end do
        end do
      end if
!
      end subroutine schmidt_b_trans_pole_vect
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_pole_grad(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rlm, nd, kr_nd
      integer(kind = kint) :: jst, jed, j_rlm, i_rlm, iflag
!
!
!
      v_np_local = zero
      v_sp_local = zero
!
      if(ist_rtm_order_zero .gt. 0) then
!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
            jst = ist_rtm_order_zero
            jed = ist_rtm_order_zero + l_truncation
            do j_rlm = jst, jed
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                   + (k_rlm-1) * nb * nidx_rlm(2)
              v_np_local(3*kr_nd-2) = v_np_local(3*kr_nd-2)             &
     &                           + sp_rlm(2*i_rlm  )                    &
     &                            * P_pole_rtm(1,j_rlm)
              v_sp_local(3*kr_nd-2) = v_sp_local(3*kr_nd-2)             &
     &                           + sp_rlm(2*i_rlm  )                    &
     &                            * P_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      if(ist_rtm_order_1s .gt. 0) then
!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
            jst = ist_rtm_order_1s
            jed = ist_rtm_order_1s + l_truncation
            do j_rlm = jst, jed
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                   + (k_rlm-1) * nb * nidx_rlm(2)
              v_np_local(3*kr_nd  ) = v_np_local(3*kr_nd  )             &
     &                              + sp_rlm(2*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(1,j_rlm)
!
              v_sp_local(3*kr_nd  ) = v_sp_local(3*kr_nd  )             &
     &                              + sp_rlm(2*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      if(ist_rtm_order_1c .gt. 0) then
!$omp parallel do private(k_rlm,nd,kr_nd,jst,jed,j_rlm,i_rlm)
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
!
            jst = ist_rtm_order_1c
            jed = ist_rtm_order_1c + l_truncation
            do j_rlm = jst, jed
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                   + (k_rlm-1) * nb * nidx_rlm(2)
              v_np_local(3*kr_nd-1) = v_np_local(3*kr_nd-1)             &
     &                              + sp_rlm(2*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(1,j_rlm)
!
              v_sp_local(3*kr_nd-1) = v_sp_local(3*kr_nd-1)             &
     &                              + sp_rlm(2*i_rlm-1)                 &
     &                               * dPdt_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      iflag = ist_rtm_order_zero+ist_rtm_order_1s+ist_rtm_order_1c
      if(iflag .gt. 0) then
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nb
            kr_nd = nd + (idx_gl_1d_rtm_r(k_rlm)-1) * nb
            v_np_local(3*kr_nd-1) = v_np_local(3*kr_nd-1)               &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_np_local(3*kr_nd  ) = v_np_local(3*kr_nd  )               &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_sp_local(3*kr_nd-1) = v_sp_local(3*kr_nd-1)               &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_sp_local(3*kr_nd  ) = v_sp_local(3*kr_nd  )               &
     &                      * a_r_1d_rlm_r(k_rlm)
          end do
        end do
      end if
!
      end subroutine schmidt_b_trans_pole_grad
!
! -----------------------------------------------------------------------
!
      end module schmidt_b_trans_at_pole
