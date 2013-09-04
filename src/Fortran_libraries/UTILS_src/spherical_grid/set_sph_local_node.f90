!set_sph_local_node.f90
!      module set_sph_local_node
!
!     Written by H. Matsui on March, 2012
!
!      subroutine count_numnod_local_sph_mesh(ip_r, ip_t, node)
!      subroutine set_local_nodes_sph_mesh(ip_r, ip_t, node)
!        type(node_data), intent(inout) :: node
!
      module set_sph_local_node
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      integer(kind = kint) :: intnod_shell =   0
      integer(kind = kint) :: intnod_Spole =   0
      integer(kind = kint) :: intnod_Npole =   0
      integer(kind = kint) :: intnod_center =  0
!
      integer(kind = kint) :: nnod_lc_shell =   0
      integer(kind = kint) :: nnod_lc_Spole =   0
      integer(kind = kint) :: nnod_lc_Npole =   0
      integer(kind = kint) :: nnod_lc_center =  0
      integer(kind = kint) :: nnod_lc_ctr_sph = 0
      integer(kind = kint) :: nnod_lc_ctr_Np =  0
!
      integer(kind = kint) :: nnod_gl_shell =   0
      integer(kind = kint) :: nnod_gl_Spole =   0
      integer(kind = kint) :: nnod_gl_Npole =   0
      integer(kind = kint) :: nnod_gl_center =  0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_numnod_local_sph_mesh(ip_r, ip_t, node)
!
      use t_geometry_data
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(node_data), intent(inout) :: node
!
!
      intnod_shell = nidx_rtp(1)*nidx_rtp(2)*nidx_rtp(3)
      nnod_lc_shell                                                     &
     &     = nnod_sph_r(ip_r)*nnod_sph_t(ip_t)*nidx_global_rtp(3)
      nnod_gl_shell                                                     &
     &     = nidx_global_rtp(1)*nidx_global_rtp(2)*nidx_global_rtp(3)
!
      intnod_Spole =   0
      intnod_Npole =   0
      intnod_center =  0
!
      nnod_lc_Spole =   0
      nnod_lc_Npole =   0
      nnod_lc_center =  0
      nnod_lc_ctr_sph = 0
      nnod_lc_ctr_Np =  0
!
      nnod_gl_Spole =   0
      nnod_gl_Npole =   0
      nnod_gl_center =  0
!
!  Count nodes for poles
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          intnod_Spole =  nidx_rtp(1)
          nnod_lc_Spole = nnod_sph_r(ip_r)
        end if
        nnod_gl_Spole = nidx_global_rtp(1)
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          intnod_Npole = nidx_rtp(1)
          nnod_lc_Npole = nnod_sph_r(ip_r)
        end if
        nnod_gl_Npole = nidx_global_rtp(1)
      end if
!
!  Count nodes for center
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(iflag_center_r(ip_r) .gt. 0) then
          if(iflag_Spole_t(ip_t) .gt. 0)  then
            intnod_center =  1
            nnod_lc_center = 1
            nnod_lc_ctr_sph = nnod_sph_ct*nidx_global_rtp(3)
            if(iflag_Npole_t(ip_t) .eq. 0) nnod_lc_ctr_Np = 1
          else
            nnod_lc_center =  1
          end if
        end if
!
        nnod_gl_center = 1
      end if
!
      node%numnod = nnod_lc_shell + nnod_lc_Spole + nnod_lc_Npole       &
     &        + nnod_lc_center + nnod_lc_ctr_sph + nnod_lc_ctr_Np
      node%internal_node = intnod_shell + intnod_Spole + intnod_Npole   &
     &               + intnod_center
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_of_nodes  Internal, subdomain, global'
        write(*,*) 'shell', intnod_shell, nnod_lc_shell, nnod_gl_shell
        write(*,*) 'N_pole', intnod_Npole, nnod_lc_Npole, nnod_gl_Npole
        write(*,*) 'S_pole', intnod_Spole, nnod_lc_Spole, nnod_gl_Spole
        write(*,*) 'center',                                            &
     &           intnod_center, nnod_lc_center, nnod_gl_center
        write(*,*) 'center shell', nnod_lc_ctr_sph, nnod_lc_ctr_Np
      end if
!
      end subroutine count_numnod_local_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_local_nodes_sph_mesh(ip_r, ip_t, node)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use m_gauss_points
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: knum, lnum, mnum
      integer(kind = kint) :: k, l, inod
!
      real(kind = kreal) :: pi
!
!
      pi = four*atan(one)
!
      do mnum = 1, nidx_global_rtp(3)
        do lnum = 1, nnod_sph_t(ip_t)
          l = inod_sph_t(lnum,ip_t)
          do knum = 1, nnod_sph_r(ip_r)
            k = inod_sph_r(knum,ip_r)
            inod = sph_shell_node_id(ip_r, ip_t, knum, lnum, mnum)
            node%inod_global(inod)                                      &
     &           = global_sph_shell_node_id(k, l, mnum)
            node%rr(inod) =     radius_1d_gl(k)
            node%theta(inod) =  w_colat(l)
            node%phi(inod) =  two*pi*dble(mnum-1)                       &
     &                         / dble(nidx_global_rtp(3))
          end do
        end do
      end do
!
!    Set nodes for poles
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set nodes for south pole
!
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          do knum = 1, nnod_sph_r(ip_r)
            k = inod_sph_r(knum,ip_r)
            inod = sph_s_pole_node_id(knum)
            node%inod_global(inod) = global_sph_s_pole_node_id(k)
!
            node%rr(inod) =     radius_1d_gl(k)
            node%theta(inod) = pi
            node%phi(inod) =  zero
          end do
        end if
!
!    Set nodes for north pole
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          do knum = 1, nnod_sph_r(ip_r)
            k = inod_sph_r(knum,ip_r)
            inod = sph_n_pole_node_id(knum)
            node%inod_global(inod) = global_sph_n_pole_node_id(k)
!
            node%rr(inod) =     radius_1d_gl(k)
            node%theta(inod) = zero
            node%phi(inod) =   zero
          end do
        end if
      end if
!
!     Set nodes at center
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(iflag_center_r(ip_r) .gt. 0)  then
          inod = sph_center_node_id()
          node%inod_global(inod) = global_sph_center_node_id()
!
          node%rr(inod) =     zero
          node%theta(inod) =  zero
          node%phi(inod) =    zero
!
          if(iflag_Spole_t(ip_t) .gt. 0)  then
            do mnum = 1, nidx_global_rtp(3)
              do lnum = 1, nnod_sph_ct
                l = inod_sph_ct(lnum)
                inod = sph_ctr_shell_node_id(nnod_sph_ct, lnum, mnum)
                node%inod_global(inod)                                  &
     &               = global_sph_shell_node_id(ione, l, mnum)
!
                node%rr(inod) =     radius_1d_gl(1)
                node%theta(inod) = w_colat(l)
                node%phi(inod) =  two*pi*dble(mnum-1)                   &
     &                         / dble(nidx_global_rtp(3))
              end do
            end do
!
            if(iflag_Npole_t(ip_t) .eq. 0)  then
              inod = sph_center_np_node_id()
              node%inod_global(inod) = global_sph_n_pole_node_id(ione)
!
              node%rr(inod) =    radius_1d_gl(1)
              node%theta(inod) = zero
              node%phi(inod) =   zero
            end if
          end if
        end if
      end if
!
      end subroutine set_local_nodes_sph_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function sph_shell_node_id(ip_r, ip_t, kr, lt, mp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      sph_shell_node_id = kr + (lt-1)*nnod_sph_r(ip_r)                  &
     &                   + (mp-1)*nnod_sph_r(ip_r)*nnod_sph_t(ip_t)
!
      end function sph_shell_node_id
!
! -----------------------------------------------------------------------
!
      integer function sph_s_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      sph_s_pole_node_id = kr + nnod_lc_shell
!
      end function sph_s_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer function sph_n_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      sph_n_pole_node_id = kr + nnod_lc_shell + nnod_lc_Spole
!
      end function sph_n_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer function sph_center_node_id()
!
      sph_center_node_id = 1 + nnod_lc_shell                            &
     &                    + nnod_lc_Spole + nnod_lc_Npole
!
      end function sph_center_node_id
!
! -----------------------------------------------------------------------
!
      integer function sph_ctr_shell_node_id(nnod_ct, lt, mp)
!
      integer(kind = kint), intent(in) :: nnod_ct, lt, mp
!
!
      sph_ctr_shell_node_id = lt + (mp-1) * nnod_ct                     &
     &       + nnod_lc_shell + nnod_lc_Spole + nnod_lc_Npole            &
     &        + nnod_lc_center
!
      end function sph_ctr_shell_node_id
!
! -----------------------------------------------------------------------
!
      integer function sph_center_np_node_id()
!
!
      sph_center_np_node_id                                             &
     &       = nnod_lc_shell + nnod_lc_Spole + nnod_lc_Npole            &
     &        + nnod_lc_center + nnod_lc_ctr_sph +  1
!
      end function sph_center_np_node_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function global_sph_shell_node_id(kr, lt, mp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      global_sph_shell_node_id = kr + (lt-1)*nidx_global_rtp(1)         &
     &                   + (mp-1)*nidx_global_rtp(1)*nidx_global_rtp(2)
!
      end function global_sph_shell_node_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_s_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      global_sph_s_pole_node_id = kr + nnod_gl_shell
!
      end function global_sph_s_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_n_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      global_sph_n_pole_node_id = kr + nnod_gl_shell + nnod_gl_Spole
!
      end function global_sph_n_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer function global_sph_center_node_id()
!
!
      global_sph_center_node_id = 1 + nnod_gl_shell                     &
     &                           + nnod_gl_Spole + nnod_gl_Npole
!
      end function global_sph_center_node_id
!
! -----------------------------------------------------------------------
!
      end module set_sph_local_node
