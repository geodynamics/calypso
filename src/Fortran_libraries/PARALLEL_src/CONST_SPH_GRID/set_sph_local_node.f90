!set_sph_local_node.f90
!      module set_sph_local_node
!
!     Written by H. Matsui on March, 2012
!
!!      subroutine count_numnod_local_sph_mesh                          &
!!     &          (iflag_shell_mode, ip_r, ip_t, stbl, node)
!!      subroutine set_local_nodes_sph_mesh(iflag_shell_mode,           &
!!     &          ip_r, ip_t, num_colat, r_global, colat_gl, stbl, node)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(node_data), intent(inout) :: node
!
      module set_sph_local_node
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      use t_sph_mesh_1d_connect
      use t_geometry_data
!
      use cal_sph_node_addresses
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_numnod_local_sph_mesh                            &
     &          (iflag_shell_mode, ip_r, ip_t, stbl, node)
!
      integer(kind = kint), intent(in) :: iflag_shell_mode, ip_r, ip_t
      type(comm_table_make_sph), intent(in) :: stbl
      type(node_data), intent(inout) :: node
!
!
      call reset_local_sph_node_constants
      call set_intnod_shell(stbl)
      call set_nnod_lc_shell(ip_r, ip_t, stbl)
      call set_nnod_gl_shell(stbl)
!
!  Count nodes for poles
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          call set_intnod_Spole(stbl)
          call set_nnod_lc_Spole(stbl%nnod_sph_r(ip_r))
        end if
        call set_nnod_gl_Spole(stbl)
!
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          call set_intnod_Npole(stbl)
          call set_nnod_lc_Npole(stbl%nnod_sph_r(ip_r))
        end if
        call set_nnod_gl_Npole(stbl)
      end if
!
!  Count nodes for center
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        call set_nnod_gl_center
!
        if(stbl%iflag_center_r(ip_r) .gt. 0) then
          if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
            call set_intnod_center
            call set_nnod_lc_center(ione)
            call set_nnod_lc_ctr_sph(stbl%nnod_sph_ct, stbl)
            if(stbl%iflag_Npole_t(ip_t) .eq. 0) then
              call set_nnod_lc_ctr_Np(ione)
            end if
          else
            call set_nnod_lc_center(ione)
          end if
        end if
      end if
!
      call cal_sph_local_numnod(node%numnod, node%internal_node)
      if(iflag_debug .gt. 0) call check_local_sph_node_constants
!
      end subroutine count_numnod_local_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_local_nodes_sph_mesh(iflag_shell_mode,             &
     &          ip_r, ip_t, num_colat, r_global, colat_gl, stbl, node)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: iflag_shell_mode, ip_r, ip_t
      integer(kind = kint), intent(in) :: num_colat
      real(kind= kreal), intent(in) :: r_global(stbl%nidx_global_fem(1))
      real(kind= kreal), intent(in) :: colat_gl(num_colat)
!
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
      do mnum = 1, stbl%nidx_global_fem(3)
        do lnum = 1, stbl%nnod_sph_t(ip_t)
          l = stbl%inod_sph_t(lnum,ip_t)
          do knum = 1, stbl%nnod_sph_r(ip_r)
            k = stbl%inod_sph_r(knum,ip_r)
            inod                                                        &
     &         = sph_shell_node_id(ip_r, ip_t, knum, lnum, mnum, stbl)
            node%inod_global(inod)                                      &
     &         = global_sph_shell_node_id(k, l, mnum, stbl)
            node%rr(inod) =     r_global(k)
            node%theta(inod) =  colat_gl(l)
            node%phi(inod) =  two*pi*dble(mnum-1)                       &
     &                         / dble(stbl%nidx_global_fem(3))
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
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          do knum = 1, stbl%nnod_sph_r(ip_r)
            k = stbl%inod_sph_r(knum,ip_r)
            inod = sph_s_pole_node_id(knum)
            node%inod_global(inod) = global_sph_s_pole_node_id(k)
!
            node%rr(inod) =    r_global(k)
            node%theta(inod) = pi
            node%phi(inod) =  zero
          end do
        end if
!
!    Set nodes for north pole
!
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          do knum = 1, stbl%nnod_sph_r(ip_r)
            k = stbl%inod_sph_r(knum,ip_r)
            inod = sph_n_pole_node_id(knum)
            node%inod_global(inod) = global_sph_n_pole_node_id(k)
!
            node%rr(inod) =    r_global(k)
            node%theta(inod) = zero
            node%phi(inod) =   zero
          end do
        end if
      end if
!
!     Set nodes at center
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(stbl%iflag_center_r(ip_r) .gt. 0)  then
          inod = sph_center_node_id()
          node%inod_global(inod) = global_sph_center_node_id()
!
          node%rr(inod) =     zero
          node%theta(inod) =  zero
          node%phi(inod) =    zero
!
          if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
            do mnum = 1, stbl%nidx_global_fem(3)
              do lnum = 1, stbl%nnod_sph_ct
                l = stbl%inod_sph_ct(lnum)
                inod = sph_ctr_shell_node_id(stbl%nnod_sph_ct,          &
     &                                       lnum, mnum)
                node%inod_global(inod)                                  &
     &                = global_sph_shell_node_id(ione, l, mnum, stbl)
!
                node%rr(inod) =    r_global(1)
                node%theta(inod) = colat_gl(l)
                node%phi(inod) =  two*pi*dble(mnum-1)                   &
     &                         / dble(stbl%nidx_global_fem(3))
              end do
            end do
!
            if(stbl%iflag_Npole_t(ip_t) .eq. 0)  then
              inod = sph_center_np_node_id()
              node%inod_global(inod) = global_sph_n_pole_node_id(ione)
!
              node%rr(inod) =    r_global(1)
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
!
      end module set_sph_local_node
