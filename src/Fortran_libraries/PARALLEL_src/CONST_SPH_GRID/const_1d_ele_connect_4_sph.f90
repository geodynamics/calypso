!const_1d_ele_connect_4_sph.f90
!      module const_1d_ele_connect_4_sph
!
!     Written by H. Matsui on March, 2012
!
!!      subroutine s_const_1d_ele_connect_4_sph                         &
!!     &         (iflag_shell_mode, m_folding, sph_rtp,                 &
!!     &          s3d_ranks, stk_lc1d, sph_gl1d, stbl)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(comm_table_make_sph), intent(inout) :: stbl
!
      module const_1d_ele_connect_4_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use t_sph_mesh_1d_connect
      use t_sph_1d_global_index
!
      implicit none
!
      private :: count_nod_ele_4_sph_radial, count_nod_ele_4_sph_theta
      private :: set_iele_4_sph_radial, set_iele_4_sph_theta
      private :: set_iele_4_sph_phi
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_1d_ele_connect_4_sph                           &
     &         (iflag_shell_mode, m_folding, sph_rtp,                   &
     &          s3d_ranks, stk_lc1d, sph_gl1d, stbl)
!
      use t_spheric_global_ranks
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: iflag_shell_mode
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
!
      call alloc_nnod_nele_sph_mesh                                     &
     &   (s3d_ranks%ndomain_sph, s3d_ranks%ndomain_rtp,                 &
     &    sph_rtp%nidx_global_rtp, m_folding, stbl)
!
      if(iflag_debug .gt. 0) write(*,*) 'count_nod_ele_4_sph_radial'
      call count_nod_ele_4_sph_radial                                   &
     &   (iflag_shell_mode, stk_lc1d, sph_gl1d, stbl)
      if(iflag_debug .gt. 0) write(*,*) 'count_nod_ele_4_sph_theta'
      call count_nod_ele_4_sph_theta(iflag_shell_mode, stk_lc1d, stbl)
!
      call alloc_iele_sph_mesh(stbl)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_iele_4_sph_radial'
      call set_iele_4_sph_radial(stbl)
      if(iflag_debug .gt. 0) write(*,*) 'set_iele_4_sph_theta'
      call set_iele_4_sph_theta(stbl)
      if(iflag_debug .gt. 0) write(*,*) 'set_iele_4_sph_phi'
      call set_iele_4_sph_phi(stbl)
!
      if(iflag_debug .gt. 1) call check_iele_4_sph_connects(stbl)
!
      end subroutine s_const_1d_ele_connect_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_nod_ele_4_sph_radial                             &
     &         (iflag_shell_mode, stk_lc1d, sph_gl1d, stbl)
!
      integer(kind = kint), intent(in) :: iflag_shell_mode
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: k, kr, ip, jp, ist, ied
!
!
      stbl%iflag_center_r =   0
      stbl%iflag_internal_r = 0
      stbl%iflag_ele_r =      0
      stbl%nmax_nod_sph_r =   0
      stbl%nmax_ele_sph_r =   0
      do ip = 1, stbl%ndomain_fem(1)
        ist = stk_lc1d%istack_idx_local_rtp_r(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_r(ip)
        do k = ist, ied
          kr = sph_gl1d%idx_global_rtp_r(k)
          stbl%iflag_internal_r(kr,ip) = 1
!
          if(kr.eq.ione) then
            if(iflag_shell_mode .eq. iflag_MESH_w_center) then
              stbl%iflag_center_r(ip) = 1
            end if
          else
            if(stbl%iflag_internal_r(kr-1,ip) .eq. 0) then
              stbl%iflag_internal_r(kr-1,ip) = -1
            end if
          end if
!
          if(kr .lt. stbl%nidx_global_fem(1)                            &
     &        .and. stbl%iflag_internal_r(kr+1,ip) .eq. 0) then
            stbl%iflag_internal_r(kr+1,ip) = -1
          end if
        end do
!
        stbl%iflag_ele_center(ip) = abs(stbl%iflag_center_r(ip)         &
     &                        * stbl%iflag_internal_r(1,ip))
        stbl%nnod_sph_r(ip) = abs(stbl%iflag_internal_r(1,ip))
        stbl%nele_sph_r(ip) = 0
        do kr = 1, stbl%nidx_global_fem(1)-1
          stbl%iflag_ele_r(kr,ip) = abs(stbl%iflag_internal_r(kr,ip)    &
     &                             * stbl%iflag_internal_r(kr+1,  ip))
          stbl%nnod_sph_r(ip) = stbl%nnod_sph_r(ip)                     &
     &                         + abs(stbl%iflag_internal_r(kr+1,ip))
          stbl%nele_sph_r(ip) = stbl%nele_sph_r(ip)                     &
     &                         + abs(stbl%iflag_ele_r(kr,ip))
        end do
        stbl%nmax_nod_sph_r                                             &
     &        = max(stbl%nmax_nod_sph_r,stbl%nnod_sph_r(ip))
        stbl%nmax_ele_sph_r                                             &
     &        = max(stbl%nmax_ele_sph_r,stbl%nele_sph_r(ip))
      end do
!
      do ip = 1, stbl%ndomain_fem(1)
        stbl%iflag_neib_r(ip,ip) = 1
      end do
!
      do kr = 1, stbl%nidx_global_fem(1)
        do jp = 1, stbl%ndomain_fem(1)
          if(stbl%iflag_internal_r(kr,jp) .eq. 1) then
            ip = jp
            exit
          end if
        end do
        do jp = 1, stbl%ndomain_fem(1)
          stbl%iflag_internal_r(kr,jp)                                  &
     &                     = stbl%iflag_internal_r(kr,jp) * ip
        end do
        do jp = 1, stbl%ndomain_fem(1)
          if(stbl%iflag_internal_r(kr,jp) .lt. 0) then
            ip = abs(stbl%iflag_internal_r(kr,jp))
            stbl%iflag_neib_r(ip,jp) = -1
          end if
        end do
      end do
!
      do jp = 1, stbl%ndomain_fem(1)
        stbl%iflag_center_r(jp) = stbl%iflag_center_r(jp) * jp
      end do
      do jp = 1, stbl%ndomain_fem(1)
        if(stbl%iflag_center_r(jp) .lt. 0) then
          ip = abs(stbl%iflag_center_r(jp))
          stbl%iflag_neib_r(ip,jp) = -1
        end if
      end do
!
      end subroutine count_nod_ele_4_sph_radial
!
! ----------------------------------------------------------------------
!
      subroutine count_nod_ele_4_sph_theta                              &
     &         (iflag_shell_mode, stk_lc1d, stbl)
!
      integer(kind = kint), intent(in) :: iflag_shell_mode
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: k, ip, ist, ied, jp
!
!
      stbl%iflag_internal_t = 0
      stbl%iflag_ele_t =      0
      stbl%nmax_nod_sph_t =   0
      stbl%nmax_ele_sph_t =   0
      do ip = 1, stbl%ndomain_fem(2)
        ist = stk_lc1d%istack_idx_local_rtp_t(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_t(ip)
!
        if(ist .eq. ione) then
          if    (iflag_shell_mode .eq. iflag_MESH_w_pole                &
     &      .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
            stbl%iflag_Spole_t(ip) = 1
          end if
        else
          stbl%iflag_internal_t(ist-1,ip) = -1
        end if
!
        do k = ist, ied
          stbl%iflag_internal_t(k,ip) = 1
        end do
!
        if(ied .eq. stbl%nidx_global_fem(2)) then
          if    (iflag_shell_mode .eq. iflag_MESH_w_pole                &
     &      .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
            stbl%iflag_Npole_t(ip) = 1
          end if
        else
          stbl%iflag_internal_t(ied+1,ip) = -1
        end if
!
        stbl%iflag_ele_Spole(ip) = abs(stbl%iflag_Spole_t(ip)           &
     &                            * stbl%iflag_internal_t(1,ip))
        stbl%nnod_sph_t(ip) = abs(stbl%iflag_internal_t(1,ip))
        stbl%nele_sph_t(ip) = 0
!
        do k = 1, stbl%nidx_global_fem(2)-1
          stbl%iflag_ele_t(k,ip) = abs(stbl%iflag_internal_t(k,ip)      &
     &                            * stbl%iflag_internal_t(k+1,  ip))
          stbl%nnod_sph_t(ip) = stbl%nnod_sph_t(ip)                     &
     &                         + abs(stbl%iflag_internal_t(k+1,ip))
          stbl%nele_sph_t(ip) = stbl%nele_sph_t(ip)                     &
     &                         + abs(stbl%iflag_ele_t(k,ip))
        end do
!
        k = stbl%nidx_global_fem(2)
        stbl%iflag_ele_Npole(ip) = abs(stbl%iflag_internal_t(k,ip)      &
     &                            * stbl%iflag_Npole_t(ip))
!
        stbl%nmax_nod_sph_t                                             &
     &      = max(stbl%nmax_nod_sph_t,stbl%nnod_sph_t(ip))
        stbl%nmax_ele_sph_t                                             &
     &      = max(stbl%nmax_ele_sph_t,stbl%nele_sph_t(ip))
      end do
!
      stbl%nnod_sph_ct = 0
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        stbl%nnod_sph_ct = stbl%nidx_global_fem(2) - stbl%nnod_sph_t(1)
        stbl%iflag_center_t(0) = stbl%iflag_Spole_t(1)
        do k = 1, stbl%nidx_global_fem(2)
          stbl%iflag_center_t(k) = stbl%iflag_internal_t(k,1)
        end do
        stbl%iflag_center_t(stbl%nidx_global_fem(2)+1)                  &
     &       = stbl%iflag_Npole_t(1)
        do k = 0, stbl%nidx_global_fem(2)+1
          if(stbl%iflag_center_t(k) .eq. 0) stbl%iflag_center_t(k) = -1
        end do
      end if
!
!
      do ip = 1, stbl%ndomain_fem(2)
        stbl%iflag_neib_t(ip,ip) = 1
      end do
!
      do k = 1, stbl%nidx_global_fem(2)
        do jp = 1, stbl%ndomain_fem(2)
          if(stbl%iflag_internal_t(k,jp) .eq. 1) then
            ip = jp
            exit
          end if
        end do
        do jp = 1, stbl%ndomain_fem(2)
          stbl%iflag_internal_t(k,jp)                                   &
     &           = stbl%iflag_internal_t(k,jp) * ip
        end do
        stbl%iflag_center_t(k) =   stbl%iflag_center_t(k) * ip
        do jp = 1, stbl%ndomain_fem(2)
          if(stbl%iflag_internal_t(k,jp) .lt. 0) then
            ip = abs(stbl%iflag_internal_t(k,jp))
            stbl%iflag_neib_t(ip,jp) = -1
          end if
        end do
      end do
!
      do jp = 1, stbl%ndomain_fem(2)
        if(stbl%iflag_Spole_t(jp) .gt. 0) then
          ip = jp
          exit
        end if
      end do
      stbl%iflag_center_t(0) =   stbl%iflag_center_t(0) * ip
      do jp = 1, stbl%ndomain_fem(2)
        stbl%iflag_Spole_t(jp) = stbl%iflag_Spole_t(jp) * ip
      end do
!
      do jp = 1, stbl%ndomain_fem(2)
        if(stbl%iflag_Npole_t(jp) .gt. 0) then
          ip = jp
          exit
        end if
      end do
      stbl%iflag_center_t(stbl%nidx_global_fem(2)+1)                    &
     &        =   stbl%iflag_center_t(stbl%nidx_global_fem(2)+1) * ip
      do jp = 1, stbl%ndomain_fem(2)
        stbl%iflag_Npole_t(jp) = stbl%iflag_Npole_t(jp) * ip
      end do
!
      end subroutine count_nod_ele_4_sph_theta
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_sph_radial(stbl)
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: kr, ip, icou
!
!
      do ip = 1, stbl%ndomain_fem(1)
        icou = 0
        do kr = 1, stbl%nidx_global_fem(1)
          if(stbl%iflag_internal_r(kr,ip) .ne. 0) then
            icou = icou + 1
            stbl%inod_sph_r(icou,ip) = kr
            stbl%irev_sph_r(kr,ip) = icou
          end if
        end do
        if(stbl%iflag_center_r(ip) .ne. 0) then
          stbl%inod_sph_r(0,ip) = 0
          stbl%irev_sph_r(0,ip) = 0
        end if
!
        icou = 0
        do kr = 1, stbl%nidx_global_fem(1)-1
          if(stbl%iflag_ele_r(kr,ip) .gt. 0) then
            icou = icou + 1
            stbl%ie_sph_r(icou,1,ip) = stbl%irev_sph_r(kr,  ip)
            stbl%ie_sph_r(icou,2,ip) = stbl%irev_sph_r(kr+1,ip)
          end if
        end do
!
        if(stbl%iflag_ele_center(ip) .gt. 0) then
          stbl%ie_center_r(1,ip) = stbl%irev_sph_r(0,ip)
          stbl%ie_center_r(2,ip) = stbl%irev_sph_r(1,ip)
        end if
      end do
!
      end subroutine set_iele_4_sph_radial
!
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_sph_theta(stbl)
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: k, ip, icou
!
!
      do ip = 1, stbl%ndomain_fem(2)
        icou = 0
        if(stbl%iflag_Spole_t(ip) .ne. 0) then
          stbl%inod_sph_t(0,ip) = 0
          stbl%irev_sph_t(0,ip) = 0
        end if
        do k = 1, stbl%nidx_global_fem(2)
          if(stbl%iflag_internal_t(k,ip) .ne. 0) then
            icou = icou + 1
            stbl%inod_sph_t(icou,ip) = k
            stbl%irev_sph_t(k,ip) = icou
          end if
        end do
        if(stbl%iflag_Npole_t(ip) .ne. 0) then
          icou = icou + 1
          stbl%inod_sph_t(icou,ip) = stbl%nidx_global_fem(2) + 1
          stbl%irev_sph_t(stbl%nidx_global_fem(2)+1,ip) = icou
        end if
!
        icou = 0
        do k = 1, stbl%nidx_global_fem(2)-1
          if(stbl%iflag_ele_t(k,ip) .gt. 0) then
            icou = icou + 1
            stbl%ie_sph_t(icou,1,ip) = stbl%irev_sph_t(k,  ip)
            stbl%ie_sph_t(icou,2,ip) = stbl%irev_sph_t(k+1,ip)
          end if
        end do
!
        if(stbl%iflag_ele_Spole(ip) .gt. 0) then
          stbl%ie_Spole_t(1,ip) = stbl%irev_sph_t(0,ip)
          stbl%ie_Spole_t(2,ip) = stbl%irev_sph_t(1,ip)
        end if
        if(stbl%iflag_ele_Npole(ip) .gt. 0) then
          stbl%ie_Npole_t(1,ip)                                         &
     &        = stbl%irev_sph_t(stbl%nidx_global_fem(2),  ip)
          stbl%ie_Npole_t(2,ip)                                         &
     &        = stbl%irev_sph_t(stbl%nidx_global_fem(2)+1,ip)
        end if
      end do
!
      icou = 0
      if       (stbl%iflag_center_t(0).lt.0                             &
     &    .and. stbl%iflag_Spole_t(1).eq.0) then
        icou = icou + 1
        stbl%inod_sph_ct(icou) = 0
        stbl%irev_sph_ct(0) = icou
      end if
!
      do k = 1, stbl%nidx_global_fem(2)
        if(stbl%iflag_internal_t(k,1) .eq. 0) then
          icou = icou + 1
          stbl%inod_sph_ct(icou) = k
          stbl%irev_sph_ct(k) = icou
        end if
      end do
      do k = 1, stbl%nidx_global_fem(2)-1
        if(stbl%irev_sph_t(k,1) .eq. 0) then
          stbl%ie_center_t(k,1) = -stbl%irev_sph_ct(k  )
        else
          stbl%ie_center_t(k,1) = stbl%irev_sph_t(k,1)
        end if
!
        if(stbl%irev_sph_t(k+1,1) .eq. 0) then
          stbl%ie_center_t(k,2) = -stbl%irev_sph_ct(k+1)
        else
          stbl%ie_center_t(k,2) = stbl%irev_sph_t(k+1,1)
        end if
      end do
!
      stbl%ie_center_Sp(1) = stbl%ie_Spole_t(1,1)
      stbl%ie_center_Sp(2) = stbl%ie_Spole_t(2,1)
!
      if(stbl%iflag_Npole_t(1) .gt. 0)  then
        stbl%ie_center_Np(1) = stbl%ie_Npole_t(1,1)
        stbl%ie_center_Np(2) = stbl%ie_Npole_t(2,1)
      else
        stbl%ie_center_Np(1) = -stbl%nnod_sph_ct
        stbl%ie_center_Np(2) = -(stbl%nnod_sph_ct+1)
      end if
!
      k = stbl%nidx_global_fem(2)+1
      if      (stbl%iflag_center_t(k).lt.0                              &
     &   .and. stbl%iflag_Npole_t(1).eq.0) then
        icou = icou + 1
        stbl%inod_sph_ct(icou) = k
        stbl%irev_sph_ct(k) = icou
      end if
!
      end subroutine set_iele_4_sph_theta
!
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_sph_phi(stbl)
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: m
!
!
      do m = 1, stbl%nidx_global_fem(3)
        stbl%ie_sph_p(m,1) = m
        stbl%ie_sph_p(m,2) = mod(m,stbl%nidx_global_fem(3)) + 1
      end do
!
      end subroutine set_iele_4_sph_phi
!
! ----------------------------------------------------------------------
!
      end module const_1d_ele_connect_4_sph
