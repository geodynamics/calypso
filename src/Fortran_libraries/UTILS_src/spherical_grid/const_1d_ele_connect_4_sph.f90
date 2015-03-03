!const_1d_ele_connect_4_sph.f90
!      module const_1d_ele_connect_4_sph
!
!     Written by H. Matsui on March, 2012
!
!      subroutine s_const_1d_ele_connect_4_sph
!
!      subroutine allocate_nnod_nele_sph_mesh
!      subroutine allocate_iele_sph_mesh
!      subroutine deallocate_nnod_nele_sph_mesh
!
!      subroutine count_nod_ele_4_sph_radial
!      subroutine count_nod_ele_4_sph_theta
!
      module const_1d_ele_connect_4_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_sph_mesh_1d_connect
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_1d_ele_connect_4_sph
!
      use m_spheric_parameter
!
!
      call allocate_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,        &
     &    nidx_global_rtp, m_folding)
!
      if(iflag_debug .gt. 0) write(*,*) 'count_nod_ele_4_sph_radial'
      call count_nod_ele_4_sph_radial
      if(iflag_debug .gt. 0) write(*,*) 'count_nod_ele_4_sph_theta'
      call count_nod_ele_4_sph_theta
!
      call allocate_iele_sph_mesh
!
      if(iflag_debug .gt. 0) write(*,*) 'set_iele_4_sph_radial'
      call set_iele_4_sph_radial
      if(iflag_debug .gt. 0) write(*,*) 'set_iele_4_sph_theta'
      call set_iele_4_sph_theta
      if(iflag_debug .gt. 0) write(*,*) 'set_iele_4_sph_phi'
      call set_iele_4_sph_phi
!
      if(iflag_debug .gt. 1) call check_iele_4_sph_connects
!
      end subroutine s_const_1d_ele_connect_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_nod_ele_4_sph_radial
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      integer(kind = kint) :: k, kr, ip, jp, ist, ied
!
!
      iflag_center_r =   0
      iflag_internal_r = 0
      iflag_ele_r =      0
      nmax_nod_sph_r =   0
      nmax_ele_sph_r =   0
      do ip = 1, ndomain_fem(1)
        ist = istack_idx_local_rtp_r(ip-1) + 1
        ied = istack_idx_local_rtp_r(ip)
        do k = ist, ied
          kr = idx_global_rtp_r(k)
          iflag_internal_r(kr,ip) = 1
!
          if(kr.eq.ione) then
            if(iflag_shell_mode .eq. iflag_MESH_w_center) then
              iflag_center_r(ip) = 1
            end if
          else
            if(iflag_internal_r(kr-1,ip) .eq. 0) then
              iflag_internal_r(kr-1,ip) = -1
            end if
          end if
!
          if(kr .lt. nidx_global_fem(1)                                 &
     &        .and. iflag_internal_r(kr+1,ip) .eq. 0) then
            iflag_internal_r(kr+1,ip) = -1
          end if
        end do
!
        iflag_ele_center(ip) = abs(iflag_center_r(ip)                   &
     &                        * iflag_internal_r(1,ip))
        nnod_sph_r(ip) = abs(iflag_internal_r(1,ip))
        nele_sph_r(ip) = 0
        do kr = 1, nidx_global_fem(1)-1
          iflag_ele_r(kr,ip) = abs(iflag_internal_r(kr,ip)              &
     &                           * iflag_internal_r(kr+1,  ip))
          nnod_sph_r(ip) = nnod_sph_r(ip)                               &
     &                   + abs(iflag_internal_r(kr+1,ip))
          nele_sph_r(ip)= nele_sph_r(ip) + abs(iflag_ele_r(kr,ip))
        end do
        nmax_nod_sph_r = max(nmax_nod_sph_r,nnod_sph_r(ip))
        nmax_ele_sph_r = max(nmax_ele_sph_r,nele_sph_r(ip))
      end do
!
      do ip = 1, ndomain_fem(1)
        iflag_neib_r(ip,ip) = 1
      end do
!
      do kr = 1, nidx_global_fem(1)
        do jp = 1, ndomain_fem(1)
          if(iflag_internal_r(kr,jp) .eq. 1) then
            ip = jp
            exit
          end if
        end do
        do jp = 1, ndomain_fem(1)
          iflag_internal_r(kr,jp) = iflag_internal_r(kr,jp) * ip
        end do
        do jp = 1, ndomain_fem(1)
          if(iflag_internal_r(kr,jp) .lt. 0) then
            ip = abs(iflag_internal_r(kr,jp))
            iflag_neib_r(ip,jp) = -1
          end if
        end do
      end do
!
      do jp = 1, ndomain_fem(1)
        iflag_center_r(jp) = iflag_center_r(jp) * jp
      end do
      do jp = 1, ndomain_fem(1)
        if(iflag_center_r(jp) .lt. 0) then
          ip = abs(iflag_center_r(jp))
          iflag_neib_r(ip,jp) = -1
        end if
      end do
!
      end subroutine count_nod_ele_4_sph_radial
!
! ----------------------------------------------------------------------
!
      subroutine count_nod_ele_4_sph_theta
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      integer(kind = kint) :: k, ip, ist, ied, jp
!
!
      iflag_internal_t = 0
      iflag_ele_t =      0
      nmax_nod_sph_t =   0
      nmax_ele_sph_t =   0
      do ip = 1, ndomain_fem(2)
        ist = istack_idx_local_rtp_t(ip-1) + 1
        ied = istack_idx_local_rtp_t(ip)
!
        if(ist .eq. ione) then
          if    (iflag_shell_mode .eq. iflag_MESH_w_pole                &
     &      .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
            iflag_Spole_t(ip) = 1
          end if
        else
          iflag_internal_t(ist-1,ip) = -1
        end if
!
        do k = ist, ied
          iflag_internal_t(k,ip) = 1
        end do
!
        if(ied .eq. nidx_global_fem(2)) then
          if    (iflag_shell_mode .eq. iflag_MESH_w_pole                &
     &      .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
            iflag_Npole_t(ip) = 1
          end if
        else
          iflag_internal_t(ied+1,ip) = -1
        end if
!
        iflag_ele_Spole(ip) = abs(iflag_Spole_t(ip)                     &
     &                       * iflag_internal_t(1,ip))
        nnod_sph_t(ip) = abs(iflag_internal_t(1,ip))
        nele_sph_t(ip) = 0
!
        do k = 1, nidx_global_fem(2)-1
          iflag_ele_t(k,ip) = abs(iflag_internal_t(k,ip)                &
     &                          * iflag_internal_t(k+1,  ip))
          nnod_sph_t(ip) = nnod_sph_t(ip)                               &
     &                          + abs(iflag_internal_t(k+1,ip))
          nele_sph_t(ip)= nele_sph_t(ip) + abs(iflag_ele_t(k,ip))
        end do
!
        k = nidx_global_fem(2)
        iflag_ele_Npole(ip) = abs(iflag_internal_t(k,ip)                &
     &                          * iflag_Npole_t(ip))
!
        nmax_nod_sph_t = max(nmax_nod_sph_t,nnod_sph_t(ip))
        nmax_ele_sph_t = max(nmax_ele_sph_t,nele_sph_t(ip))
      end do
!
      nnod_sph_ct = 0
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        nnod_sph_ct = nidx_global_fem(2) - nnod_sph_t(1)
        iflag_center_t(0) = iflag_Spole_t(1)
        do k = 1, nidx_global_fem(2)
          iflag_center_t(k) = iflag_internal_t(k,1)
        end do
        iflag_center_t(nidx_global_fem(2)+1) = iflag_Npole_t(1)
        do k = 0, nidx_global_fem(2)+1
          if(iflag_center_t(k) .eq. 0) iflag_center_t(k) = -1
        end do
      end if
!
!
      do ip = 1, ndomain_fem(2)
        iflag_neib_t(ip,ip) = 1
      end do
!
      do k = 1, nidx_global_fem(2)
        do jp = 1, ndomain_fem(2)
          if(iflag_internal_t(k,jp) .eq. 1) then
            ip = jp
            exit
          end if
        end do
        do jp = 1, ndomain_fem(2)
          iflag_internal_t(k,jp) = iflag_internal_t(k,jp) * ip
        end do
        iflag_center_t(k) =   iflag_center_t(k) * ip
        do jp = 1, ndomain_fem(2)
          if(iflag_internal_t(k,jp) .lt. 0) then
            ip = abs(iflag_internal_t(k,jp))
            iflag_neib_t(ip,jp) = -1
          end if
        end do
      end do
!
      do jp = 1, ndomain_fem(2)
        if(iflag_Spole_t(jp) .gt. 0) then
          ip = jp
          exit
        end if
      end do
      iflag_center_t(0) =   iflag_center_t(0) * ip
      do jp = 1, ndomain_fem(2)
        iflag_Spole_t(jp) = iflag_Spole_t(jp) * ip
      end do
!
      do jp = 1, ndomain_fem(2)
        if(iflag_Npole_t(jp) .gt. 0) then
          ip = jp
          exit
        end if
      end do
      iflag_center_t(nidx_global_fem(2)+1)                              &
     &           =   iflag_center_t(nidx_global_fem(2)+1) * ip
      do jp = 1, ndomain_fem(2)
        iflag_Npole_t(jp) = iflag_Npole_t(jp) * ip
      end do
!
      end subroutine count_nod_ele_4_sph_theta
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_sph_radial
!
      use m_sph_1d_global_index
!
      integer(kind = kint) :: kr, ip, icou
!
!
      do ip = 1, ndomain_fem(1)
        icou = 0
        do kr = 1, nidx_global_fem(1)
          if(iflag_internal_r(kr,ip) .ne. 0) then
            icou = icou + 1
            inod_sph_r(icou,ip) = kr
            irev_sph_r(kr,ip) = icou
          end if
        end do
        if(iflag_center_r(ip) .ne. 0) then
          inod_sph_r(0,ip) = 0
          irev_sph_r(0,ip) = 0
        end if
!
        icou = 0
        do kr = 1, nidx_global_fem(1)-1
          if(iflag_ele_r(kr,ip) .gt. 0) then
            icou = icou + 1
            ie_sph_r(icou,1,ip) = irev_sph_r(kr,  ip)
            ie_sph_r(icou,2,ip) = irev_sph_r(kr+1,ip)
          end if
        end do
!
        if(iflag_ele_center(ip) .gt. 0) then
          ie_center_r(1,ip) = irev_sph_r(0,ip)
          ie_center_r(2,ip) = irev_sph_r(1,ip)
        end if
      end do
!
      end subroutine set_iele_4_sph_radial
!
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_sph_theta
!
      use m_sph_1d_global_index
!
      integer(kind = kint) :: k, ip, icou
!
!
      do ip = 1, ndomain_fem(2)
        icou = 0
        if(iflag_Spole_t(ip) .ne. 0) then
          inod_sph_t(0,ip) = 0
          irev_sph_t(0,ip) = 0
        end if
        do k = 1, nidx_global_fem(2)
          if(iflag_internal_t(k,ip) .ne. 0) then
            icou = icou + 1
            inod_sph_t(icou,ip) = k
            irev_sph_t(k,ip) = icou
          end if
        end do
        if(iflag_Npole_t(ip) .ne. 0) then
          icou = icou + 1
          inod_sph_t(icou,ip) = nidx_global_fem(2) + 1
          irev_sph_t(nidx_global_fem(2)+1,ip) = icou
        end if
!
        icou = 0
        do k = 1, nidx_global_fem(2)-1
          if(iflag_ele_t(k,ip) .gt. 0) then
            icou = icou + 1
            ie_sph_t(icou,1,ip) = irev_sph_t(k,  ip)
            ie_sph_t(icou,2,ip) = irev_sph_t(k+1,ip)
          end if
        end do
!
        if(iflag_ele_Spole(ip) .gt. 0) then
          ie_Spole_t(1,ip) = irev_sph_t(0,ip)
          ie_Spole_t(2,ip) = irev_sph_t(1,ip)
        end if
        if(iflag_ele_Npole(ip) .gt. 0) then
          ie_Npole_t(1,ip) = irev_sph_t(nidx_global_fem(2),  ip)
          ie_Npole_t(2,ip) = irev_sph_t(nidx_global_fem(2)+1,ip)
        end if
      end do
!
      icou = 0
      if(iflag_center_t(0).lt.0 .and. iflag_Spole_t(1).eq.0) then
        icou = icou + 1
        inod_sph_ct(icou) = 0
        irev_sph_ct(0) = icou
      end if
!
      do k = 1, nidx_global_fem(2)
        if(iflag_internal_t(k,1) .eq. 0) then
          icou = icou + 1
          inod_sph_ct(icou) = k
          irev_sph_ct(k) = icou
        end if
      end do
      do k = 1, nidx_global_fem(2)-1
        if(irev_sph_t(k,1) .eq. 0) then
          ie_center_t(k,1) = -irev_sph_ct(k  )
        else
          ie_center_t(k,1) = irev_sph_t(k,1)
        end if
!
        if(irev_sph_t(k+1,1) .eq. 0) then
          ie_center_t(k,2) = -irev_sph_ct(k+1)
        else
          ie_center_t(k,2) = irev_sph_t(k+1,1)
        end if
      end do
!
      ie_center_Sp(1) = ie_Spole_t(1,1)
      ie_center_Sp(2) = ie_Spole_t(2,1)
!
      if(iflag_Npole_t(1) .gt. 0)  then
        ie_center_Np(1) = ie_Npole_t(1,1)
        ie_center_Np(2) = ie_Npole_t(2,1)
      else
        ie_center_Np(1) = -nnod_sph_ct
        ie_center_Np(2) = -(nnod_sph_ct+1)
      end if
!
      k = nidx_global_fem(2)+1
      if(iflag_center_t(k).lt.0 .and. iflag_Npole_t(1).eq.0) then
        icou = icou + 1
        inod_sph_ct(icou) = k
        irev_sph_ct(k) = icou
      end if
!
      end subroutine set_iele_4_sph_theta
!
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_sph_phi
!
      integer(kind = kint) :: m
!
!
      do m = 1, nidx_global_fem(3)
        ie_sph_p(m,1) = m
        ie_sph_p(m,2) = mod(m,nidx_global_fem(3)) + 1
      end do
!
      end subroutine set_iele_4_sph_phi
!
! ----------------------------------------------------------------------
!
      end module const_1d_ele_connect_4_sph
