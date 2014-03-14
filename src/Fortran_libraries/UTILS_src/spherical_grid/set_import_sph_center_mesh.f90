!set_import_sph_center_mesh.f90
!      module set_import_sph_center_mesh
!
!     Written by H. Matsui on March, 2013
!
!      subroutine count_import_4_center_mesh(ip_r, ip_t, jp_r, jp_t,    &
!     &          num_import)
!      subroutine set_import_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          num_import)
!      subroutine count_import_4_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          num_import)
!      subroutine set_import_rtp_center_mesh(ip_r, ip_t, jp_r, jp_t,    &
!     &          icou, nod_comm)
!      subroutine set_import_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          icou, nod_comm)
!      subroutine set_import_rtp_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          icou, nod_comm)
!
      module set_import_sph_center_mesh
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_import_4_center_mesh(ip_r, ip_t, jp_r, jp_t,     &
     &          num_import)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_import
!
!
      if(iflag_center_r(ip_r) .eq. ip_r                                 &
     &  .and.  iflag_center_r(jp_r) .eq. ip_r                           &
     &  .and.  iflag_Spole_t(ip_t) .eq.  0                              &
     &  .and.  iflag_Spole_t(jp_t) .eq. jp_t) then
        num_import = num_import + 1
      end if
!
      end subroutine count_import_4_center_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_import_4_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          num_import)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_import
!
      integer(kind = kint) :: lnum, l
!
!
      if(iflag_center_r(ip_r) .eq. ip_r                                 &
     &  .and.  iflag_center_r(jp_r) .eq. ip_r                           &
     &  .and.  iflag_Spole_t(ip_t) .eq.  ip_t) then
!
        do lnum = 1, nnod_sph_ct
          l = inod_sph_ct(lnum)
          if(iflag_internal_t(l,jp_t) .eq. jp_t) then
            num_import = num_import + nidx_global_rtp(3)
          end if
        end do
      end if
!
      end subroutine count_import_4_ctr_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_import_4_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          num_import)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_import
!
!
      if(iflag_center_r(ip_r) .eq. ip_r                                 &
     &  .and.  iflag_center_r(jp_r) .eq. ip_r                           &
     &  .and.  iflag_Spole_t(ip_t) .eq.  ip_t                           &
     &  .and.  iflag_Npole_t(ip_t) .ne.  ip_t                           &
     &  .and.  iflag_Npole_t(jp_t) .eq.  jp_t) then
        num_import = num_import + 1
      end if
!
      end subroutine count_import_4_ctr_Npole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_center_mesh(ip_r, ip_t, jp_r, jp_t,     &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      if(iflag_center_r(ip_r) .eq. ip_r                                 &
     &  .and.  iflag_center_r(jp_r) .eq. ip_r                           &
     &  .and.  iflag_Spole_t(ip_t) .eq.  0                              &
     &  .and.  iflag_Spole_t(jp_t) .eq.  jp_t) then
        icou = icou + 1
        item_import_1d_rtp(1,icou) = 0
        item_import_1d_rtp(2,icou) = 0
        item_import_1d_rtp(3,icou) = 1
!
        nod_comm%item_import(icou) = sph_center_node_id()
      end if
!
      end subroutine set_import_rtp_center_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: lnum, l, m
      integer(kind = kint) :: ist, num_rl
!
!
      if(iflag_center_r(ip_r) .eq. ip_r                                 &
     &  .and.  iflag_center_r(jp_r) .eq. ip_r                           &
     &  .and.  iflag_Spole_t(ip_t) .eq.  ip_t) then
!
        ist = icou
        do lnum = 1, nnod_sph_ct
          l = inod_sph_ct(lnum)
          if(iflag_internal_t(l,jp_t) .eq. jp_t) then
!
            icou = icou + 1
            item_import_1d_rtp(1,icou) = irev_sph_r(ione,ip_r)
            item_import_1d_rtp(2,icou) = lnum
            item_import_1d_rtp(3,icou) = 1
!
            nod_comm%item_import(icou)                                  &
     &         = sph_ctr_shell_node_id(nnod_sph_ct,                     &
     &          item_import_1d_rtp(2,icou), item_import_1d_rtp(3,icou))
          end if
        end do
        num_rl = icou - ist
!
        do m = 2, nidx_global_rtp(3)
          do l = 1, num_rl
            icou = icou + 1
            item_import_1d_rtp(1,icou) = item_import_1d_rtp(1,l+ist)
            item_import_1d_rtp(2,icou) = item_import_1d_rtp(2,l+ist)
            item_import_1d_rtp(3,icou) = m
!
            nod_comm%item_import(icou)                                  &
     &         = sph_ctr_shell_node_id(nnod_sph_ct,                     &
     &          item_import_1d_rtp(2,icou), item_import_1d_rtp(3,icou))
          end do
        end do
      end if
!
      end subroutine set_import_rtp_ctr_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      if(iflag_center_r(ip_r) .eq. ip_r                                 &
     &  .and.  iflag_center_r(jp_r) .eq. ip_r                           &
     &  .and.  iflag_Spole_t(ip_t) .eq.  ip_t                           &
     &  .and.  iflag_Npole_t(ip_t) .ne.  ip_t                           &
     &  .and.  iflag_Npole_t(jp_t) .eq.  jp_t) then
!
        icou = icou + 1
        item_import_1d_rtp(1,icou) = irev_sph_r(ione,ip_r)
        item_import_1d_rtp(2,icou) = nnod_sph_t(ip_t)+1
        item_import_1d_rtp(3,icou) = 1
!
        nod_comm%item_import(icou) = sph_center_np_node_id()
      end if
!
      end subroutine set_import_rtp_ctr_Npole_mesh
!
! -----------------------------------------------------------------------
!
      end module set_import_sph_center_mesh
