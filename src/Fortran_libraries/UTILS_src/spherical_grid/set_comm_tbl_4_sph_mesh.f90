!set_comm_tbl_4_sph_mesh.f90
!      module set_comm_tbl_4_sph_mesh
!
!     Written by H. Matsui on March, 2013
!
!      subroutine count_import_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,     &
!     &          nod_comm)
!      subroutine set_import_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,     &
!     &          icou, nod_comm)
!
!      subroutine count_export_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,     &
!     &          num_export)
!      subroutine set_export_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,     &
!     &          icou, nod_comm)
!
      module set_comm_tbl_4_sph_mesh
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
      subroutine count_import_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          num_import)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_import
!
      integer(kind = kint) :: lflag, kflag, k, l
      integer(kind = kint) :: knum, lnum
!
!
      do lnum = 1, nnod_sph_t(ip_t)
        l = inod_sph_t(lnum,ip_t)
        lflag = iflag_internal_t(l,ip_t)
        if(abs(lflag) .ne. jp_t) cycle
!
        do knum = 1, nnod_sph_r(ip_r)
          k = inod_sph_r(knum,ip_r)
          kflag = iflag_internal_r(k,ip_r)
          if(abs(kflag) .ne. jp_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            num_import = num_import + nidx_global_rtp(3)
          end if
        end do
      end do
!
      end subroutine count_import_4_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: lflag, kflag, k, l, m
      integer(kind = kint) :: knum, lnum, num_rl, ist
!
!
      ist = icou
      do lnum = 1, nnod_sph_t(ip_t)
        l = inod_sph_t(lnum,ip_t)
        lflag = iflag_internal_t(l,ip_t)
        if(abs(lflag) .ne. jp_t) cycle
!
        do knum = 1, nnod_sph_r(ip_r)
          k = inod_sph_r(knum,ip_r)
          kflag = iflag_internal_r(k,ip_r)
          if(abs(kflag) .ne. jp_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            icou = icou + 1
            item_import_1d_rtp(1,icou) = knum
            item_import_1d_rtp(2,icou) = lnum
            item_import_1d_rtp(3,icou) = 1
!
            nod_comm%item_import(icou)                                  &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &          item_import_1d_rtp(1,icou), item_import_1d_rtp(2,icou), &
     &          item_import_1d_rtp(3,icou))
          end if
        end do
      end do
      num_rl = icou - ist
!
      do m = 2, nidx_global_rtp(3)
        do k = 1, num_rl
          icou = icou + 1
          item_import_1d_rtp(1,icou) = item_import_1d_rtp(1,k+ist)
          item_import_1d_rtp(2,icou) = item_import_1d_rtp(2,k+ist)
          item_import_1d_rtp(3,icou) = m
          nod_comm%item_import(icou)                                    &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &          item_import_1d_rtp(1,icou), item_import_1d_rtp(2,icou), &
     &          item_import_1d_rtp(3,icou))
        end do
      end do
!
      end subroutine set_import_rtp_shell_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_export_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          num_export)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) ::  ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
      integer(kind = kint) :: lflag, kflag, k, l
      integer(kind = kint) :: knum, lnum
!
!
      do lnum = 1, nnod_sph_t(jp_t)
        l = inod_sph_t(lnum,jp_t)
        lflag = iflag_internal_t(l,jp_t)
        if(abs(lflag) .ne. ip_t) cycle
!
        do knum = 1, nnod_sph_r(jp_r)
          k = inod_sph_r(knum,jp_r)
          kflag = iflag_internal_r(k,jp_r)
          if(abs(kflag) .ne. ip_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            num_export = num_export + nidx_global_rtp(3)
          end if
        end do
      end do
!
      end subroutine count_export_4_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_sph_local_node
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: lflag, kflag, k, l, m
      integer(kind = kint) :: knum, lnum, num_rl, ist
!
!
      ist = icou
      do lnum = 1, nnod_sph_t(jp_t)
        l = inod_sph_t(lnum,jp_t)
        lflag = iflag_internal_t(l,jp_t)
        if(abs(lflag) .ne. ip_t) cycle
!
        do knum = 1, nnod_sph_r(jp_r)
          k = inod_sph_r(knum,jp_r)
          kflag = iflag_internal_r(k,jp_r)
          if(abs(kflag) .ne. ip_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            icou = icou + 1
            item_export_1d_rtp(1,icou) = irev_sph_r(k,ip_r)
            item_export_1d_rtp(2,icou) = irev_sph_t(l,ip_t)
            item_export_1d_rtp(3,icou) = 1
!
            nod_comm%item_export(icou)                                  &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &          item_export_1d_rtp(1,icou), item_export_1d_rtp(2,icou), &
     &          item_export_1d_rtp(3,icou))
          end if
        end do
      end do
      num_rl = icou - ist
!
      do m = 2, nidx_global_rtp(3)
        do k = 1, num_rl
          icou = icou + 1
          item_export_1d_rtp(1,icou) = item_export_1d_rtp(1,k+ist)
          item_export_1d_rtp(2,icou) = item_export_1d_rtp(2,k+ist)
          item_export_1d_rtp(3,icou) = m
          nod_comm%item_export(icou)                                    &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &          item_export_1d_rtp(1,icou), item_export_1d_rtp(2,icou), &
     &          item_export_1d_rtp(3,icou))
        end do
      end do
!
      end subroutine set_export_rtp_shell_mesh
!
! -----------------------------------------------------------------------
!
      end module set_comm_tbl_4_sph_mesh
