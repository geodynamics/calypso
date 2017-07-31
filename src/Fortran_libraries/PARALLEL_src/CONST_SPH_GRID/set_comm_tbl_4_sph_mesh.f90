!set_comm_tbl_4_sph_mesh.f90
!      module set_comm_tbl_4_sph_mesh
!
!     Written by H. Matsui on March, 2013
!
!!      subroutine count_import_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          stbl, num_import)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!      subroutine set_import_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          icou, stbl, nod_comm)
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(communication_table), intent(inout) :: nod_comm
!!
!!      subroutine count_export_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          stbl, num_export)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!      subroutine set_export_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          icou, stbl, nod_comm)
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(communication_table), intent(inout) :: nod_comm
!
      module set_comm_tbl_4_sph_mesh
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_sph_mesh_1d_connect
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
     &          stbl, num_import)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_import
!
      integer(kind = kint) :: lflag, kflag, k, l
      integer(kind = kint) :: knum, lnum
!
!
      do lnum = 1, stbl%nnod_sph_t(ip_t)
        l = stbl%inod_sph_t(lnum,ip_t)
        lflag = stbl%iflag_internal_t(l,ip_t)
        if(abs(lflag) .ne. jp_t) cycle
!
        do knum = 1, stbl%nnod_sph_r(ip_r)
          k = stbl%inod_sph_r(knum,ip_r)
          kflag = stbl%iflag_internal_r(k,ip_r)
          if(abs(kflag) .ne. jp_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            num_import = num_import + stbl%nidx_global_fem(3)
          end if
        end do
      end do
!
      end subroutine count_import_4_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          icou, stbl, nod_comm)
!
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
!
      integer(kind = kint), intent(inout) :: icou
      type(comm_table_make_sph), intent(inout) :: stbl
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: lflag, kflag, k, l, m
      integer(kind = kint) :: knum, lnum, num_rl, ist
!
!
      ist = icou
      do lnum = 1, stbl%nnod_sph_t(ip_t)
        l = stbl%inod_sph_t(lnum,ip_t)
        lflag = stbl%iflag_internal_t(l,ip_t)
        if(abs(lflag) .ne. jp_t) cycle
!
        do knum = 1, stbl%nnod_sph_r(ip_r)
          k = stbl%inod_sph_r(knum,ip_r)
          kflag = stbl%iflag_internal_r(k,ip_r)
          if(abs(kflag) .ne. jp_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            icou = icou + 1
            stbl%item_import_1d_rtp(1,icou) = knum
            stbl%item_import_1d_rtp(2,icou) = lnum
            stbl%item_import_1d_rtp(3,icou) = 1
!
            nod_comm%item_import(icou)                                  &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &                             stbl%item_import_1d_rtp(1,icou),     &
     &                             stbl%item_import_1d_rtp(2,icou),     &
     &                             stbl%item_import_1d_rtp(3,icou),     &
     &                             stbl)
          end if
        end do
      end do
      num_rl = icou - ist
!
      do m = 2, stbl%nidx_global_fem(3)
        do k = 1, num_rl
          icou = icou + 1
          stbl%item_import_1d_rtp(1,icou)                               &
     &                           = stbl%item_import_1d_rtp(1,k+ist)
          stbl%item_import_1d_rtp(2,icou)                               &
     &                           = stbl%item_import_1d_rtp(2,k+ist)
          stbl%item_import_1d_rtp(3,icou) = m
          nod_comm%item_import(icou)                                    &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &                             stbl%item_import_1d_rtp(1,icou),     &
     &                             stbl%item_import_1d_rtp(2,icou),     &
     &                             stbl%item_import_1d_rtp(3,icou),     &
     &                             stbl)
        end do
      end do
!
      end subroutine set_import_rtp_shell_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_export_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          stbl, num_export)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) ::  ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
      integer(kind = kint) :: lflag, kflag, k, l
      integer(kind = kint) :: knum, lnum
!
!
      do lnum = 1, stbl%nnod_sph_t(jp_t)
        l = stbl%inod_sph_t(lnum,jp_t)
        lflag = stbl%iflag_internal_t(l,jp_t)
        if(abs(lflag) .ne. ip_t) cycle
!
        do knum = 1, stbl%nnod_sph_r(jp_r)
          k = stbl%inod_sph_r(knum,jp_r)
          kflag = stbl%iflag_internal_r(k,jp_r)
          if(abs(kflag) .ne. ip_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            num_export = num_export + stbl%nidx_global_fem(3)
          end if
        end do
      end do
!
      end subroutine count_export_4_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          icou, stbl, nod_comm)
!
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
!
      integer(kind = kint), intent(inout) :: icou
      type(comm_table_make_sph), intent(inout) :: stbl
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: lflag, kflag, k, l, m
      integer(kind = kint) :: knum, lnum, num_rl, ist
!
!
      ist = icou
      do lnum = 1, stbl%nnod_sph_t(jp_t)
        l = stbl%inod_sph_t(lnum,jp_t)
        lflag = stbl%iflag_internal_t(l,jp_t)
        if(abs(lflag) .ne. ip_t) cycle
!
        do knum = 1, stbl%nnod_sph_r(jp_r)
          k = stbl%inod_sph_r(knum,jp_r)
          kflag = stbl%iflag_internal_r(k,jp_r)
          if(abs(kflag) .ne. ip_r) cycle
!
          if(kflag.lt.izero .or. lflag.lt. izero) then
            icou = icou + 1
            stbl%item_export_1d_rtp(1,icou) = stbl%irev_sph_r(k,ip_r)
            stbl%item_export_1d_rtp(2,icou) = stbl%irev_sph_t(l,ip_t)
            stbl%item_export_1d_rtp(3,icou) = 1
!
            nod_comm%item_export(icou)                                  &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &                             stbl%item_export_1d_rtp(1,icou),     &
     &                             stbl%item_export_1d_rtp(2,icou),     &
     &                             stbl%item_export_1d_rtp(3,icou),     &
     &                             stbl)
          end if
        end do
      end do
      num_rl = icou - ist
!
      do m = 2, stbl%nidx_global_fem(3)
        do k = 1, num_rl
          icou = icou + 1
          stbl%item_export_1d_rtp(1,icou)                               &
     &                           = stbl%item_export_1d_rtp(1,k+ist)
          stbl%item_export_1d_rtp(2,icou)                               &
     &                           = stbl%item_export_1d_rtp(2,k+ist)
          stbl%item_export_1d_rtp(3,icou) = m
          nod_comm%item_export(icou)                                    &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &                             stbl%item_export_1d_rtp(1,icou),     &
     &                             stbl%item_export_1d_rtp(2,icou),     &
     &                             stbl%item_export_1d_rtp(3,icou),     &
     &                             stbl)
        end do
      end do
!
      end subroutine set_export_rtp_shell_mesh
!
! -----------------------------------------------------------------------
!
      end module set_comm_tbl_4_sph_mesh
