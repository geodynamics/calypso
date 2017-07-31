!set_comm_tbl_4_pole_mesh.f90
!      module set_comm_tbl_4_pole_mesh
!
!     Written by H. Matsui on March, 2013
!
!!      subroutine count_import_4_Spole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          stbl, num_import)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!      subroutine count_import_4_Npole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          stbl, num_import)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!      subroutine set_import_rtp_Spole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          icou, stbl, nod_comm)
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(communication_table), intent(inout) :: nod_comm
!!      subroutine set_import_rtp_Npole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          icou, stbl, nod_comm)
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(communication_table), intent(inout) :: nod_comm
!!
!!      subroutine count_export_4_Spole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          stbl, num_export)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!      subroutine count_export_4_Npole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          stbl, num_export)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!      subroutine set_export_rtp_Spole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          icou, stbl, nod_comm)
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(communication_table), intent(inout) :: nod_comm
!!      subroutine set_export_rtp_Npole_mesh(ip_r, ip_t, jp_r, jp_t,    &
!!     &          icou, stbl, nod_comm)
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(communication_table), intent(inout) :: nod_comm
!
      module set_comm_tbl_4_pole_mesh
!
      use m_precision
      use m_constants
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
      subroutine count_import_4_Spole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          stbl, num_import)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
!
      integer(kind = kint), intent(inout) :: num_import
!
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Spole_t(ip_t)
      if(abs(lflag) .ne. jp_t) return
!
      do knum = 1, stbl%nnod_sph_r(ip_r)
        k = stbl%inod_sph_r(knum,ip_r)
        kflag = stbl%iflag_internal_r(k,ip_r)
        if(abs(kflag) .ne. jp_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          num_import = num_import + 1
        end if
      end do
!
      end subroutine count_import_4_Spole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_import_4_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          stbl, num_import)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_import
!
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Npole_t(ip_t)
      if(abs(lflag) .ne. jp_t) return
!
      do knum = 1, stbl%nnod_sph_r(ip_r)
        k = stbl%inod_sph_r(knum,ip_r)
        kflag = stbl%iflag_internal_r(k,ip_r)
        if(abs(kflag) .ne. jp_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          num_import = num_import + 1
        end if
      end do
!
      end subroutine count_import_4_Npole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_Spole_mesh(ip_r, ip_t, jp_r, jp_t,      &
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
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Spole_t(ip_t)
      if(abs(lflag) .ne. jp_t) return
!
      do knum = 1, stbl%nnod_sph_r(ip_r)
        k = stbl%inod_sph_r(knum,ip_r)
        kflag = stbl%iflag_internal_r(k,ip_r)
        if(abs(kflag) .ne. jp_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          icou = icou + 1
          stbl%item_import_1d_rtp(1,icou) = knum
          stbl%item_import_1d_rtp(2,icou) = 0
          stbl%item_import_1d_rtp(3,icou) = 1
!
          nod_comm%item_import(icou)                                    &
     &          = sph_s_pole_node_id(stbl%item_import_1d_rtp(1,icou))
        end if
      end do
!
      end subroutine set_import_rtp_Spole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
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
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Npole_t(ip_t)
      if(abs(lflag) .ne. jp_t) return
!
      do knum = 1, stbl%nnod_sph_r(ip_r)
        k = stbl%inod_sph_r(knum,ip_r)
        kflag = stbl%iflag_internal_r(k,ip_r)
        if(abs(kflag) .ne. jp_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          icou = icou + 1
          stbl%item_import_1d_rtp(1,icou) = knum
          stbl%item_import_1d_rtp(2,icou) = stbl%nnod_sph_t(ip_t) + 1
          stbl%item_import_1d_rtp(3,icou) = 1
!
          nod_comm%item_import(icou)                                    &
     &          = sph_n_pole_node_id(stbl%item_import_1d_rtp(1,icou))
        end if
      end do
!
      end subroutine set_import_rtp_Npole_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_export_4_Spole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          stbl, num_export)
!
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Spole_t(jp_t)
      if(abs(lflag) .ne. ip_t) return
!
      do knum = 1, stbl%nnod_sph_r(jp_r)
        k = stbl%inod_sph_r(knum,jp_r)
        kflag = stbl%iflag_internal_r(k,jp_r)
        if(abs(kflag) .ne. ip_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          num_export = num_export + 1
        end if
      end do
!
      end subroutine count_export_4_Spole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_export_4_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &          stbl, num_export)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Npole_t(jp_t)
      if(abs(lflag) .ne. ip_t) return
!
      do knum = 1, stbl%nnod_sph_r(jp_r)
        k = stbl%inod_sph_r(knum,jp_r)
        kflag = stbl%iflag_internal_r(k,jp_r)
        if(abs(kflag) .ne. ip_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          num_export = num_export + 1
        end if
      end do
!
      end subroutine count_export_4_Npole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_Spole_mesh(ip_r, ip_t, jp_r, jp_t,      &
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
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Spole_t(jp_t)
      if(abs(lflag) .ne. ip_t) return
!
      do knum = 1, stbl%nnod_sph_r(jp_r)
        k = stbl%inod_sph_r(knum,jp_r)
        kflag = stbl%iflag_internal_r(k,jp_r)
        if(abs(kflag) .ne. ip_r) cycle
!
        if(kflag.lt.izero .or. lflag.lt. izero) then
          icou = icou + 1
          stbl%item_export_1d_rtp(1,icou) = stbl%irev_sph_r(k,ip_r)
          stbl%item_export_1d_rtp(2,icou) = 0
          stbl%item_export_1d_rtp(3,icou) = 1
!
          nod_comm%item_export(icou)                                    &
     &          = sph_s_pole_node_id(stbl%item_export_1d_rtp(1,icou))
        end if
      end do
!
      end subroutine set_export_rtp_Spole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
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
      integer(kind = kint) :: lflag, kflag, k
      integer(kind = kint) :: knum
!
!
      lflag = stbl%iflag_Npole_t(jp_t)
      if(abs(lflag) .ne. ip_t) return
!
      do knum = 1, stbl%nnod_sph_r(jp_r)
        k = stbl%inod_sph_r(knum,jp_r)
        kflag = stbl%iflag_internal_r(k,jp_r)
        if(abs(kflag) .ne. ip_r) cycle
!
       if(kflag.lt.izero .or. lflag.lt. izero) then
          icou = icou + 1
          stbl%item_export_1d_rtp(1,icou) = stbl%irev_sph_r(k,ip_r)
          stbl%item_export_1d_rtp(2,icou) = stbl%nnod_sph_t(ip_t) + 1
          stbl%item_export_1d_rtp(3,icou) = 1
!
          nod_comm%item_export(icou)                                    &
     &          = sph_n_pole_node_id(stbl%item_export_1d_rtp(1,icou))
        end if
      end do
!
      end subroutine set_export_rtp_Npole_mesh
!
! -----------------------------------------------------------------------
!
      end module set_comm_tbl_4_pole_mesh
