!const_comm_tbl_4_sph_mesh.f90
!      module const_comm_tbl_4_sph_mesh
!
!     Written by H. Matsui on March, 2013
!
!      subroutine count_neib_4_sph_mesh(ip_rank, ip_r, ip_t, nod_comm)
!      subroutine count_neib_4_sph_center_mesh(ip_rank, ip_r, ip_t,     &
!     &          nod_comm)
!
!      subroutine set_neib_4_sph_mesh(ip_rank, ip_r, ip_t, nod_comm)
!      subroutine set_neib_4_sph_center_mesh(ip_rank, ip_r, ip_t,       &
!     &          nod_comm)
!
!      subroutine count_import_4_sph_mesh(ip_r, ip_t, nod_comm)
!      subroutine set_import_rtp_sph_mesh(ip_r, ip_t, nod_comm)
!
!      subroutine count_export_4_sph_mesh(ip_r, ip_t, nod_comm)
!      subroutine set_export_rtp_sph_mesh(ip_r, ip_t, nod_comm)
!
      module const_comm_tbl_4_sph_mesh
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
      subroutine count_neib_4_sph_mesh(ip_rank, ip_r, ip_t, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_rank, ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: jp, j_rank
      integer(kind = kint) :: jp_r, jp_t
!
!
      nod_comm%num_neib = 0
      do jp = 1, ndomain_sph-1
        j_rank = mod((ip_rank+jp),ndomain_sph)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
        if(     iflag_neib_r(jp_r,ip_r).ne.izero                        &
     &    .and. iflag_neib_t(jp_t,ip_t).ne.izero) then
          nod_comm%num_neib = nod_comm%num_neib + 1
        end if
      end do
!
      end subroutine count_neib_4_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_neib_4_sph_center_mesh(ip_rank, ip_r, ip_t,      &
     &          nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_rank, ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: j_rank, jp
      integer(kind = kint) :: jp_r, jp_t
!
!
      if(iflag_center_r(ip_r) .eq. izero) return
!
      do jp = 1, ndomain_sph-1
        j_rank = mod((ip_rank+jp),ndomain_sph)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
        if(     iflag_neib_r(jp_r,ip_r).gt.izero                        &
     &    .and. iflag_neib_t(jp_t,ip_t).eq.izero) then
          nod_comm%num_neib = nod_comm%num_neib + 1
        end if
      end do
!
      end subroutine count_neib_4_sph_center_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_neib_4_sph_mesh(ip_rank, ip_r, ip_t, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_rank, ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: j_rank, jp, icou
      integer(kind = kint) :: jp_r, jp_t
!
!
      icou = 0
      do jp = 1, ndomain_sph-1
        j_rank = mod((ip_rank+jp),ndomain_sph)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
!
        if(     iflag_neib_r(jp_r,ip_r).ne.izero                        &
     &    .and. iflag_neib_t(jp_t,ip_t).ne.izero) then
          icou = icou + 1
          nod_comm%id_neib(icou) = j_rank
        end if
      end do
      nod_comm%num_neib = icou
!
      end subroutine set_neib_4_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_neib_4_sph_center_mesh(ip_rank, ip_r, ip_t,        &
     &          nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_comm_tbl_4_pole_mesh
!
      integer(kind = kint), intent(in) :: ip_rank, ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: jp, j_rank, jp_r, jp_t
      integer(kind = kint) :: icou
!
!
      if(iflag_center_r(ip_r) .eq. izero) return
!
      icou = nod_comm%num_neib
      do jp = 1, ndomain_sph-1
        j_rank = mod((ip_rank+jp),ndomain_sph)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
        if(     iflag_neib_r(jp_r,ip_r).gt.izero                        &
     &    .and. iflag_neib_t(jp_t,ip_t).eq.izero) then
          icou = icou + 1
          nod_comm%id_neib(icou) = j_rank
        end if
      end do
      nod_comm%num_neib = icou
!
      end subroutine set_neib_4_sph_center_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_import_4_sph_mesh(ip_r, ip_t, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_comm_tbl_4_sph_mesh
      use set_comm_tbl_4_pole_mesh
      use set_import_sph_center_mesh
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: jp, j_rank, jp_r, jp_t
!
!
      do jp = 1, nod_comm%num_neib
        j_rank = nod_comm%id_neib(jp)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
!
        nod_comm%num_import(jp) = 0
        call count_import_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      nod_comm%num_import(jp))
        call count_import_4_Spole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      nod_comm%num_import(jp))
        call count_import_4_Npole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      nod_comm%num_import(jp))
!
        call count_import_4_center_mesh(ip_r, ip_t, jp_r, jp_t,         &
     &      nod_comm%num_import(jp))
        call count_import_4_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      nod_comm%num_import(jp))
        call count_import_4_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      nod_comm%num_import(jp))
      end do
!
      call s_cal_total_and_stacks(nod_comm%num_neib,                    &
     &    nod_comm%num_import, izero, nod_comm%istack_import,           &
     &    nod_comm%ntot_import)
!
      end subroutine count_import_4_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_import_rtp_sph_mesh(ip_r, ip_t, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_comm_tbl_4_sph_mesh
      use set_comm_tbl_4_pole_mesh
      use set_import_sph_center_mesh
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: jp, j_rank, jp_r, jp_t
      integer(kind = kint) :: icou
!
!
      do jp = 1, nod_comm%num_neib
        j_rank = nod_comm%id_neib(jp)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
!
        icou = nod_comm%istack_import(jp-1)
        call set_import_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      icou, nod_comm)
        call set_import_rtp_Spole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      icou, nod_comm)
        call set_import_rtp_Npole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      icou, nod_comm)
!
        call set_import_rtp_center_mesh(ip_r, ip_t, jp_r, jp_t,         &
     &      icou, nod_comm)
        call set_import_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      icou, nod_comm)
        call set_import_rtp_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      icou, nod_comm)
      end do
!
      end subroutine set_import_rtp_sph_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_export_4_sph_mesh(ip_r, ip_t, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_comm_tbl_4_sph_mesh
      use set_comm_tbl_4_pole_mesh
      use set_export_sph_center_mesh
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: jp, j_rank, jp_r, jp_t
!
!
      do jp = 1, nod_comm%num_neib
        j_rank = nod_comm%id_neib(jp)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
!
        nod_comm%num_export(jp) = 0
        call count_export_4_shell_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      nod_comm%num_export(jp) )
        call count_export_4_Spole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      nod_comm%num_export(jp) )
        call count_export_4_Npole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      nod_comm%num_export(jp) )
!
        call count_export_4_center_mesh(ip_r, ip_t, jp_r, jp_t,         &
     &      nod_comm%num_export(jp) )
        call count_export_4_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      nod_comm%num_export(jp) )
        call count_export_4_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      nod_comm%num_export(jp) )
      end do
!
      call s_cal_total_and_stacks(nod_comm%num_neib,                    &
     &    nod_comm%num_export, izero, nod_comm%istack_export,           &
     &    nod_comm%ntot_export)
!
      end subroutine count_export_4_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_sph_mesh(ip_r, ip_t, nod_comm)
!
      use t_comm_table
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
      use set_comm_tbl_4_sph_mesh
      use set_comm_tbl_4_pole_mesh
      use set_export_sph_center_mesh
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: jp, j_rank, jp_r, jp_t
      integer(kind = kint) :: icou
!
!
      do jp = 1, nod_comm%num_neib
        j_rank = nod_comm%id_neib(jp)
        jp_r = iglobal_rank_rtp(1,j_rank) + 1
        jp_t = iglobal_rank_rtp(2,j_rank) + 1
!
        icou = nod_comm%istack_export(jp-1)
        call set_export_rtp_shell_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      icou, nod_comm)
        call set_export_rtp_Spole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      icou, nod_comm)
        call set_export_rtp_Npole_mesh(ip_r, ip_t, jp_r, jp_t,          &
     &      icou, nod_comm)
!
        call set_export_rtp_center_mesh(ip_r, ip_t, jp_r, jp_t,         &
     &      icou, nod_comm)
        call set_export_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      icou, nod_comm)
        call set_export_rtp_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,      &
     &      icou, nod_comm)
      end do
!
      end subroutine set_export_rtp_sph_mesh
!
! -----------------------------------------------------------------------
!
      end module const_comm_tbl_4_sph_mesh
