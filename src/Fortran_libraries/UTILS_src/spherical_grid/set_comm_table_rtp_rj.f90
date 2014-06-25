!
!      module set_comm_table_rtp_rj
!
!     Written by H. Matsui on July, 2007
!
!      subroutine allocate_domain_sr_tmp
!      subroutine deallocate_domain_sr_tmp
!
!      subroutine count_comm_table_4_rj(ip_rank)
!      subroutine set_comm_table_4_rj(ip_rank, icou)
!      subroutine count_comm_table_4_rtp(ip_rank)
!      subroutine set_comm_table_4_rtp(ip_rank, icou)
!
!      subroutine set_comm_stack_rtp_rj(nneib_domain, id_domain,        &
!     &          istack_sr, ntot_item_sr)
!
      module set_comm_table_rtp_rj
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: id_domain_tmp(:)
      integer(kind = kint), allocatable :: nnod_sr_tmp(:)
      private :: id_domain_tmp, nnod_sr_tmp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_domain_sr_tmp
!
      use m_spheric_parameter
!
      allocate( id_domain_tmp(ndomain_sph) )
      allocate( nnod_sr_tmp(ndomain_sph) )
      id_domain_tmp = 0
      nnod_sr_tmp = 0
!
      end subroutine allocate_domain_sr_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_domain_sr_tmp
!
      deallocate( id_domain_tmp )
      deallocate( nnod_sr_tmp )
!
      end subroutine deallocate_domain_sr_tmp
!
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rj(ip_rank)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_parallel_sph_grids
!
      integer(kind = kint), intent(in) :: ip_rank
!
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, sph_para(ip_org)%sph_comms%comm_rlm%nneib_domain
          if(sph_para(ip_org)%sph_comms%comm_rlm%id_domain(jp)          &
     &       .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        nneib_domain_rj = nneib_domain_rj + 1
        id_domain_tmp(nneib_domain_rj) = id_org_rank
        nnod_sr_tmp(nneib_domain_rj)                                    &
     &     =  sph_para(ip_org)%sph_comms%comm_rlm%istack_sr(iflag_jp)   &
     &      - sph_para(ip_org)%sph_comms%comm_rlm%istack_sr(iflag_jp-1)
      end do
!
      end subroutine count_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rj(ip_rank, icou)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_parallel_sph_grids
      use load_data_for_sph_IO
      use set_local_index_table_sph
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(inout) :: icou
!
      integer(kind = kint) :: jst, jed, j, jnod, k_tmp, j_tmp
      integer(kind = kint) :: k_glb, j_glb, ip_org
      integer(kind = kint) :: ip1, jp, id_org_rank
      integer(kind = kint) :: iflag_jp
!
!
      call set_local_idx_table_rj
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, sph_para(ip_org)%sph_comms%comm_rlm%nneib_domain
          if(sph_para(ip_org)%sph_comms%comm_rlm%id_domain(jp)          &
     &         .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        if(iflag_memory_conserve_sph .gt. 0) then
          call input_modes_rlm_sph_trans(id_org_rank)
        else
!          write(*,*) 'copy_sph_rlm_grid_from_mem', ip_org
          call copy_sph_rlm_grid_from_mem(ip_org)
        end if
!
        jst = istack_sr_rlm(iflag_jp-1)+1
        jed = istack_sr_rlm(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = item_sr_rlm(j)
          k_glb = idx_global_rlm(jnod,1)
          j_glb = idx_global_rlm(jnod,2)
          k_tmp = idx_local_rj_r(k_glb)
          j_tmp = idx_local_rj_j(j_glb)
          item_sr_rj(icou) =  j_tmp + (k_tmp-1) * nidx_rj(2)
        end do
!
        call deallocate_sph_comm_item_rlm
        call deallocate_sph_1d_index_rlm
        call deallocate_spheric_param_rlm
      end do
!
      end subroutine set_comm_table_4_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rtp(ip_rank)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_parallel_sph_grids
!
      integer(kind = kint), intent(in) :: ip_rank
!
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, sph_para(ip_org)%sph_comms%comm_rtm%nneib_domain
          if(sph_para(ip_org)%sph_comms%comm_rtm%id_domain(jp)          &
     &        .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        nneib_domain_rtp = nneib_domain_rtp + 1
        id_domain_tmp(nneib_domain_rtp) = id_org_rank
        nnod_sr_tmp(nneib_domain_rtp)                                   &
     &     =  sph_para(ip_org)%sph_comms%comm_rtm%istack_sr(iflag_jp)   &
     &      - sph_para(ip_org)%sph_comms%comm_rtm%istack_sr(iflag_jp-1)
      end do
!
      end subroutine count_comm_table_4_rtp
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rtp(ip_rank, icou)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_parallel_sph_grids
      use load_data_for_sph_IO
      use set_local_index_table_sph
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(inout) :: icou
!
      integer(kind = kint) :: jst, jed, j, jnod
      integer(kind = kint) :: k_tmp, l_tmp, m_tmp, k_glb, l_glb, m_glb
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org, iflag_jp
!
!
      call set_local_idx_table_rtp
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, sph_para(ip_org)%sph_comms%comm_rtm%nneib_domain
          if(sph_para(ip_org)%sph_comms%comm_rtm%id_domain(jp)          &
     &        .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        if(iflag_memory_conserve_sph .gt. 0) then
          call input_geom_rtm_sph_trans(id_org_rank)
        else
          call copy_sph_rtm_grid_from_mem(ip_org)
        end if
!
        jst = istack_sr_rtm(iflag_jp-1)+1
        jed = istack_sr_rtm(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = item_sr_rtm(j)
          k_glb = idx_global_rtm(jnod,1)
          l_glb = idx_global_rtm(jnod,2)
          m_glb = idx_global_rtm(jnod,3)
          k_tmp = idx_local_rtp_r(k_glb)
          l_tmp = idx_local_rtp_t(l_glb)
          m_tmp = idx_local_rtp_p(m_glb)
          item_sr_rtp(icou) =  k_tmp + (l_tmp-1) * nidx_rtp(1)          &
     &                        + (m_tmp-1) * nidx_rtp(1) * nidx_rtp(2)
        end do
!
        call deallocate_sph_comm_item_rtm
        call deallocate_sph_1d_index_rtm
        call deallocate_spheric_param_rtm
      end do
!
      end subroutine set_comm_table_4_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_comm_stack_rtp_rj(nneib_domain, id_domain,         &
     &          istack_sr, ntot_item_sr)
!
      integer(kind = kint), intent(in) :: nneib_domain
      integer(kind = kint), intent(inout) :: ntot_item_sr
      integer(kind = kint), intent(inout) :: id_domain(nneib_domain)
      integer(kind = kint), intent(inout) :: istack_sr(0:nneib_domain)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nneib_domain
        id_domain(ip) = id_domain_tmp(ip)
        istack_sr(ip) = istack_sr(ip-1) + nnod_sr_tmp(ip)
      end do
      ntot_item_sr = istack_sr(nneib_domain)
!
      end subroutine set_comm_stack_rtp_rj
!
! -----------------------------------------------------------------------
!
      end module set_comm_table_rtp_rj
