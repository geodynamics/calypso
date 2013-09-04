!
!      module m_sph_domain_group
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine set_sph_domain_group
!      subroutine copy_domain_grp_sph_to_part
!
      module m_sph_domain_group
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint) :: nnod_domain_grp
      integer(kind = kint),  allocatable :: i_domain_sph(:)
      integer(kind = kint),  allocatable :: inod_local_sph(:)
!
      private :: s_set_domain_id_sph
      private :: allocate_sph_domain_grp
      private :: set_domain_id_sph_pole, set_domain_id_sph_center
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_sph_domain_grp
!
!
      allocate( i_domain_sph(nnod_domain_grp) )
      allocate( inod_local_sph(nnod_domain_grp) )
      i_domain_sph = 0
      inod_local_sph = 0
!
      end subroutine allocate_sph_domain_grp
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_domain_grp
!
!
      deallocate(i_domain_sph, inod_local_sph)
!
      end subroutine deallocate_sph_domain_grp
!
!   --------------------------------------------------------------------
!
      subroutine set_sph_domain_group
!
      use m_spheric_constants
      use m_spheric_parameter
      use m_subdomain_table_IO
!
      integer(kind = kint) :: inod
      integer(kind = kint) :: ndomain_grp
!
!
      ndomain_grp = ndomain_rtp(1)*ndomain_rtp(2)
!
      nnod_domain_grp = nidx_global_rtp(1)*nidx_global_rtp(2)           &
     &                 *nidx_global_rtp(3)
      if (iflag_shell_mode .eq. iflag_MESH_w_pole) then
        nnod_domain_grp = nnod_domain_grp + 2*nidx_global_rtp(1)
      end if
      if (iflag_shell_mode .eq. iflag_MESH_w_center) then
        nnod_domain_grp = nnod_domain_grp + 2*nidx_global_rtp(1) + 1
      end if
!
      call allocate_sph_domain_grp
!
      inod = 0
      call s_set_domain_id_sph(inod)
!
      if (iflag_shell_mode .eq. iflag_MESH_w_pole) then
        call set_domain_id_sph_pole(inod)
      end if
      if (iflag_shell_mode .eq. iflag_MESH_w_center) then
        call set_domain_id_sph_pole(inod)
        call set_domain_id_sph_center(inod)
      end if
!
!      call copy_domain_grp_sph_to_IO
!      call output_group_4_partition
!
      end subroutine set_sph_domain_group
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_set_domain_id_sph(inod)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
!
      integer(kind = kint), intent(inout) :: inod
      integer(kind = kint) :: kr, kt, kp, ir_rank, it_rank
!
      do kp = 1, nidx_global_rtp(3)
        do kt = 1, nidx_global_rtp(2)
          do kr = 1, nidx_global_rtp(1)
            inod = inod + 1
            ir_rank = id_domain_rtp_r(kr)
            it_rank = id_domain_rtp_t(kt)
            i_domain_sph(inod) =  it_rank+1                             &
     &                          + ir_rank * ndomain_rtp(2)
          end do
        end do
      end do
!
      end subroutine s_set_domain_id_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_domain_id_sph_pole(inod)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
!
      integer(kind = kint), intent(inout) :: inod
      integer(kind = kint) :: kr, kt, ir_rank, it_rank
!
      kt = 1
      do kr = 1, nidx_global_rtp(1)
        inod = inod + 1
        ir_rank = id_domain_rtp_r(kr)
        it_rank = id_domain_rtp_t(kt)
        i_domain_sph(inod) =  it_rank+1                                 &
     &                          + ir_rank * ndomain_rtp(2)
      end do
      kt = nidx_global_rtp(2)
      do kr = 1, nidx_global_rtp(1)
        inod = inod + 1
        ir_rank = id_domain_rtp_r(kr)
        it_rank = id_domain_rtp_t(kt)
        i_domain_sph(inod) =  it_rank+1                                 &
     &                          + ir_rank * ndomain_rtp(2)
      end do
!
      end subroutine set_domain_id_sph_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_domain_id_sph_center(inod)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
!
      integer(kind = kint), intent(inout) :: inod
      integer(kind = kint) :: kr, kt, ir_rank, it_rank
!
      kr = 1
      kt = 1
      inod = inod + 1
      ir_rank = id_domain_rtp_r(kr)
      it_rank = id_domain_rtp_t(kt)
      i_domain_sph(inod) =  it_rank+1 + ir_rank * ndomain_rtp(2)
!
      end subroutine set_domain_id_sph_center
!
! -----------------------------------------------------------------------
!
      end module m_sph_domain_group
