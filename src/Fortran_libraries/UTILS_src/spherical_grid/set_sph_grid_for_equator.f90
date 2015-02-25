!
!      module set_sph_grid_for_equator
!
      module set_sph_grid_for_equator
!
!     Written by H. Matsui on Nov., 2007
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      implicit none
!
      private :: set_grids_for_equator
      private :: set_sph_comm_for_equator, set_sph_group_for_equator
!
!      subroutine s_set_sph_grid_for_equator
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_sph_grid_for_equator
!
      call set_grids_for_equator
      call set_sph_comm_for_equator
      call set_sph_group_for_equator
!
      end subroutine s_set_sph_grid_for_equator
!
! ----------------------------------------------------------------------
!
      subroutine set_grids_for_equator
!
      use m_2d_sph_trans_table
      use set_indices_4_sph_tranform
      use set_sph_tranform_ordering
!
      integer(kind= kint) :: i, kp, kr, inod, np, np2
!
!
      sph_rank_rtp(1:ithree) = izero
      ndomain_rtp(1:ithree) =  ione
!
      nidx_rtp(1:ithree) = nidx_global_rtp(1:ithree)
      ist_rtp(1:ithree) =  ione
      ied_rtp(1:ithree) =  nidx_global_rtp(1:ithree)
      nnod_rtp = nidx_rtp(1)*nidx_rtp(2)*nidx_rtp(3)
!
!
      call allocate_spheric_param_rtp
      call allocate_sph_1d_index_rtp
!
      inod = 0
      do kp = 1, nidx_global_rtp(3)
        do kr = 1, nidx_global_rtp(1)
          inod = inod + 1
          idx_global_rtp(inod,1) = kr
          idx_global_rtp(inod,2) = ione
          idx_global_rtp(inod,3) = kp
        end do
      end do
!
      do i = 1, nidx_rtp(1)
        radius_1d_rtp_r(i) = radius_1d_gl(i)
        idx_gl_1d_rtp_r(i) = i
      end do
!
      idx_gl_1d_rtp_t(1) = 1
!
!
      np =  nidx_global_rtp(3)
      np2 = nidx_global_rtp(3) / 2
!
      allocate( mspec_4_ispack(-np2:np2) )
      allocate( mdx_ispack(np) )
!
      call set_wavenumber_4_ispack_fft(np2, np, mspec_4_ispack,         &
     &    mdx_ispack)
!
      do i = 1, nidx_rtp(3)
        idx_gl_1d_rtp_p(i,1) = i
        idx_gl_1d_rtp_p(i,2) = mdx_ispack(i)
      end do
!
      deallocate( mspec_4_ispack )
      deallocate( mdx_ispack )
!
      end subroutine set_grids_for_equator
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_comm_for_equator
!
      use m_sph_trans_comm_table
!
      nneib_domain_rtp = 0
      ntot_item_sr_rtp = 0
      call allocate_sph_comm_stack_rtp
      call allocate_sph_comm_item_rtp(nnod_rtp)
!
      end subroutine set_sph_comm_for_equator
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_group_for_equator
!
      use m_group_data_sph_specr
!
      integer(kind= kint) :: i, icou
!
!
      num_bc_grp_rtp =  0
      ntot_bc_grp_rtp = 0
      call allocate_rtp_nod_grp_stack
      call allocate_rtp_nod_grp_item
!
      num_theta_grp_rtp =  0
      ntot_theta_grp_rtp = 0
      call allocate_rtp_theta_grp_stack
      call allocate_rtp_theta_grp_item
!
      num_zonal_grp_rtp =  0
      ntot_zonal_grp_rtp = 0
      call allocate_rtp_zonal_grp_stack
      call allocate_rtp_zonal_grp_item
!
!     set radial group
!
      num_radial_grp_rtp = 6
      call allocate_rtp_r_grp_stack
!
      istack_radial_grp_rtp(1) = 1
      istack_radial_grp_rtp(2) = 2
      istack_radial_grp_rtp(3) = istack_radial_grp_rtp(2)
      if (nlayer_2_center .gt. 0) then
        istack_radial_grp_rtp(3) = istack_radial_grp_rtp(3) + 1
      end if
      istack_radial_grp_rtp(4) = istack_radial_grp_rtp(3)
      if (nlayer_mid_OC .gt. 0) then
        istack_radial_grp_rtp(4) = istack_radial_grp_rtp(4) + 1
      end if
      istack_radial_grp_rtp(5) = istack_radial_grp_rtp(4)               &
     &                          + nlayer_ICB - 1
      istack_radial_grp_rtp(6) = istack_radial_grp_rtp(5)               &
     &                          + nlayer_CMB - nlayer_ICB + 1
!
      name_radial_grp_rtp(1) = ICB_nod_grp_name
      name_radial_grp_rtp(2) = CMB_nod_grp_name
      name_radial_grp_rtp(3) = CTR_nod_grp_name
      name_radial_grp_rtp(4) = 'Mid_OC'
      name_radial_grp_rtp(5) = IC_ele_grp_name
      name_radial_grp_rtp(6) = OC_ele_grp_name
!
      ntot_radial_grp_rtp = istack_radial_grp_rtp(6)
      call allocate_rtp_r_grp_item
!
      item_radial_grp_rtp(1) = nlayer_ICB
      item_radial_grp_rtp(2) = nlayer_CMB
!
      if (nlayer_2_center .gt. 0) then
        icou = istack_radial_grp_rtp(2) + 1
        item_radial_grp_rtp(icou) = nlayer_2_center
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        icou = istack_radial_grp_rtp(3) + 1
        item_radial_grp_rtp(icou) = nlayer_mid_OC
      end if
!
      icou = istack_radial_grp_rtp(4)
      do i = 1, nlayer_ICB-1
        icou = icou + 1
        item_radial_grp_rtp(icou) = i
      end do
!
      icou = istack_radial_grp_rtp(5)
      do i = nlayer_ICB, nlayer_CMB
        icou = icou + 1
        item_radial_grp_rtp(icou) = i
      end do
!
      end subroutine set_sph_group_for_equator
!
! ----------------------------------------------------------------------
!
      end module set_sph_grid_for_equator
