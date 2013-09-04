!>@file   set_sph_groups.f90
!!@brief  module set_sph_groups
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Set groups for spherical harmonics indexing
!!
!!@verbatim
!!      subroutine s_set_sph_groups
!!@endverbatim
!
      module set_sph_groups
!
      use m_precision
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      implicit none
!
      private :: set_rtp_radial_grp
      private :: set_rj_spectr_grp
      private :: set_no_rtp_node_grp
      private :: set_no_rtp_meridian_grp, set_no_rtp_zonal_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_sph_groups
!
!
!      write(*,*) 'set_rtp_radial_grp'
      call set_rtp_radial_grp
!      write(*,*) 'set_no_rtp_meridian_grp'
      call set_no_rtp_meridian_grp
!      write(*,*) 'set_no_rtp_zonal_grp'
      call set_no_rtp_zonal_grp
!
!      write(*,*) 'set_no_rtp_node_grp'
      call set_no_rtp_node_grp
!
!      write(*,*) 'set_rj_radial_grp'
      call set_rj_radial_grp
!      write(*,*) 'set_rj_spectr_grp'
      call set_rj_spectr_grp
!
      end subroutine s_set_sph_groups
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rtp_radial_grp
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
!
      num_radial_grp_rtp =  3
      if (nlayer_2_center .gt. 0) then
        num_radial_grp_rtp =  num_radial_grp_rtp + 2
      end if
      if (nidx_global_rtp(1) .gt. nlayer_CMB) then
        num_radial_grp_rtp =  num_radial_grp_rtp + 1
      end if
      if (nlayer_mid_OC .gt. 0) then
        num_radial_grp_rtp =  num_radial_grp_rtp + 1
      end if
!
      call allocate_rtp_r_grp_stack
      call set_stack_rtp_radial_grp
!
      call allocate_rtp_r_grp_item
      call set_item_rtp_radial_grp
!
      end subroutine set_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_radial_grp
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
!
      num_radial_grp_rj = num_radial_grp_rtp
      call allocate_rj_r_grp_stack
      call set_stack_rj_radial_grp
!
      call allocate_rj_r_grp_item
      call set_item_rj_radial_grp
!
      end subroutine set_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_spectr_grp
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
!
      num_sphere_grp_rj =  4
!      write(*,*) 'allocate_rj_sphere_grp_stack'
      call allocate_rj_sphere_grp_stack
!      write(*,*) 'set_stack_rj_spectr_grp'
      call set_stack_rj_spectr_grp
!
!      write(*,*) 'allocate_rj_sphere_grp_item'
      call allocate_rj_sphere_grp_item
!      write(*,*) 'set_item_rj_spectr_grp'
      call set_item_rj_spectr_grp
!
      end subroutine set_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_node_grp
!
      num_bc_grp_rtp =  0
      ntot_bc_grp_rtp = 0
      call allocate_rtp_nod_grp_stack
      call allocate_rtp_nod_grp_item
!
      end subroutine set_no_rtp_node_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_meridian_grp
!
      num_theta_grp_rtp =  0
      ntot_theta_grp_rtp = 0
      call allocate_rtp_theta_grp_stack
      call allocate_rtp_theta_grp_item
!
      end subroutine set_no_rtp_meridian_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_zonal_grp
!
      num_zonal_grp_rtp =  0
      ntot_zonal_grp_rtp = 0
      call allocate_rtp_zonal_grp_stack
      call allocate_rtp_zonal_grp_item
!
      end subroutine set_no_rtp_zonal_grp
!
! -----------------------------------------------------------------------
!
      end module set_sph_groups
