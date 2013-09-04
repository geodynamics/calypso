!copy_rj_spec_name_to_node.f90
!     module copy_rj_spec_name_to_node
!
!      Written by H. Matsui on Feb., 2008
!
!      subroutine copy_rj_spec_name_to_nod_fld
!
      module copy_rj_spec_name_to_node
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_spec_name_to_nod_fld
!
      use m_node_phys_data
      use m_sph_spectr_data
!
      num_nod_phys =     num_phys_rj
      num_tot_nod_phys = ntot_phys_rj
!
      num_nod_phys_vis =     num_phys_rj_vis
      num_tot_nod_phys_vis = ntot_comp_rj_vis
!
      call allocate_phys_name
!
      num_nod_component(1:num_nod_phys)                                 &
     &                          = num_phys_comp_rj(1:num_nod_phys)
      phys_nod_name(1:num_nod_phys) = phys_name_rj(1:num_nod_phys)
      iflag_nod_fld_monitor(1:num_nod_phys)                             &
     &                          = iflag_monitor_rj(1:num_nod_phys)
      istack_nod_component(0:num_nod_phys)                              &
     &                          = istack_phys_comp_rj(0:num_nod_phys)
!
      end subroutine copy_rj_spec_name_to_nod_fld
!
! -----------------------------------------------------------------------
!
      end module copy_rj_spec_name_to_node
