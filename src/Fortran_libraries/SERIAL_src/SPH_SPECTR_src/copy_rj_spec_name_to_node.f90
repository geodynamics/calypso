!> @file  copy_rj_spec_name_to_node.f90
!!      module copy_rj_spec_name_to_node
!!
!! @author  H. Matsui
!! @date Programmed in Feb., 2008
!
!> @brief Copy spectr fields name to nodal field name
!!
!!@verbatim
!!      subroutine copy_rj_spec_name_to_nod_fld
!!@endverbatim
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
      subroutine copy_rj_spec_name_to_nod_fld(nod_fld)
!
      use t_phys_data
      use m_sph_spectr_data
!
      type(phys_data),intent(inout) :: nod_fld
!
!
      nod_fld%num_phys =  num_phys_rj
      nod_fld%ntot_phys = ntot_phys_rj
!
      nod_fld%num_phys_viz =  num_phys_rj_vis
      nod_fld%ntot_phys_viz = ntot_comp_rj_vis
!
      call alloc_phys_name_type(nod_fld)
!
      nod_fld%num_component(1:nod_fld%num_phys)                         &
     &                     = num_phys_comp_rj(1:nod_fld%num_phys)
      nod_fld%phys_name(1:nod_fld%num_phys)                             &
     &                     = phys_name_rj(1:nod_fld%num_phys)
      nod_fld%iflag_monitor(1:nod_fld%num_phys)                         &
     &                     = iflag_monitor_rj(1:nod_fld%num_phys)
      nod_fld%istack_component(0:nod_fld%num_phys)                      &
     &                     = istack_phys_comp_rj(0:nod_fld%num_phys)
!
      end subroutine copy_rj_spec_name_to_nod_fld
!
! -----------------------------------------------------------------------
!
      end module copy_rj_spec_name_to_node
