!>@file   copy_spectr_4_sph_trans.f90
!!@brief  module copy_spectr_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer
!!        and local array for center
!!
!!@verbatim
!!      subroutine sel_sph_rj_scalar_2_send_wpole                       &
!!     &         (ncomp_send, i_field, i_send, n_WS, WS, v_pl_local)
!!      subroutine sel_sph_rj_scalar_to_send                            &
!!     &         (ncomp_send, i_field, i_send, n_WS, WS)
!!      subroutine sel_sph_rj_vector_to_send                            &
!!     &         (ncomp_send, i_field, i_send, n_WS, WS)
!!      subroutine sel_sph_rj_tensor_to_send                            &
!!     &         (ncomp_send, i_field, i_send, n_WS, WS)
!!
!!      subroutine sel_sph_rj_scalar_from_recv                          &
!!     &         (ncomp_recv, i_field, i_recv, n_WR, WR)
!!      subroutine sel_sph_rj_vector_from_recv                          &
!!     &         (ncomp_recv, i_field, i_recv, n_WR, WR)
!!      subroutine sel_sph_rj_tensor_from_recv                          &
!!     &         (ncomp_recv, i_field, i_recv, n_WR, WR)
!!@endverbatim
!
      module copy_spectr_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use m_sph_spectr_data
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine sel_sph_rj_scalar_2_send_wpole                         &
     &         (ncomp_send, i_field, i_send, n_WS, WS, v_pl_local)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
      use m_work_pole_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_scalar(ncomp_send, nnod_rj, n_WS,        &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    ntot_phys_rj, i_field, i_send, d_rj, WS)
!
      if(iflag_rj_center .le. 0) return
!
      if(inod_rj_center .gt. 0) then
        v_pl_local(nnod_pole,i_send) = d_rj(inod_rj_center,i_field)
      else
        v_pl_local(nnod_pole,i_send) = 0.0d0
      end if
!
      end subroutine sel_sph_rj_scalar_2_send_wpole
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_scalar_to_send                              &
     &         (ncomp_send, i_field, i_send, n_WS, WS)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_scalar(ncomp_send, nnod_rj, n_WS,        &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    ntot_phys_rj, i_field, i_send, d_rj, WS)
!
      end subroutine sel_sph_rj_scalar_to_send
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_vector_to_send                              &
     &         (ncomp_send, i_field, i_send, n_WS, WS)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_vector(ncomp_send, nnod_rj, n_WS,        &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    ntot_phys_rj, i_field, i_send, d_rj, WS)
!
      end subroutine sel_sph_rj_vector_to_send
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_tensor_to_send                              &
     &         (ncomp_send, i_field, i_send, n_WS, WS)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_tensor(ncomp_send, nnod_rj, n_WS,        &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    ntot_phys_rj, i_field, i_send, d_rj, WS)
!
      end subroutine sel_sph_rj_tensor_to_send
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_scalar_from_recv                            &
     &         (ncomp_recv, i_field, i_recv, n_WR, WR)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_recv
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      if(i_field*i_recv .eq. 0) return
      call sel_sph_scalar_from_recv(ncomp_recv, nnod_rj, n_WR,          &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    irev_sr_rj,  ntot_phys_rj, i_field, i_recv, WR, d_rj)
!
      end subroutine sel_sph_rj_scalar_from_recv
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_vector_from_recv                            &
     &         (ncomp_recv, i_field, i_recv, n_WR, WR)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_recv
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      if(i_field*i_recv .eq. 0) return
      call sel_sph_vector_from_recv(ncomp_recv, nnod_rj, n_WR,          &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    irev_sr_rj,  ntot_phys_rj, i_field, i_recv, WR, d_rj)
!
      end subroutine sel_sph_rj_vector_from_recv
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_tensor_from_recv                            &
     &         (ncomp_recv, i_field, i_recv, n_WR, WR)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_recv
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      if(i_field*i_recv .eq. 0) return
      call sel_sph_tensor_from_recv(ncomp_recv, nnod_rj, n_WR,          &
     &    nmax_sr_rj,  nneib_domain_rj,  istack_sr_rj,  item_sr_rj,     &
     &    irev_sr_rj,  ntot_phys_rj, i_field, i_recv, WR, d_rj)
!
      end subroutine sel_sph_rj_tensor_from_recv
!
!-----------------------------------------------------------------------
!
      end module copy_spectr_4_sph_trans
