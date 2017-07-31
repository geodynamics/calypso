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
!!     &         (ncomp_send, i_field, i_send, nnod_pole,               &
!!     &          sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!      subroutine sel_sph_rj_scalar_to_send                            &
!!     &         (ncomp_send, i_field, i_send, comm_rj, rj_fld,         &
!!     &          n_WS, WS)
!!      subroutine sel_sph_rj_vector_to_send                            &
!!     &         (ncomp_send, i_field, i_send, comm_rj, rj_fld,         &
!!     &          n_WS, WS)
!!      subroutine sel_sph_rj_tensor_to_send                            &
!!     &         (ncomp_send, i_field, i_send, comm_rj, rj_fld,         &
!!     &          n_WS, WS)
!!        type(phys_data), intent(in) :: rj_fld
!!
!!      subroutine sel_sph_rj_scalar_from_recv                          &
!!     &         (ncomp_recv, i_field, i_recv, comm_rj,                 &
!!     &          n_WR, WR, rj_fld)
!!      subroutine sel_sph_rj_vector_from_recv                          &
!!     &         (ncomp_recv, i_field, i_recv, comm_rj,                 &
!!     &          n_WR, WR, rj_fld)
!!      subroutine sel_sph_rj_tensor_from_recv                          &
!!     &         (ncomp_recv, i_field, i_recv, comm_rj,                 &
!!     &          n_WR, WR, rj_fld)
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module copy_spectr_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
!
      use t_sph_trans_comm_tbl
      use t_phys_data
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
     &         (ncomp_send, i_field, i_send, nnod_pole,                 &
     &          sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      use t_spheric_rj_data
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
!
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_scalar(ncomp_send, rj_fld%n_point, n_WS, &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    rj_fld%ntot_phys, i_field, i_send, rj_fld%d_fld, WS)
!
      if(sph_rj%iflag_rj_center .le. 0) return
!
      if(sph_rj%inod_rj_center .gt. 0) then
        v_pl_local(nnod_pole,i_send)                                    &
     &      = rj_fld%d_fld(sph_rj%inod_rj_center,i_field)
      else
        v_pl_local(nnod_pole,i_send) = 0.0d0
      end if
!
      end subroutine sel_sph_rj_scalar_2_send_wpole
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_scalar_to_send                              &
     &         (ncomp_send, i_field, i_send, comm_rj, rj_fld,           &
     &          n_WS, WS)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_scalar(ncomp_send, rj_fld%n_point, n_WS, &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    rj_fld%ntot_phys, i_field, i_send, rj_fld%d_fld, WS)
!
      end subroutine sel_sph_rj_scalar_to_send
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_vector_to_send                              &
     &         (ncomp_send, i_field, i_send, comm_rj, rj_fld,           &
     &          n_WS, WS)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_vector(ncomp_send, rj_fld%n_point, n_WS, &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    rj_fld%ntot_phys, i_field, i_send, rj_fld%d_fld, WS)
!
      end subroutine sel_sph_rj_vector_to_send
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_tensor_to_send                              &
     &         (ncomp_send, i_field, i_send, comm_rj, rj_fld,           &
     &          n_WS, WS)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_tensor(ncomp_send, rj_fld%n_point, n_WS, &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    rj_fld%ntot_phys, i_field, i_send, rj_fld%d_fld, WS)
!
      end subroutine sel_sph_rj_tensor_to_send
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_scalar_from_recv                            &
     &         (ncomp_recv, i_field, i_recv, comm_rj,                   &
     &          n_WR, WR, rj_fld)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      integer(kind = kint), intent(in) :: i_field, i_recv
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_field*i_recv .eq. 0) return
      call sel_sph_scalar_from_recv(ncomp_recv, rj_fld%n_point, n_WR,   &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    comm_rj%irev_sr,  rj_fld%ntot_phys, i_field, i_recv,          &
     &    WR, rj_fld%d_fld)
!
      end subroutine sel_sph_rj_scalar_from_recv
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_vector_from_recv                            &
     &         (ncomp_recv, i_field, i_recv, comm_rj,                   &
     &          n_WR, WR, rj_fld)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      integer(kind = kint), intent(in) :: i_field, i_recv
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_field*i_recv .eq. 0) return
      call sel_sph_vector_from_recv(ncomp_recv, rj_fld%n_point, n_WR,   &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    comm_rj%irev_sr,  rj_fld%ntot_phys, i_field, i_recv,          &
     &    WR, rj_fld%d_fld)
!
      end subroutine sel_sph_rj_vector_from_recv
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rj_tensor_from_recv                            &
     &         (ncomp_recv, i_field, i_recv, comm_rj,                   &
     &          n_WR, WR, rj_fld)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      integer(kind = kint), intent(in) :: i_field, i_recv
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_field*i_recv .eq. 0) return
      call sel_sph_tensor_from_recv(ncomp_recv, rj_fld%n_point, n_WR,   &
     &    comm_rj%nneib_domain, comm_rj%istack_sr, comm_rj%item_sr,     &
     &    comm_rj%irev_sr,  rj_fld%ntot_phys, i_field, i_recv,          &
     &    WR, rj_fld%d_fld)
!
      end subroutine sel_sph_rj_tensor_from_recv
!
!-----------------------------------------------------------------------
!
      end module copy_spectr_4_sph_trans
