!>@file   set_address_sph_trans_MHD.f90
!!@brief  module set_address_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_MHD                          &
!!     &         (MHD_prop, SPH_MHD, iphys, trns_MHD,                   &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!
!!      subroutine mhd_spectr_to_sendbuf                                &
!!     &         (backward, comm_rj, rj_fld, n_WS, WS)
!!        type(address_each_sph_trans), intent(in) :: backward
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(in) :: rj_fld
!!      subroutine mhd_spectr_to_sendbuf_wpole(nnod_pole,               &
!!     &          sph_rj, comm_rj, rj_fld, n_WS, WS, backward)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_each_sph_trans), intent(inout) :: backward
!!      subroutine mhd_spectr_from_recvbuf                              &
!!     &         (forward, comm_rj, n_WR, WR, rj_fld)
!!        type(address_each_sph_trans), intent(in) :: forward
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module set_address_sph_trans_MHD
!
      use m_precision
!
      use t_sph_trans_comm_tbl
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_SPH_mesh_field_data
      use t_addresses_sph_transform
      use t_control_parameter
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_MHD                            &
     &         (MHD_prop, SPH_MHD, iphys, trns_MHD,                     &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_MHD
      use address_fwd_sph_trans_MHD
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for MHD'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      call b_trans_address_vector_MHD                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, MHD_prop%ht_prop,         &
     &    MHD_prop%cp_prop,  SPH_MHD%ipol, SPH_MHD%itor, iphys,         &
     &    trns_MHD%b_trns, trns_MHD%backward)
      call b_trans_address_scalar_MHD(MHD_prop%ht_prop,                 &
     &    MHD_prop%cp_prop,  SPH_MHD%ipol, SPH_MHD%itor, iphys,         &
     &    trns_MHD%b_trns, trns_MHD%backward)
      trns_MHD%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      call f_trans_address_vector_MHD                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, MHD_prop%ht_prop,         &
     &    MHD_prop%cp_prop, SPH_MHD%ipol, SPH_MHD%itor, iphys,          &
     &    trns_MHD%f_trns, trns_MHD%forward)
      call f_trans_address_scalar_MHD                                   &
     &   (SPH_MHD%ipol, SPH_MHD%itor, iphys,                            &
     &    trns_MHD%f_trns, trns_MHD%forward)
      trns_MHD%forward%num_tensor = 0
!
      ncomp_sph_trans =   0
      nvector_sph_trans = 0
      nscalar_sph_trans = 0
      call count_num_fields_each_trans(trns_MHD%backward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_MHD%forward,                &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_MHD%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_MHD%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_MHD%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_MHD%forward%num_scalar
      end if
!
      end subroutine set_addresses_trans_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mhd_spectr_to_sendbuf                                  &
     &         (backward, comm_rj, rj_fld, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      type(address_each_sph_trans), intent(in) :: backward
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, backward%num_vector
        call sel_sph_rj_vector_to_send                                  &
     &     (backward%ncomp, backward%ifld_rj(i), backward%ifld_trns(i), &
     &      comm_rj, rj_fld, n_WS, WS)
      end do
      do inum = 1, backward%num_scalar
        i = inum + backward%num_vector
        call sel_sph_rj_scalar_to_send                                  &
     &     (backward%ncomp, backward%ifld_rj(i), backward%ifld_trns(i), &
     &      comm_rj, rj_fld, n_WS, WS)
      end do
!
      end subroutine mhd_spectr_to_sendbuf
!
!-----------------------------------------------------------------------
!
      subroutine mhd_spectr_to_sendbuf_wpole(nnod_pole,                 &
     &          sph_rj, comm_rj, rj_fld, n_WS, WS, backward)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: nnod_pole
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      type(address_each_sph_trans), intent(inout) :: backward
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, backward%num_vector
        call sel_sph_rj_vector_to_send                                  &
     &     (backward%ncomp, backward%ifld_rj(i), backward%ifld_trns(i), &
     &      comm_rj, rj_fld, n_WS, WS)
      end do
      do inum = 1, backward%num_scalar
        i = inum + backward%num_vector
        call sel_sph_rj_scalar_2_send_wpole(backward%ncomp,             &
     &      backward%ifld_rj(i), backward%ifld_trns(i), nnod_pole,      &
     &      sph_rj, comm_rj, rj_fld, n_WS, WS, backward%flc_pole)
      end do
!
      end subroutine mhd_spectr_to_sendbuf_wpole
!
!-----------------------------------------------------------------------
!
      subroutine mhd_spectr_from_recvbuf                                &
     &         (forward, comm_rj, n_WR, WR, rj_fld)
!
      use copy_spectr_4_sph_trans
!
      type(address_each_sph_trans), intent(in) :: forward
      type(sph_comm_tbl), intent(in) :: comm_rj
      integer(kind = kint), intent(in) :: n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, forward%num_vector
        call sel_sph_rj_vector_from_recv                                &
     &     (forward%ncomp, forward%ifld_rj(i), forward%ifld_trns(i),    &
     &      comm_rj, n_WR, WR, rj_fld)
      end do
      do inum = 1, forward%num_scalar
        i = inum + forward%num_vector
        call sel_sph_rj_scalar_from_recv                                &
     &     (forward%ncomp, forward%ifld_rj(i), forward%ifld_trns(i),    &
     &      comm_rj, n_WR, WR, rj_fld)
      end do
!
      end  subroutine mhd_spectr_from_recvbuf
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_MHD
