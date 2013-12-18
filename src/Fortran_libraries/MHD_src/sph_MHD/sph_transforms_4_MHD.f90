!>@file   sph_transforms_4_MHD.f90
!!@brief  module sph_transforms_4_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD
!!
!!      subroutine sph_back_trans_4_MHD
!!      subroutine sph_forward_trans_4_MHD
!!
!!      subroutine sph_back_trans_snapshot_MHD
!!      subroutine sph_forward_trans_snapshot_MHD
!!@endverbatim
!!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: leg_trans_selector
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD
!
      use m_machine_parameter
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_work_4_sph_trans
      use init_sph_trans
      use const_wz_coriolis_rtp
      use select_legendre_transform
      use sph_trans_vector
!
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD
      call set_addresses_snapshot_trans
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_add_trans_sph_MHD
        call check_addresses_snapshot_trans
      end if
!
      call initialize_sph_trans
!
      call set_colatitude_rtp
!
      if(id_legendre_transfer .eq. iflag_leg_undefined) then
        call leg_trans_selector
      end if
!
      call sel_legendre_trans_init
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_transforms_sph_MHD
!
!
      if(ncomp_rj_2_rtp .gt. 0) call copy_mhd_sph_spec_to_trans
!
      if((nvector_rj_2_rtp+nscalar_rj_2_rtp) .gt. 0) then
        call sph_b_trans_sph_MHD                                        &
     &      (nvector_rj_2_rtp, nscalar_rj_2_rtp, izero)
      end if
!
      if(ncomp_rj_2_rtp .gt. 0) call copy_mhd_sph_fld_from_trans
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_transforms_sph_MHD
!
!
      if(ncomp_rtp_2_rj .gt. 0) then
        call copy_mhd_sph_fld_to_trans
        call sph_f_trans_sph_MHD                                        &
    &      (nvector_rtp_2_rj, nscalar_rtp_2_rj, izero)
        call copy_mhd_sph_spec_from_trans
      end if
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD
!
      use sph_trans_vector
      use copy_snap_4_sph_trans
!
!
!   transform for vectors
      if(nvector_snap_rj_2_rtp .gt. 0) call copy_snap_vec_spec_to_trans
      if(nscalar_snap_rj_2_rtp .gt. 0) call copy_snap_scl_spec_to_trans
!
      if((nvector_snap_rj_2_rtp+nscalar_snap_rj_2_rtp) .gt. 0) then
        call sph_b_trans_vector                                         &
     &     (nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp, izero)
      end if
!
      if(nvector_snap_rj_2_rtp .gt.0) call copy_snap_vec_fld_from_trans
      if(nscalar_snap_rj_2_rtp .gt.0) call copy_snap_scl_fld_from_trans
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD
!
      use sph_trans_vector
      use copy_snap_4_sph_trans
!
!
!   transform for vectors
      if(nvector_snap_rtp_2_rj .gt. 0) call copy_snap_vec_fld_to_trans
      if(nscalar_snap_rtp_2_rj .gt. 0) call copy_snap_scl_fld_to_trans
!
      call sph_f_trans_vector                                           &
     &   (nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj, izero)
!
      if(nvector_snap_rtp_2_rj .gt. 0) call copy_snap_vec_spec_from_trans
      if(nscalar_snap_rtp_2_rj .gt. 0) call copy_snap_scl_spec_from_trans
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine leg_trans_selector
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_4_sph_trans
      use select_legendre_transform
      use sph_trans_vector
!
      real(kind = kreal) :: stime, etime(1:5), etime_shortest
      real(kind = kreal) :: etime_trans(1:5)
!
!
      id_legendre_transfer = iflag_leg_orginal_loop
      call sel_legendre_trans_init
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_legendre_transfer) = MPI_WTIME() - stime
      call sel_legendre_trans_finalize
!
      id_legendre_transfer = iflag_leg_krloop_inner
      call sel_legendre_trans_init
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_legendre_transfer) = MPI_WTIME() - stime
      call sel_legendre_trans_finalize
!
      id_legendre_transfer = iflag_leg_krloop_outer
      call sel_legendre_trans_init
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_legendre_transfer) = MPI_WTIME() - stime
      call sel_legendre_trans_finalize
!
      id_legendre_transfer = iflag_leg_krloop_long
      call sel_legendre_trans_init
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_legendre_transfer) = MPI_WTIME() - stime
      call sel_legendre_trans_finalize
!
      id_legendre_transfer = iflag_leg_fldloop_outer
      call sel_legendre_trans_init
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_legendre_transfer) = MPI_WTIME() - stime
      call sel_legendre_trans_finalize
!
!
      call MPI_allREDUCE (etime, etime_trans, ifive,                    &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      id_legendre_transfer = iflag_leg_orginal_loop
      etime_shortest =       etime_trans(iflag_leg_orginal_loop)
!
      if(etime_trans(iflag_leg_krloop_inner) .lt. etime_shortest) then
        id_legendre_transfer = iflag_leg_krloop_inner
        etime_shortest =       etime_trans(iflag_leg_krloop_inner)
      end if
      if(etime_trans(iflag_leg_krloop_outer) .lt. etime_shortest) then
        id_legendre_transfer = iflag_leg_krloop_outer
        etime_shortest =       etime_trans(iflag_leg_krloop_outer)
      end if
      if(etime_trans(iflag_leg_krloop_long) .lt. etime_shortest) then
        id_legendre_transfer = iflag_leg_krloop_long
        etime_shortest =       etime_trans(iflag_leg_krloop_long)
      end if
      if(etime_trans(iflag_leg_fldloop_outer) .lt. etime_shortest) then
        id_legendre_transfer = iflag_leg_fldloop_outer
        etime_shortest =       etime_trans(iflag_leg_fldloop_outer)
      end if
!
      if(my_rank .gt. 0) return
        write(*,'(a,i4)', advance='no')                                 &
     &         'Selected id_legendre_transfer: ', id_legendre_transfer
!
        if     (id_legendre_transfer .eq. iflag_leg_orginal_loop) then
          write(*,'(a,a)') ' (ORIGINAL_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
          write(*,'(a,a)') ' (INNER_RADIAL_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
          write(*,'(a,a)') ' (OUTER_RADIAL_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
          write(*,'(a,a)') ' (LONG_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
          write(*,'(a,a)') ' (OUTER_FIELD_LOOP) '
        end if
!
        write(*,*) '1: elapsed by original loop: ',                     &
     &            etime_trans(iflag_leg_orginal_loop)
        write(*,*) '2: elapsed by inner radius loop: ',                 &
     &            etime_trans(iflag_leg_krloop_inner)
        write(*,*) '3: elapsed by outer radius loop: ',                 &
     &            etime_trans(iflag_leg_krloop_outer)
        write(*,*) '4: elapsed by longest loop: ',                      &
     &            etime_trans(iflag_leg_krloop_long)
        write(*,*) '5: elapsed by outer field loop: ',                  &
     &            etime_trans(iflag_leg_fldloop_outer)
!
      end subroutine leg_trans_selector
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
