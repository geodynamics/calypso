!>@file   pickup_sph_rms_spectr.f90
!!@brief      module pickup_sph_rms_spectr
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief choose mean square data to output
!!
!!@verbatim
!!      subroutine init_sph_rms_4_monitor
!!
!!      subroutine pickup_sph_rms_4_monitor
!!      subroutine pickup_sph_rms_vol_monitor
!!@endverbatim
!
      module pickup_sph_rms_spectr
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_pickup_sph_rms_data
      use m_rms_4_sph_spectr
      use pickup_sph_spectr
!
      implicit  none
!
      private :: set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_rms_4_monitor
!
      use m_pickup_sph_spectr_data
!
      integer(kind = kint) :: k
!
!
      pickup_sph_rms_head = pickup_sph_head
      if(num_pick_layer .le. 0) then
        num_pick_rms_layer = nidx_rj(1)
      else
        num_pick_rms_layer = num_pick_layer
      end if
!
      call count_picked_sph_adrress(l_truncation,                       &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_l, idx_pick_sph_m, ntot_pick_sph_rms_mode)
!
      call allocate_pick_sph_rms
      call allocate_iflag_pick_sph(l_truncation)
!
      call set_picked_sph_adrress                                       &
     &   (l_truncation, nidx_rj(2), idx_gl_1d_rj_j(1,1),                &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    ntot_pick_sph_rms_mode, num_pick_sph_rms_mode,                &
     &    idx_pick_sph_rms_gl, idx_pick_sph_rms_lc)
!
      if(num_pick_layer .le. 0) then
        do k = 1, num_pick_rms_layer
          id_pick_rms_layer(k) = k
          r_pick_rms_layer(k) =  radius_1d_rj_r(k)
        end do
      else
        do k = 1, num_pick_rms_layer
          id_pick_rms_layer(k) = id_pick_layer(k)
          r_pick_rms_layer(k) =  r_pick_layer(k)
        end do
      end if
!
      call deallocate_iflag_pick_sph
      call deallocate_pick_sph_mode
!
      call set_sph_rms_labels_4_monitor
!
      end subroutine init_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_4_monitor
!
      use calypso_mpi
!
      integer(kind = kint) :: inum, knum, j, k, nd
      integer(kind = kint) :: inod, ipick, num
!
!
!$omp parallel do
      do inum = 1, num_pick_sph_rms_mode*num_pick_rms_layer
        d_rms_pick_sph_lc(1:ntot_rms_rj,inum) = zero
      end do
!$omp end parallel do
!
!$omp parallel private(j)
      do inum = 1, num_pick_sph_rms_mode
        j = idx_pick_sph_rms_lc(inum)
        if(j .gt. izero) then
!$omp do private(knum,k,inod,ipick,nd)
          do knum = 1, num_pick_rms_layer
            k = id_pick_rms_layer(knum)
            inod =  j +    (k-1) * nidx_rj(2)
            ipick = knum + (inum-1) * num_pick_rms_layer
            do nd = 1, ntot_rms_rj
              d_rms_pick_sph_lc(nd,ipick) = rms_sph_dat(nd,inod)
            end do
          end do
!$omp end do nowait
        end if
      end do
!$omp end parallel
!
      num = ntot_rms_rj*num_pick_rms_layer*num_pick_sph_rms_mode
      call MPI_allREDUCE(d_rms_pick_sph_lc(1,1),                        &
     &    d_rms_pick_sph_gl(1,1), num, CALYPSO_REAL, MPI_SUM,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_vol_monitor
!
      use calypso_mpi
!
      integer(kind = kint) :: inum, j, nd, num
!
!
!$omp parallel do
      do inum = 1, num_pick_sph_rms_mode
        d_rms_pick_sph_lc(1:ntot_rms_rj,inum) = zero
      end do
!$omp end parallel do
!
!$omp parallel private(j)
      do inum = 1, num_pick_sph_rms_mode
        j = idx_pick_sph_rms_lc(inum)
        if(j .gt. izero) then
!$omp do private(nd)
            do nd = 1, ntot_rms_rj
              d_rms_pick_sph_lc(nd,inum) = rms_sph_vol_dat(nd,j)
            end do
!$omp end do nowait
        end if
      end do
!$omp end parallel
!
      num = ntot_rms_rj*num_pick_sph_rms_mode
      call MPI_allREDUCE(d_rms_pick_sph_lc(1,1),                        &
     &    d_rms_pick_sph_gl(1,1), num, CALYPSO_REAL, MPI_SUM,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_rms_vol_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels_4_monitor
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint) :: i_fld, ist
!
!
      do i_fld = 1, num_rms_rj
        ist = istack_rms_comp_rj(i_fld-1)
          if ( rms_name_rj(i_fld) .eq. fhd_velo) then
            write(rms_pick_sph_name(ist+1),'(a)') 'K_ene_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'K_ene_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'K_ene'
!
          else if (rms_name_rj(i_fld) .eq. fhd_magne) then
            write(rms_pick_sph_name(ist+1),'(a)') 'M_ene_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'M_ene_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'M_ene'
!
          else if (rms_name_rj(i_fld) .eq. fhd_filter_v) then
            write(rms_pick_sph_name(ist+1),'(a)') 'filter_KE_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'filter_KE_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'filter_KE'
!
          else if (rms_name_rj(i_fld) .eq. fhd_filter_b) then
            write(rms_pick_sph_name(ist+1),'(a)') 'filter_ME_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'filter_ME_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'filter_ME'
!
          else if (num_rms_comp_rj(i_fld) .eq. 1) then
            write(rms_pick_sph_name(ist+1),'(a)')                       &
     &                      trim(rms_name_rj(i_fld))
!
          else if (num_rms_comp_rj(i_fld) .eq. 3) then
            call add_vector_power_sph_label(rms_name_rj(i_fld),         &
     &          rms_pick_sph_name(ist+1), rms_pick_sph_name(ist+2),     &
     &          rms_pick_sph_name(ist+3))
            write(rms_pick_sph_name(ist+3),'(a)') rms_name_rj(i_fld)
          else if (num_rms_comp_rj(i_fld) .eq. 6) then
            call add_tensor_direction_label_rtp(rms_name_rj(i_fld),     &
     &          rms_pick_sph_name(ist+1), rms_pick_sph_name(ist+2),     &
     &          rms_pick_sph_name(ist+3), rms_pick_sph_name(ist+4),     &
     &          rms_pick_sph_name(ist+5), rms_pick_sph_name(ist+6))
          end if
      end do
      ncomp_pick_sph_rms = ntot_rms_rj
!
      end subroutine set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_rms_spectr
