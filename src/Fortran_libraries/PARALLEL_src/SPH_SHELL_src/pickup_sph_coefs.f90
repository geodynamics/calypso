!>@file   pickup_sph_coefs.f90
!!@brief      module pickup_sph_coefs
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief choose spectr data to output
!!
!!@verbatim
!!      subroutine init_sph_spec_4_monitor
!!      subroutine pickup_sph_spec_4_monitor
!!@endverbatim
!
      module pickup_sph_coefs
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_pickup_sph_spectr_data
      use m_sph_spectr_data
      use pickup_sph_spectr
!
      implicit  none
!
      private :: count_sph_labels_4_monitor, set_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_spec_4_monitor
!
      integer(kind = kint) :: k, knum
!
!
      if( (num_pick_sph+num_pick_sph_l+num_pick_sph_m) .eq. 0) return
!
      if(num_pick_layer .le. 0) then
        num_pick_layer = nidx_rj(1)
        call allocate_num_pick_layer
!
        do k = 1, num_pick_layer
          id_pick_layer(k) = k
        end do
      end if
!
      do knum = 1, num_pick_layer
        k = id_pick_layer(knum)
        r_pick_layer(knum) = radius_1d_rj_r(k)
      end do
!
      call count_sph_labels_4_monitor
      call count_picked_sph_adrress(l_truncation,                       &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_l, idx_pick_sph_m, ntot_pick_sph_mode)
!
      call allocate_pick_sph_monitor
      call allocate_iflag_pick_sph(l_truncation)
!
      call set_picked_sph_adrress(l_truncation, ist_rj(2), ied_rj(2),   &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    ntot_pick_sph_mode, num_pick_sph_mode, idx_pick_sph_gl,       &
     &    idx_pick_sph_lc)
      call deallocate_iflag_pick_sph
      call deallocate_pick_sph_mode
!
      call set_sph_labels_4_monitor
!
      end subroutine init_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_spec_4_monitor
!
      use calypso_mpi
      use m_parallel_var_dof
!
      integer(kind = kint) :: inum, knum, j, k, nd,icou, i_fld
      integer(kind = kint) :: inod, ipick, num, ist_comp, ncomp
!
!
      if(num_pick_sph_mode*num_pick_layer .eq. 0) return
!
!$omp parallel do
      do inum = 1, num_pick_sph_mode*num_pick_layer
        d_rj_pick_sph_lc(1:ncomp_pick_sph_coef,inum) = zero
      end do
!$omp end parallel do
!
!$omp parallel private(j)
      do inum = 1, num_pick_sph_mode
        j = idx_pick_sph_lc(inum)
        if(j .gt. izero) then
!$omp do private(knum,k,inod,ipick,i_fld,icou,nd,ist_comp,ncomp)
          do knum = 1, num_pick_layer
            k = id_pick_layer(knum)
            inod =  j +    (k-1) * nidx_rj(2)
            ipick = knum + (inum-1) * num_pick_layer
            icou = 0
            do i_fld = 1, num_phys_rj
              if(iflag_monitor_rj(i_fld) .gt. 0) then
                ist_comp = istack_phys_comp_rj(i_fld-1)
                ncomp = num_phys_comp_rj(i_fld)
                if(num_phys_comp_rj(i_fld) .eq. 3) then
                  d_rj_pick_sph_lc(icou+1,ipick)= d_rj(inod,ist_comp+1)
                  d_rj_pick_sph_lc(icou+2,ipick)= d_rj(inod,ist_comp+3)
                  d_rj_pick_sph_lc(icou+3,ipick)= d_rj(inod,ist_comp+2)
                else
                  do nd = 1, num_phys_comp_rj(i_fld)
                    d_rj_pick_sph_lc(icou+nd,ipick)                     &
     &                          = d_rj(inod,ist_comp+nd)
                  end do
                end if
                icou = icou + num_phys_comp_rj(i_fld)
              end if
            end do
          end do
!$omp end do nowait
        end if
      end do
!$omp end parallel
!
      num = ncomp_pick_sph_coef*num_pick_layer*num_pick_sph_mode
      call MPI_allREDUCE(d_rj_pick_sph_lc(1,1), d_rj_pick_sph_gl(1,1),  &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr)
!
      end subroutine pickup_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_sph_labels_4_monitor
!
      use m_phys_labels
!
      integer(kind = kint) :: i_fld
!
!
      ncomp_pick_sph_coef = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
          ncomp_pick_sph_coef = ncomp_pick_sph_coef                     &
     &                         + num_phys_comp_rj(i_fld)
        end if
      end do
!
      end subroutine count_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_labels_4_monitor
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint) :: i_fld, ist
!
!
      ist = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
          if(num_phys_comp_rj(i_fld) .eq. 1) then
            write(pick_sph_spec_name(ist+1),'(a)')                      &
     &                      trim(phys_name_rj(i_fld))
          else if(num_phys_comp_rj(i_fld) .eq. 3) then
            call add_vector_sph_spectr_label(phys_name_rj(i_fld),       &
     &          pick_sph_spec_name(ist+1), pick_sph_spec_name(ist+2),   &
     &          pick_sph_spec_name(ist+3))
          else if(num_phys_comp_rj(i_fld) .eq. 6) then
            call add_tensor_direction_label_rtp(phys_name_rj(i_fld),    &
     &          pick_sph_spec_name(ist+1), pick_sph_spec_name(ist+2),   &
     &          pick_sph_spec_name(ist+3), pick_sph_spec_name(ist+4),   &
     &          pick_sph_spec_name(ist+5), pick_sph_spec_name(ist+6))
          end if
          ist = ist + num_phys_comp_rj(i_fld)
        end if
      end do
      ncomp_pick_sph_coef = ist
!
      end subroutine set_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_coefs
