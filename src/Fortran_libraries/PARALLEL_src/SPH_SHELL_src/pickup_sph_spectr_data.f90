!>@file   pickup_sph_spectr_data.f90
!!@brief  module pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine init_sph_spec_4_monitor(l_truncation, sph_rj, rj_fld,&
!!     &          pick_list, picked)
!!      subroutine pickup_sph_spec_4_monitor(sph_rj, n_point,           &
!!     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj, &
!!     &          picked)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(pickup_mode_list), intent(inout) :: pick_list
!!        type(picked_spectrum_data), intent(inout) :: picked
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module pickup_sph_spectr_data
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_spec_4_monitor(l_truncation, sph_rj, rj_fld,  &
     &          pick_list, picked)
!
      use calypso_mpi
      use quicksort
!
      use t_phys_data
      use pickup_sph_coefs
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: l, num
!
!
      num = pick_list%num_modes                                         &
     &     + pick_list%num_degree + pick_list%num_order
      if(num .eq. 0) return
!
      call init_sph_radial_monitor_list(sph_rj, picked)
!
      call count_sph_labels_4_monitor(rj_fld%num_phys,                  &
     &    rj_fld%num_component, rj_fld%iflag_monitor, picked)
!
      if(pick_list%num_degree .eq. -9999) then
        pick_list%num_degree = l_truncation+1
        call alloc_pick_sph_l(pick_list)
        do l = 0, l_truncation
          pick_list%idx_pick_l(l+1) = l
        end do
      end if
!
      call const_picked_sph_address                                     &
     &   (l_truncation, sph_rj, pick_list, picked)
!
      call set_sph_fld_id_4_monitor(rj_fld%num_phys,                    &
     &    rj_fld%num_component, rj_fld%iflag_monitor, picked)
!
      call alloc_scale_4_l0(picked)
      call set_scale_4_vect_l0                                          &
     &   (picked%num_sph_mode, picked%idx_gl, picked%scale_for_zelo)
!
      if(my_rank .ne. 0) return
      call set_sph_labels_4_monitor                                     &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%phys_name, picked)
!
      end subroutine init_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_spec_4_monitor(sph_rj, n_point,             &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj,   &
     &          picked)
!
      use calypso_mpi
      use pickup_sph_coefs
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: inum, knum, j, k, nd, i_fld, j_fld
      integer(kind = kint) :: inod, ipick, num, icou, jcou, kst, ncomp
!
!
      if(picked%num_sph_mode * picked%num_layer .eq. 0) return
!
!$omp parallel do
      do inum = 1, picked%num_sph_mode*picked%num_layer
        picked%d_rj_lc(1:picked%ntot_comp_rj,inum) = zero
      end do
!$omp end parallel do
!
!   Set field at center
      kst = 1
      if(picked%idx_gl(1,1).eq.0 .and. sph_rj%iflag_rj_center.gt.0) then
        kst = kst + 1
        inod = sph_rj%inod_rj_center
!
        do j_fld = 1, picked%num_field_rj
          i_fld = picked%ifield_monitor_rj(j_fld)
          ncomp = istack_phys_comp_rj(i_fld)                            &
     &           - istack_phys_comp_rj(i_fld-1)
          icou = istack_phys_comp_rj(i_fld-1)
          jcou = picked%istack_comp_rj(j_fld-1)
          if(ncomp .eq. 3) then
             picked%d_rj_lc(jcou+1,1) = 0.0d0
             picked%d_rj_lc(jcou+2,1) = 0.0d0
             picked%d_rj_lc(jcou+3,1) = 0.0d0
           else
             do nd = 1, ncomp
               picked%d_rj_lc(jcou+nd,1)= d_rj(inod,icou+nd)
             end do
           end if
         end do
!
      end if
!
!!$omp parallel private(j)
      do inum = 1, picked%num_sph_mode
        j = picked%idx_lc(inum)
        if(j .gt. izero) then
!!$omp do private(knum,k,inod,ipick,j_fld,i_fld,icou,jcou,nd)
          do knum = kst, picked%num_layer
            k = picked%id_radius(knum)
            inod =  j +    (k-1) * sph_rj%nidx_rj(2)
            ipick = knum + (inum-1) * picked%num_layer
!
            do j_fld = 1, picked%num_field_rj
              i_fld = picked%ifield_monitor_rj(j_fld)
              ncomp = istack_phys_comp_rj(i_fld)                        &
     &               - istack_phys_comp_rj(i_fld-1)
              icou = istack_phys_comp_rj(i_fld-1)
              jcou = picked%istack_comp_rj(j_fld-1)
              if(ncomp .eq. 3) then
                  picked%d_rj_lc(jcou+1,ipick)                          &
     &                = picked%scale_for_zelo(inum) * d_rj(inod,icou+1)
                  picked%d_rj_lc(jcou+2,ipick)                          &
     &                = picked%scale_for_zelo(inum) * d_rj(inod,icou+3)
                  picked%d_rj_lc(jcou+3,ipick)                          &
     &                = picked%scale_for_zelo(inum) * d_rj(inod,icou+2)
              else
                do nd = 1, ncomp
                  picked%d_rj_lc(jcou+nd,ipick)= d_rj(inod,icou+nd)
                end do
              end if
            end do
!
          end do
!!$omp end do nowait
        end if
      end do
!!$omp end parallel
!
      num = picked%ntot_comp_rj * picked%num_layer*picked%num_sph_mode
      call MPI_allREDUCE(picked%d_rj_lc, picked%d_rj_gl,                &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr_data
