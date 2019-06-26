!>@file   pickup_sph_spectr_data.f90
!!@brief  module pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine init_sph_spec_4_monitor(sph_params, sph_rj, rj_fld,  &
!!     &          pick_list, picked)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(pickup_mode_list), intent(inout) :: pick_list
!!        type(picked_spectrum_data), intent(inout) :: picked
!!      subroutine pick_degre0_sped_4_monitor                           &
!!     &         (rj_fld, picked, ntot_comp_rj, d_rj_out)
!!      subroutine pick_single_sph_spec_4_monitor(inum, knum,           &
!!     &          sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
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
      use t_spheric_parameter
      use t_phys_data
      use t_pickup_sph_spectr_data
!
      implicit  none
!
      private :: copy_rj_spectrum_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_spec_4_monitor(sph_params, sph_rj, rj_fld,    &
     &          pick_list, picked)
!
      use calypso_mpi
      use quicksort
!
      use t_phys_data
      use pickup_sph_coefs
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: l, num
      integer(kind = kint) :: iflag_center = 0
!
!
      num = pick_list%num_modes                                         &
     &     + pick_list%num_degree + pick_list%num_order
      if(num .eq. 0) return
!
      call init_sph_radial_monitor_list(sph_rj, picked, iflag_center)
!
      call count_sph_labels_4_monitor(rj_fld%num_phys,                  &
     &    rj_fld%num_component, rj_fld%iflag_monitor, picked)
!
      if(pick_list%num_degree .eq. -9999) then
        pick_list%num_degree = sph_params%l_truncation+ 1 
        call alloc_pick_sph_l(pick_list)
        do l = 0, sph_params%l_truncation
          pick_list%idx_pick_l(l+1) = l
        end do
      end if
!
      call const_picked_sph_address(iflag_center,                       &
     &    sph_params%l_truncation, sph_rj, pick_list, picked)
!
      call set_sph_fld_id_4_monitor(rj_fld%num_phys,                    &
     &    rj_fld%num_component, rj_fld%iflag_monitor, picked)
!
      if(my_rank .ne. 0) return
      call set_sph_labels_4_monitor                                     &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%phys_name, picked)
!
      end subroutine init_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pick_degre0_sped_4_monitor                             &
     &         (rj_fld, picked, ntot_comp_rj, d_rj_out)
!
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_rj)
!
!
      call copy_rj_spectrum_4_monitor(picked%idx_out(0,4),              &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, half, d_rj_out)
!
      end subroutine pick_degre0_sped_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pick_single_sph_spec_4_monitor(inum, knum,             &
     &          sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
!
      integer(kind = kint), intent(in) :: inum, knum
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_rj)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: prod_4_zero
!
!
      if(picked%idx_out(inum,1) .eq. 0) then
        prod_4_zero = 0.5d0
      else
        prod_4_zero = 1.0d0
      end if
!
      inod = picked%idx_out(inum,4)                                     &
     &      + (picked%id_radius(knum) - 1) * sph_rj%nidx_rj(2)
!
      call copy_rj_spectrum_4_monitor                                   &
     &   (inod, rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,      &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, prod_4_zero, d_rj_out)
!
      end subroutine pick_single_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_rj_spectrum_4_monitor(inod, n_point,              &
     &         num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj,    &
     &         nfld_monitor, istack_comp_monitor, ifld_monitor,         &
     &         ntot_comp_monitor, prod_degree_zero, d_rj_out)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
      real(kind=kreal), intent(in) :: prod_degree_zero
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
!
!
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            d_rj_out(jcou+1) = prod_degree_zero * d_rj(inod,icou+1)
            d_rj_out(jcou+2) = prod_degree_zero * d_rj(inod,icou+3)
            d_rj_out(jcou+3) = prod_degree_zero * d_rj(inod,icou+2)
        else
          do nd = 1, ncomp
            d_rj_out(jcou+nd) = d_rj(inod,icou+nd)
          end do
        end if
      end do
!
      end subroutine copy_rj_spectrum_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr_data
