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
!!      subroutine pick_center_spectrum_monitor                         &
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
      if(num .eq. 0) then
        picked%num_sph_mode_lc = 0
        return
      end if
!
      call init_sph_radial_monitor_list(sph_rj, picked, iflag_center)
!
      call count_sph_labels_4_monitor(rj_fld%num_phys,                  &
     &    rj_fld%num_component, rj_fld%flag_monitor, picked)
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
     &    rj_fld%num_component, rj_fld%flag_monitor, picked)
!
      call set_sph_labels_4_monitor                                     &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%phys_name, picked)
!
      end subroutine init_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pick_center_spectrum_monitor                           &
     &         (rj_fld, picked, ntot_comp_monitor, d_rj_out)
!
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_monitor
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_monitor)
!
      integer(kind = kint) :: inod, nd, i_fld, j_fld, icou, jcou, ncomp
!
!
      inod = picked%idx_out(0,4)
      if(inod .le. 0) return
!$omp parallel do private(j_fld,i_fld,ncomp,icou,jcou,nd)
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
            d_rj_out(jcou+1) = half * rj_fld%d_fld(inod,icou+1)
            d_rj_out(jcou+2) = half * rj_fld%d_fld(inod,icou+3)
            d_rj_out(jcou+3) = half * rj_fld%d_fld(inod,icou+2)
        else
          do nd = 1, ncomp
            d_rj_out(jcou+nd) = rj_fld%d_fld(inod,icou+nd)
          end do
        end if
      end do
!$omp end parallel do
!
      end subroutine pick_center_spectrum_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pick_single_sph_spec_4_monitor(inum, knum,             &
     &          sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
!
      use single_pt_sph_mean_square
!
      integer(kind = kint), intent(in) :: inum, knum
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_rj)
!
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
      integer(kind = kint) :: j, k_in, k_out, i_in, i_out
      real(kind = kreal) :: prod_degree_zero
!
!
      if(picked%idx_out(inum,1) .eq. 0) then
        prod_degree_zero = 0.5d0
      else
        prod_degree_zero = 1.0d0
      end if
!
      j =     picked%idx_out(inum,4)
      k_in =  picked%id_radius(knum,1)
      k_out = picked%id_radius(knum,2)
      i_in =  1 + (k_in-1) * sph_rj%istep_rj(1)                         &
     &          + (j-1) *    sph_rj%istep_rj(2)
      i_out =  1 + (k_out-1) * sph_rj%istep_rj(1)                       &
     &           + (j-1) *      sph_rj%istep_rj(2)
!
!$omp parallel do private(j_fld,i_fld,ncomp,icou,jcou,nd)
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
          call interpolate_rj_vec_spec                                  &
     &       (rj_fld%d_fld(i_in,icou+1),  rj_fld%d_fld(i_in,icou+2),    &
     &        rj_fld%d_fld(i_in,icou+3),  rj_fld%d_fld(i_out,icou+1),   &
     &        rj_fld%d_fld(i_out,icou+2), rj_fld%d_fld(i_out,icou+3),   &
     &        picked%coef_radius_gl(knum), d_rj_out(jcou+1),            &
     &        d_rj_out(jcou+3), d_rj_out(jcou+2))
          d_rj_out(jcou+1:jcou+3)                                       &
     &            = prod_degree_zero * d_rj_out(jcou+1:jcou+3)
        else
          do nd = 1, ncomp
            d_rj_out(jcou+nd)                                           &
     &        = interpolate_rj_scalar_spec(rj_fld%d_fld(i_in,icou+nd),  &
     &                                     rj_fld%d_fld(i_out,icou+nd), &
     &                                     picked%coef_radius_gl(knum))
          end do
        end if
      end do
!$omp end parallel do
!
      end subroutine pick_single_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr_data
