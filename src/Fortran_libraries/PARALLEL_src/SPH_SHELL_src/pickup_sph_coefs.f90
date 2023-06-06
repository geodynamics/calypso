!>@file   pickup_sph_coefs.f90
!!@brief      module pickup_sph_coefs
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Pick spectr data to output
!!
!!@verbatim
!!      subroutine init_sph_radial_monitor_list                         &
!!     &         (sph_rj, picked, iflag_center)
!!      subroutine count_sph_labels_4_monitor                           &
!!     &       (num_phys_rj, num_phys_comp_rj, flag_monitor_rj, picked)
!!      subroutine set_sph_fld_id_4_monitor                             &
!!     &       (num_phys_rj, num_phys_comp_rj, flag_monitor_rj, picked)
!!      subroutine set_sph_labels_4_monitor                             &
!!     &        (num_phys_rj, num_phys_comp_rj, phys_name_rj, picked)
!!@endverbatim
!
      module pickup_sph_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_pickup_sph_spectr_data
!
      use pickup_sph_spectr
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_radial_monitor_list                           &
     &         (sph_rj, picked, iflag_center)
!
      use calypso_mpi
      use quicksort
      use set_radial_interpolation
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(picked_spectrum_data), intent(inout) :: picked
      integer(kind = kint), intent(inout) :: iflag_center
!
      integer(kind = kint) :: k, kg, kr_st
!
!
      if(picked%num_layer .le. 0) then
        picked%num_layer = sph_rj%nidx_rj(1)
        call alloc_num_pick_layer(picked)
!
!$omp parallel do
        do k = 1, picked%num_layer
          picked%id_radius(k,1) = k
          picked%radius_gl(k,1:2) = -one
        end do
!$omp end parallel do
      end if
!
!$omp parallel do
      do k = 1, picked%num_layer
        if(picked%radius_gl(k,1) .lt. zero) then
          if(picked%id_radius(k,1) .eq. 0) then
            picked%radius_gl(k,1) = zero
          else
            kg = picked%id_radius(k,1)
            picked%radius_gl(k,1) = sph_rj%radius_1d_rj_r(kg)
          end if
        end if
      end do
!$omp end parallel do
!
      if(picked%num_layer .gt. 1) then
        call quicksort_real_w_index(picked%num_layer,                   &
     &      picked%radius_gl(1,1), ione, picked%num_layer,              &
     &      picked%id_radius(1,1))
      end if
!
      kr_st = 1
      do k = 1, picked%num_layer
        if(picked%radius_gl(k,1) .eq. zero) then
          picked%id_radius(k,2) = picked%id_radius(k,1)
          picked%radius_gl(k,2) = zero
          picked%coef_radius_gl(k) = one
        else if(picked%id_radius(k,1) .gt. 0) then
          picked%id_radius(k,2) = picked%id_radius(k,1)
          picked%radius_gl(k,2) = one / picked%radius_gl(k,1)
          picked%coef_radius_gl(k) = one
        else
          call s_set_radial_interpolation                               &
     &       (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                 &
     &        picked%radius_gl(k,1), kr_st,                             &
     &        picked%id_radius(k,1), picked%id_radius(k,2),             &
     &        picked%coef_radius_gl(k))
          picked%radius_gl(k,2) = one / picked%radius_gl(k,1)
        end if
      end do
!
      iflag_center = 0
      if(sph_rj%iflag_rj_center.gt.0 .and. picked%id_radius(1,1).eq.1)  &
     &     iflag_center = 1
!
      if(iflag_debug .eq. 0) return
!      if(my_rank .gt. 0) return
      write(*,*) 'Picked spectr later data:', picked%num_layer
      do k = 1, picked%num_layer
        write(*,*) k, picked%radius_gl(k,1), picked%id_radius(k,1:2),   &
     &            sph_rj%radius_1d_rj_r(picked%id_radius(k,1:2)),       &
     &            picked%coef_radius_gl(k)
      end do
!
      end subroutine init_sph_radial_monitor_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_sph_labels_4_monitor                             &
     &        (num_phys_rj, num_phys_comp_rj, flag_monitor_rj, picked)
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      logical, intent(in) :: flag_monitor_rj(num_phys_rj)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: i_fld
!
!
      picked%num_field_rj = 0
      picked%ntot_comp_rj = 0
      do i_fld = 1, num_phys_rj
        if(flag_monitor_rj(i_fld)) then
          picked%num_field_rj = picked%num_field_rj + 1
          picked%ntot_comp_rj = picked%ntot_comp_rj                     &
     &                        + num_phys_comp_rj(i_fld)
        end if
      end do
!
      end subroutine count_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_fld_id_4_monitor                               &
     &        (num_phys_rj, num_phys_comp_rj, flag_monitor_rj, picked)
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      logical, intent(in) :: flag_monitor_rj(num_phys_rj)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      j_fld = 0
      picked%istack_comp_rj(0) = 0
      do i_fld = 1, num_phys_rj
        if(flag_monitor_rj(i_fld)) then
          j_fld = j_fld + 1
          picked%istack_comp_rj(j_fld) = picked%istack_comp_rj(j_fld-1) &
     &                                 + num_phys_comp_rj(i_fld)
          picked%ifield_monitor_rj(j_fld) = i_fld
        end if
      end do
!
      end subroutine set_sph_fld_id_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_labels_4_monitor                               &
     &         (num_phys_rj, num_phys_comp_rj, phys_name_rj, picked)
!
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      character(len=kchara), intent(in) :: phys_name_rj(num_phys_rj)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: i_fld, j_fld, jcou
!
!
      picked%istack_comp_rj(0) = 0
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(num_phys_comp_rj(i_fld) .eq. 1) then
          write(picked%spectr_name(jcou+1),'(a)')                       &
     &                      trim(phys_name_rj(i_fld))
        else if(num_phys_comp_rj(i_fld) .eq. 3) then
          call add_vector_sph_spectr_label(phys_name_rj(i_fld),         &
     &          picked%spectr_name(jcou+1), picked%spectr_name(jcou+2), &
     &          picked%spectr_name(jcou+3))
        else if(num_phys_comp_rj(i_fld) .eq. 6) then
          call add_tensor_direction_label_rtp(phys_name_rj(i_fld),      &
     &          picked%spectr_name(jcou+1), picked%spectr_name(jcou+2), &
     &          picked%spectr_name(jcou+3), picked%spectr_name(jcou+4), &
     &          picked%spectr_name(jcou+5), picked%spectr_name(jcou+6))
        end if
      end do
!
      end subroutine set_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_coefs
