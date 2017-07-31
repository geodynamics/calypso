!>@file   pickup_sph_coefs.f90
!!@brief      module pickup_sph_coefs
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Pick spectr data to output
!!
!!@verbatim
!!      subroutine init_sph_radial_monitor_list(sph_rj, picked)
!!      subroutine count_sph_labels_4_monitor                           &
!!     &       (num_phys_rj, num_phys_comp_rj, iflag_monitor_rj, picked)
!!      subroutine set_sph_fld_id_4_monitor                             &
!!     &       (num_phys_rj, num_phys_comp_rj, iflag_monitor_rj, picked)
!!      subroutine set_sph_labels_4_monitor                             &
!!     &        (num_phys_rj, num_phys_comp_rj, phys_name_rj, picked)
!!@endverbatim
!
      module pickup_sph_coefs
!
      use m_precision
      use m_constants
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
      subroutine init_sph_radial_monitor_list(sph_rj, picked)
!
      use calypso_mpi
      use quicksort
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: k, knum
!
!
      if(picked%num_layer .le. 0) then
        picked%num_layer = sph_rj%nidx_rj(1) + sph_rj%iflag_rj_center
!
        call alloc_num_pick_layer(picked)
!
        do k = 1, picked%num_layer
          picked%id_radius(k) = k - sph_rj%iflag_rj_center
        end do
      end if
      call quicksort_int(picked%num_layer, picked%id_radius,            &
     &    ione, picked%num_layer)
!
!
      do knum = 1, picked%num_layer
        k = picked%id_radius(knum)
        if(k .le. 0) then
          picked%radius_gl(knum) = 0.0d0
        else
          picked%radius_gl(knum) = sph_rj%radius_1d_rj_r(k)
        end if
      end do
!
      end subroutine init_sph_radial_monitor_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_sph_labels_4_monitor                             &
     &        (num_phys_rj, num_phys_comp_rj, iflag_monitor_rj, picked)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      integer(kind = kint), intent(in) :: iflag_monitor_rj(num_phys_rj)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: i_fld
!
!
      picked%num_field_rj = 0
      picked%ntot_comp_rj = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
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
     &        (num_phys_rj, num_phys_comp_rj, iflag_monitor_rj, picked)
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      integer(kind = kint), intent(in) :: iflag_monitor_rj(num_phys_rj)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      j_fld = 0
      picked%istack_comp_rj(0) = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
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
