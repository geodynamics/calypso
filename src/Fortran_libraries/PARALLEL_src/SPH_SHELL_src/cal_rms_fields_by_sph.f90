!>@file   cal_rms_fields_by_sph.f90
!!@brief      module cal_rms_fields_by_sph
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine init_rms_4_sph_spectr
!!
!!      subroutine cal_rms_sph_spec_rms_whole
!!      subroutine cal_rms_sph_outer_core
!!      subroutine cal_rms_sph_inner_core
!!@endverbatim
!
      module cal_rms_fields_by_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_spheric_parameter
!
      implicit none
!
      private :: cal_mean_squre_in_shell
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr
!
      use calypso_mpi
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use sum_sph_rms_data
      use volume_average_4_sph
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      num_rms_rj = 0
      do i_fld = 1, num_phys_rj
        num_rms_rj = num_rms_rj + iflag_monitor_rj(i_fld)
      end do
!
      call allocate_rms_name_sph_spec
!
      j_fld = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
          j_fld = j_fld + 1
          ifield_rms_rj(j_fld) =   i_fld
          num_rms_comp_rj(j_fld) = num_phys_comp_rj(i_fld)
          istack_rms_comp_rj(j_fld) = istack_rms_comp_rj(j_fld-1)       &
     &                              + num_phys_comp_rj(i_fld)
          rms_name_rj(j_fld) =     phys_name_rj(i_fld)
        end if
      end do
      ntot_rms_rj = istack_rms_comp_rj(num_rms_rj)
!
      call allocate_rms_4_sph_spectr(my_rank)
      call allocate_ave_4_sph_spectr(ntot_rms_rj)
      call set_sum_table_4_sph_spectr
!
      end subroutine init_rms_4_sph_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_rms_whole
!
      call cal_mean_squre_in_shell(ione, nidx_rj(1))
!
      end subroutine cal_rms_sph_spec_rms_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_inner_core
!
      call cal_mean_squre_in_shell(izero, nlayer_ICB)
!
      end subroutine cal_rms_sph_inner_core
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_outer_core
!
      call cal_mean_squre_in_shell(nlayer_ICB, nlayer_CMB)
!
      end subroutine cal_rms_sph_outer_core
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_mean_squre_in_shell(kr_st, kr_ed)
!
      use m_rms_4_sph_spectr
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
!
      real(kind = kreal) :: avol
!
!
      if(ntot_rms_rj .eq. 0) return

      if(iflag_debug .gt. 0) write(*,*) 'cal_one_over_volume'
      call cal_one_over_volume(kr_st, kr_ed, avol)
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_rms'
      call sum_sph_layerd_rms
!
      if(iflag_debug .gt. 0) write(*,*) 'r_int_sph_rms_data'
      call r_int_sph_rms_data(kr_st, kr_ed, avol)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_average_sph'
      call cal_volume_average_sph(kr_st, kr_ed, avol)
!
      end subroutine cal_mean_squre_in_shell
!
! ----------------------------------------------------------------------
!
      end module cal_rms_fields_by_sph
