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
!
      implicit none
!
      private :: r_int_sph_rms_data, cal_average_for_sph_rms
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr
!
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use sum_sph_rms_data
!
      integer(kind = kint) :: i, icou
!
!
      num_rms_rj = 0
      do i = 1, num_phys_rj
        num_rms_rj = num_rms_rj + iflag_monitor_rj(i)
      end do
!
      call allocate_rms_name_sph_spec
!
      icou = 0
      do i = 1, num_phys_rj
        if(iflag_monitor_rj(i) .gt. 0) then
          icou = icou + 1
          num_rms_comp_rj(icou) = num_phys_comp_rj(i)
          istack_rms_comp_rj(icou) = istack_rms_comp_rj(icou-1)         &
     &                              + num_phys_comp_rj(i)
          rms_name_rj(icou) =     phys_name_rj(i)
        end if
      end do
      ntot_rms_rj = istack_rms_comp_rj(num_rms_rj)
!
      call allocate_rms_4_sph_spectr
      call set_sum_table_4_sph_spectr
!
      end subroutine init_rms_4_sph_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_rms_whole
!
      use m_spheric_parameter
      use sum_sph_rms_data
!
!
      call cal_rms_sph_spec_local
!
      call r_int_sph_rms_data(ione, nidx_global_rj(1))
      call cal_average_for_sph_rms(ione, nidx_global_rj(1))
!
      end subroutine cal_rms_sph_spec_rms_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_inner_core
!
      use m_spheric_parameter
!
!
      call cal_rms_sph_spec_local
!
      call r_int_sph_rms_data(izero, nlayer_ICB)
      call cal_average_for_sph_rms(izero, nlayer_ICB)
!
      end subroutine cal_rms_sph_inner_core
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_outer_core
!
      use m_spheric_parameter
!
!
      call cal_rms_sph_spec_local
!
      call r_int_sph_rms_data(nlayer_ICB, nlayer_CMB)
      call cal_average_for_sph_rms(nlayer_ICB, nlayer_CMB)
!
      end subroutine cal_rms_sph_outer_core
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_local
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
!
      integer(kind = kint) :: i, icomp_st, icou
      integer(kind = kint) :: num
!
!
      call clear_rms_sph_spectr
!
      icou = 1
      do i = 1, num_phys_rj
        if(iflag_monitor_rj(i) .gt. 0) then
          icomp_st = istack_phys_comp_rj(i-1) + 1
          if (num_phys_comp_rj(i) .eq. n_scalar) then
            call cal_ave_scalar_sph_spectr(icomp_st, icou)
            call cal_rms_each_scalar_sph_spec(icomp_st, icou)
          else if (num_phys_comp_rj(i) .eq. n_vector) then
            call cal_ave_vector_sph_spectr(icomp_st, icou)
            call cal_rms_each_vector_sph_spec(icomp_st, icou)
!
            if (   icomp_st .eq. ipol%i_velo                            &
     &        .or. icomp_st .eq. ipol%i_magne                           &
     &        .or. icomp_st .eq. ipol%i_filter_velo                     &
     &        .or. icomp_st .eq. ipol%i_filter_magne) then
              call set_sph_energies_by_rms(icou)
            end if
!
          end if
          icou = icou + num_phys_comp_rj(i)
        end if
      end do
!
      num = ntot_rms_rj * nidx_rj(1)
      call MPI_allREDUCE (ave_sph_lc(1,1), ave_sph(1,1), num,           &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      call sum_sph_layerd_rms
!
      end subroutine cal_rms_sph_spec_local
!
! -----------------------------------------------------------------------
!
      subroutine r_int_sph_rms_data(kg_st, kg_ed)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use radial_int_for_sph_spec
      use sum_sph_rms_data
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      integer(kind = kint) :: ntot_comp
!
!
      ntot_comp = ntot_rms_rj * nidx_rj(2)
      call radial_integration(nidx_rj(1), kg_st, kg_ed, radius_1d_rj_r, &
     &    ntot_comp, rms_sph_dat(1,1), rms_sph_vol_dat(1,1) )
!
      if(my_rank .gt. 0) return
!
      call radial_integration(nidx_rj(1), kg_st, kg_ed, radius_1d_rj_r, &
     &    ntot_rms_rj, ave_sph(1,1), ave_sph_vol(1) )
!
!
      ntot_comp = ntot_rms_rj * (l_truncation+1)
      call radial_integration(nidx_rj(1), kg_st, kg_ed, radius_1d_rj_r, &
     &    ntot_comp, rms_sph_l(1,0,1), rms_sph_vol_l(1,0) )
!
      call radial_integration(nidx_rj(1), kg_st, kg_ed, radius_1d_rj_r, &
     &    ntot_comp, rms_sph_m(1,0,1), rms_sph_vol_m(1,0) )
!
      call radial_integration(nidx_rj(1), kg_st, kg_ed, radius_1d_rj_r, &
     &    ntot_comp, rms_sph_lm(1,0,1), rms_sph_vol_lm(1,0) )
!
      call sum_sph_rms_all_modes(l_truncation, ione, ntot_rms_rj,       &
     &    rms_sph_vol_l(1,0), rms_sph_vol(1) )
!
!
      end subroutine r_int_sph_rms_data
!
! -----------------------------------------------------------------------
!
      subroutine cal_average_for_sph_rms(kg_st, kg_ed)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
!
      real(kind = kreal) :: avol
!
!
      if(kg_st .eq. 0) then
        avol = three / (radius_1d_rj_r(kg_ed)**3)
      else
        avol = three / (radius_1d_rj_r(kg_ed)**3                        &
     &                - radius_1d_rj_r(kg_st)**3 )
      end if
!
      call surf_ave_4_each_sph_rms
      call vol_ave_4_each_sph_rms(avol)
!
      if(my_rank .gt. 0) return
!
      call surf_ave_4_sph_rms_int
      call vol_ave_4_rms_sph(avol)
!
      end subroutine cal_average_for_sph_rms
!
! -----------------------------------------------------------------------
!
      end module cal_rms_fields_by_sph
