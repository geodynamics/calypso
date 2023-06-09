!> @file  swap_full_perturbation_sph.f90
!!      module swap_full_perturbation_sph
!!
!! @author  H. Matsui
!! @date Programmed in Sep., 2007
!
!> @brief Convert temperature data using reference temperature
!!
!!@verbatim
!!      subroutine chenge_temp_to_per_temp_sph                          &
!!     &         (idx_rj_degree_zero, inod_rj_center, nnod_rj, nidx_rj, &
!!     &          radius_1d_rj_r, reference_r, refgrad_r,               &
!!     &          scalar_rj, grad_rj, pert_rj, grad_pert_rj)
!!        d_rj(inod,is_tempp):        T => \Theta = T - T0
!!        d_rj(inod,is_par_temp):    \Theta = T - T0
!!        d_rj(inod,is_grad_t):      T => d \Theta / dr
!!        d_rj(inod,is_grad_part_t): d \Theta / dr
!!      subroutine back_per_temp_to_temp_sph                            &
!!     &         (idx_rj_degree_zero, inod_rj_center, nnod_rj, nidx_rj, &
!!     &          radius_1d_rj_r, reference_r, refgrad_r,               &
!!     &          scalar_rj, grad_rj, pert_rj, grad_pert_rj)
!!        d_rj(inod,is_temp):        \Theta = T - T0 => T
!!        d_rj(inod,is_par_temp):    \Theta = T - T0
!!        d_rj(inod,is_grad_t):      d \Theta / dr   => dT / dr
!!        d_rj(inod,is_grad_part_t): d \Theta / dr
!!
!!      subroutine remove_ref_source_sph(sph_rj, source_rj)
!!          d_rj(inod,is_source):      Q = 0
!!      subroutine back_ref_source_sph(sph_rj, ref_source_r,            &
!!     &                               source_rj)
!!          d_rj(inod,is_source):      Q = Q0
!!         type(sph_rj_grid), intent(in) ::  sph_rj
!!        real(kind=kreal), intent(in)                                  &
!!     &                   :: ref_source_r(0:sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(inout) :: source_rj(sph_rj%nnod_rj)
!!@endverbatim
!!
!!@n @param is_fld Address of poloidal component
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module swap_full_perturbation_sph
!
      use m_precision
!
      use m_constants
      use t_spheric_rj_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine chenge_temp_to_per_temp_sph                            &
     &         (sph_rj, reference_r, refgrad_r,                         &
     &          scalar_rj, grad_rj, pert_rj, grad_pert_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      real(kind=kreal), intent(in) :: reference_r(0:sph_rj%nidx_rj(1))
      real(kind=kreal), intent(in) :: refgrad_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: scalar_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: grad_rj(sph_rj%nnod_rj,3)
!
      real(kind = kreal), intent(inout) :: pert_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: grad_pert_rj(sph_rj%nnod_rj,3)
!
      integer(kind = kint) :: k, inod
!
!
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        do k = 1, sph_rj%nidx_rj(1)
          inod = 1 + (sph_rj%idx_rj_degree_zero) * sph_rj%istep_rj(1)   &
     &             + (k-1)*sph_rj%istep_rj(2)
          scalar_rj(inod) = scalar_rj(inod) - reference_r(k)
          grad_rj(inod,1) = grad_rj(inod,1)                             &
     &                 - two*refgrad_r(k) * sph_rj%radius_1d_rj_r(k)**2
        end do
      end if
!
!$omp parallel workshare
      pert_rj(1:sph_rj%nnod_rj) =        scalar_rj(1:sph_rj%nnod_rj)
      grad_pert_rj(1:sph_rj%nnod_rj,1) = grad_rj(1:sph_rj%nnod_rj,1)
      grad_pert_rj(1:sph_rj%nnod_rj,2) = grad_rj(1:sph_rj%nnod_rj,2)
      grad_pert_rj(1:sph_rj%nnod_rj,3) = grad_rj(1:sph_rj%nnod_rj,3)
!$omp end parallel workshare
!
      if(sph_rj%inod_rj_center .gt. 0) then
        scalar_rj(sph_rj%inod_rj_center)                                &
     &         = scalar_rj(sph_rj%inod_rj_center) - reference_r(0)
        pert_rj(sph_rj%inod_rj_center)                                  &
     &          =   scalar_rj(sph_rj%inod_rj_center)
      end if
!
      end subroutine chenge_temp_to_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine back_per_temp_to_temp_sph                              &
     &         (sph_rj, reference_r, refgrad_r,                         &
     &          scalar_rj, grad_rj, pert_rj, grad_pert_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      real(kind=kreal), intent(in) :: reference_r(0:sph_rj%nidx_rj(1))
      real(kind=kreal), intent(in) :: refgrad_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: scalar_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: grad_rj(sph_rj%nnod_rj,3)
!
      real(kind = kreal), intent(inout) :: pert_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: grad_pert_rj(sph_rj%nnod_rj,3)
!
!
      integer(kind = kint) :: k, inod
!
!
!$omp parallel workshare
      pert_rj(1:sph_rj%nnod_rj) =        scalar_rj(1:sph_rj%nnod_rj)
      grad_pert_rj(1:sph_rj%nnod_rj,1) = grad_rj(1:sph_rj%nnod_rj,1)
      grad_pert_rj(1:sph_rj%nnod_rj,2) = grad_rj(1:sph_rj%nnod_rj,2)
      grad_pert_rj(1:sph_rj%nnod_rj,3) = grad_rj(1:sph_rj%nnod_rj,3)
!$omp end parallel workshare
!
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        do k = 1, sph_rj%nidx_rj(1)
          inod = 1 + (sph_rj%idx_rj_degree_zero) * sph_rj%istep_rj(1)   &
     &             + (k-1)*sph_rj%istep_rj(2)
          scalar_rj(inod) = scalar_rj(inod) + reference_r(k)
          grad_rj(inod,1) = grad_pert_rj(inod,1)                        &
     &                 + two*refgrad_r(k) * sph_rj%radius_1d_rj_r(k)**2
        end do
      end if
!
      if(sph_rj%inod_rj_center .gt. 0) then
        pert_rj(sph_rj%inod_rj_center)                                  &
     &           =   scalar_rj(sph_rj%inod_rj_center)
        scalar_rj(sph_rj%inod_rj_center)                                &
     &           = scalar_rj(sph_rj%inod_rj_center) + reference_r(0)
      end if
!
      end subroutine back_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine remove_ref_source_sph(sph_rj, source_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(inout) :: source_rj(sph_rj%nnod_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        do k = 1, sph_rj%nidx_rj(1)
          inod = 1 + (sph_rj%idx_rj_degree_zero) * sph_rj%istep_rj(1)   &
     &             + (k-1)*sph_rj%istep_rj(2)
          source_rj(inod) = 0.0d0
        end do
      end if
!
      if(sph_rj%inod_rj_center .gt. 0) then
        source_rj(sph_rj%inod_rj_center) = 0.0d0
      end if
!
      end subroutine remove_ref_source_sph
!
! -----------------------------------------------------------------------
!
      subroutine back_ref_source_sph(sph_rj, ref_source_r,              &
     &                               source_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind=kreal), intent(in) :: ref_source_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: source_rj(sph_rj%nnod_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        do k = 1, sph_rj%nidx_rj(1)
          inod = 1 + (sph_rj%idx_rj_degree_zero) * sph_rj%istep_rj(1)   &
     &             + (k-1)*sph_rj%istep_rj(2)
          source_rj(inod) = ref_source_r(k)
        end do
      end if
!
      if(sph_rj%inod_rj_center .gt. 0) then
        source_rj(sph_rj%inod_rj_center) = ref_source_r(0)
      end if
!
      end subroutine back_ref_source_sph
!
! -----------------------------------------------------------------------
!
      end module swap_full_perturbation_sph
