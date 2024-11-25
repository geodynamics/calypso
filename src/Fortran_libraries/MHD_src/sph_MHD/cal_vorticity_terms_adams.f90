!>@file   cal_vorticity_terms_adams.f90
!!@brief  module cal_vorticity_terms_adams
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evoluve the vorticity equation by explicit scheme 
!!
!!@verbatim
!!      subroutine cal_vorticity_eq_adams(sph_rj, fl_prop, sph_bc_U,    &
!!     &          ipol_base, ipol_exp, ipol_dif,                        &
!!     &          dt, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_vorticity_eq_euler(sph_rj, fl_prop, sph_bc_U,    &
!!     &          ipol_base, ipol_exp, ipol_dif,                        &
!!     &          dt, nnod_rj, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(diffusion_address), intent(in) :: ipol_dif
!!
!!      subroutine set_MHD_terms_to_force                               &
!!     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                   &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine set_rotating_cv_terms_to_force                       &
!!     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                   &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!
!!      subroutine subtract_advection_to_force                          &
!!     &         (is_exp, is_inertia, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_each_force_to_forces                             &
!!     &         (is_exp, is_force, nnod_rj, ntot_phys_rj, d_rj)
!!
!!      subroutine set_ini_adams_inertia                                &
!!     &         (fl_prop, ipol_exp, nnod_rj, ntot_phys_rj, d_rj)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!!
!!@n @param is_rot_buo  Spectr field address
!!                       for toroidal curl of buodyancy
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module cal_vorticity_terms_adams
!
      use m_precision
!
      use m_t_step_parameter
      use t_spheric_rj_data
      use t_physical_property
      use t_boundary_data_sph_MHD
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_adams(sph_rj, fl_prop, sph_bc_U,      &
     &          ipol_base, ipol_exp, ipol_dif,                          &
     &          dt, nnod_rj, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(diffusion_address), intent(in) :: ipol_dif
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, ist, ied
!
!
      if(fl_prop%iflag_scheme .eq. id_no_evolution) return
      ist = (sph_bc_U%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied = sph_bc_U%kr_out *    sph_rj%nidx_rj(2)
!$omp parallel do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_base%i_vort  ) = d_rj(inod,ipol_base%i_vort  )   &
     &     + dt * (fl_prop%coef_exp * d_rj(inod,ipol_dif%i_w_diffuse  ) &
     &                     + adam_0 * d_rj(inod,ipol_exp%i_forces  )    &
     &                     + adam_1 * d_rj(inod,ipol_exp%i_pre_mom  ))
        d_rj(inod,ipol_base%i_vort+2) = d_rj(inod,ipol_base%i_vort+2)   &
     &     + dt * (fl_prop%coef_exp * d_rj(inod,ipol_dif%i_w_diffuse+2) &
     &                     + adam_0 * d_rj(inod,ipol_exp%i_forces+2)    &
     &                     + adam_1 * d_rj(inod,ipol_exp%i_pre_mom+2))
!
        d_rj(inod,ipol_exp%i_pre_mom  )                                 &
     &        = d_rj(inod,ipol_exp%i_forces  )
        d_rj(inod,ipol_exp%i_pre_mom+2)                                 &
     &        = d_rj(inod,ipol_exp%i_forces+2)
      end do
!$omp end parallel do
!
      end subroutine cal_vorticity_eq_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_euler(sph_rj, fl_prop, sph_bc_U,      &
     &          ipol_base, ipol_exp, ipol_dif,                          &
     &          dt, nnod_rj, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(diffusion_address), intent(in) :: ipol_dif
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, ist, ied
!
!
      if(fl_prop%iflag_scheme .eq. id_no_evolution) return
      ist = (sph_bc_U%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_U%kr_out *   sph_rj%nidx_rj(2)
!$omp parallel do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_base%i_vort  ) = d_rj(inod,ipol_base%i_vort  )   &
     &     + dt * (fl_prop%coef_exp * d_rj(inod,ipol_dif%i_w_diffuse  ) &
     &                              + d_rj(inod,ipol_exp%i_forces  ))
!
        d_rj(inod,ipol_base%i_vort+2) = d_rj(inod,ipol_base%i_vort+2)   &
     &     + dt * (fl_prop%coef_exp * d_rj(inod,ipol_dif%i_w_diffuse+2) &
     &                              + d_rj(inod,ipol_exp%i_forces+2))
       end do
!$omp end parallel do
!
      end subroutine cal_vorticity_eq_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_MHD_terms_to_force                                 &
     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                     &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: is_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect  )                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis  )                  &
     &          + d_rj(inod,ipol_rot_frc%i_lorentz  )                   &
     &          + d_rj(inod,is_rot_buo  )
        d_rj(inod,ipol_exp%i_forces+1)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+1)                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis+1)                  &
     &          + d_rj(inod,ipol_rot_frc%i_lorentz+1)                   &
     &          + d_rj(inod,is_rot_buo+1)
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+2)                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis+2)                  &
     &          + d_rj(inod,ipol_rot_frc%i_lorentz+2)                   &
     &          + d_rj(inod,is_rot_buo+2)
      end do
!$omp end parallel do
!
      end subroutine set_MHD_terms_to_force
!
! ----------------------------------------------------------------------
!
      subroutine set_rotating_cv_terms_to_force                         &
     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                     &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: is_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect  )                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis  )                  &
     &          + d_rj(inod,is_rot_buo  )
        d_rj(inod,ipol_exp%i_forces+1)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+1)                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis+1)                  &
     &          + d_rj(inod,is_rot_buo+1)
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+2)                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis+2)                  &
     &          + d_rj(inod,is_rot_buo+2)
      end do
!$omp end parallel do
!
      end subroutine set_rotating_cv_terms_to_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine subtract_advection_to_force                            &
     &         (is_exp, is_inertia, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_exp, is_inertia
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
      d_rj(1:nnod_rj,is_exp  ) = d_rj(1:nnod_rj,is_exp  )               &
     &                        - d_rj(1:nnod_rj,is_inertia  )
      d_rj(1:nnod_rj,is_exp+1) = d_rj(1:nnod_rj,is_exp+1)               &
     &                        - d_rj(1:nnod_rj,is_inertia+1)
      d_rj(1:nnod_rj,is_exp+2) = d_rj(1:nnod_rj,is_exp+2)               &
     &                        - d_rj(1:nnod_rj,is_inertia+2)
!$omp end workshare nowait
!
      end subroutine subtract_advection_to_force
!
! ----------------------------------------------------------------------
!
      subroutine add_each_force_to_forces                               &
     &         (is_exp, is_force, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_exp, is_force
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
      d_rj(1:nnod_rj,is_exp  ) = d_rj(1:nnod_rj,is_exp  )               &
     &                       + d_rj(1:nnod_rj,is_force  )
      d_rj(1:nnod_rj,is_exp+1) = d_rj(1:nnod_rj,is_exp+1)               &
     &                       + d_rj(1:nnod_rj,is_force+1)
      d_rj(1:nnod_rj,is_exp+2) = d_rj(1:nnod_rj,is_exp+2)               &
     &                       + d_rj(1:nnod_rj,is_force+2)
!$omp end workshare nowait
!
      end subroutine add_each_force_to_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_inertia                                  &
     &         (fl_prop, ipol_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(fluid_property), intent(in) :: fl_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
      if(fl_prop%iflag_scheme .eq. id_no_evolution) return
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_pre_mom  )                                 &
     &        = d_rj(inod,ipol_exp%i_forces  )
        d_rj(inod,ipol_exp%i_pre_mom+2)                                 &
     &        = d_rj(inod,ipol_exp%i_forces+2)
      end do
!$omp end parallel do
!
      end subroutine set_ini_adams_inertia
!
! ----------------------------------------------------------------------
!
      end module cal_vorticity_terms_adams
