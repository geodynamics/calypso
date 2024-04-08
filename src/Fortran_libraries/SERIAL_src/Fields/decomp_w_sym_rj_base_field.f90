!>@file   sym_rj_base_field.f90
!!        module sym_rj_base_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief decomp base fields
!!
!!@verbatim
!!      subroutine s_decomp_rj_base_field(ltr_filter, sph_rj,           &
!!     &          base_fld, filter_fld, rj_fld)
!!        integer(kind = kint), intent(in) :: ltr_filter
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_field_address), intent(in) :: filter_fld
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
      module decomp_w_sym_rj_base_field
!
      use m_precision
      use t_spheric_rj_data
      use t_phys_data
      use t_base_field_labels
!
      implicit  none
! 
      private :: decomp_rj_vector, decomp_rj_scalar
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_decomp_w_sym_rj_base_field(sph_rj,                   &
     &          base_fld, sym_fld, asym_fld, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(base_field_address), intent(in) :: base_fld
      type(base_field_address), intent(in) :: sym_fld
      type(base_field_address), intent(in) :: asym_fld
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call decomp_rj_vector(sph_rj, rj_fld,                             &
     &    base_fld%i_velo, sym_fld%i_velo, asym_fld%i_velo)
      call decomp_rj_vector(sph_rj, rj_fld,                             &
     &    base_fld%i_vort, sym_fld%i_vort, asym_fld%i_vort)
      call decomp_rj_vector(sph_rj, rj_fld,                             &
     &    base_fld%i_magne, sym_fld%i_magne, asym_fld%i_magne)
      call decomp_rj_vector(sph_rj, rj_fld,                             &
     &    base_fld%i_vecp, sym_fld%i_vecp, asym_fld%i_vecp)
      call decomp_rj_vector(sph_rj, rj_fld,                             &
     &    base_fld%i_current, sym_fld%i_current, asym_fld%i_current)
!
!
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_temp, sym_fld%i_temp, asym_fld%i_temp)
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_per_temp, sym_fld%i_per_temp, asym_fld%i_per_temp)
!
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_light, sym_fld%i_light, asym_fld%i_light)
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_per_light, sym_fld%i_per_light,                    &
     &    asym_fld%i_per_light)
!
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_density, sym_fld%i_density, asym_fld%i_density)
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_per_density, sym_fld%i_per_density,                &
     &    asym_fld%i_per_density)
!
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_entropy, sym_fld%i_entropy, asym_fld%i_entropy)
      call decomp_rj_scalar(sph_rj, rj_fld,                             &
     &    base_fld%i_per_entropy, sym_fld%i_per_entropy,                &
     &    asym_fld%i_per_entropy)
!
      end subroutine s_decomp_w_sym_rj_base_field
!
!-----------------------------------------------------------------------
!
      subroutine decomp_rj_vector(sph_rj,                 &
     &          rj_fld, ipol_fld, ipol_sym, ipol_asym)
!
      integer(kind = kint), intent(in) :: ipol_fld, ipol_sym, ipol_asym
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl, m_gl, lm_odd
!
!
      if(ipol_sym .le. 0 .and. ipol_asym .le. 0 ) return
!      write(*,*) 'Filtering vector', ipol_filtered
!$omp parallel do private(inod, l_gl, m_gl, lm_odd)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        m_gl = sph_rj%idx_global_rj(inod,2) - l_gl*(l_gl + 1)
!        lm_odd = mod(l_gl - abs(m_gl), 2)
!        lm_odd = (l_gl - abs(m_gl)+1)/2 - (l_gl - abs(m_gl))/2
        lm_odd = int(l_gl - abs(m_gl)+1)/2 - int(l_gl - abs(m_gl))/2
! 
        rj_fld%d_fld(inod,ipol_sym  )                                   &
     &          = dble(1-lm_odd)*rj_fld%d_fld(inod,ipol_fld  )
        rj_fld%d_fld(inod,ipol_sym+1)                                   &
     &          = dble(1-lm_odd)*rj_fld%d_fld(inod,ipol_fld+1)
        rj_fld%d_fld(inod,ipol_sym+2)                                   &
     &          = dble(lm_odd)*rj_fld%d_fld(inod,ipol_fld+2)
!
        rj_fld%d_fld(inod,ipol_asym  )                                  &
     &          = dble(lm_odd)*rj_fld%d_fld(inod,ipol_fld  )
        rj_fld%d_fld(inod,ipol_asym+1)                                  &
     &          = dble(lm_odd)*rj_fld%d_fld(inod,ipol_fld+1)
        rj_fld%d_fld(inod,ipol_asym+2)                                  &
     &          = dble(1-lm_odd)*rj_fld%d_fld(inod,ipol_fld+2)
! 
!        if ( l_gl .eq. abs(m_gl) ) then 
!          rj_fld%d_fld(inod,ipol_sym  )                                &
!     &          = rj_fld%d_fld(inod,ipol_fld  )
!          rj_fld%d_fld(inod,ipol_sym+1)                                &
!     &          = rj_fld%d_fld(inod,ipol_fld+1)
!          rj_fld%d_fld(inod,ipol_sym+2)                                &
!     &          = 0.0d0
!!
!          rj_fld%d_fld(inod,ipol_asym  )                               &
!     &          = 0.0d0
!          rj_fld%d_fld(inod,ipol_asym+1)                               &
!     &          = 0.0d0
!          rj_fld%d_fld(inod,ipol_asym+2)                               &
!     &          = rj_fld%d_fld(inod,ipol_fld+2)
!                   write(*,*) ' l = m is called'
!             end if
      end do
!$omp end parallel do
!
      end subroutine decomp_rj_vector
!
!-----------------------------------------------------------------------
!
      subroutine decomp_rj_scalar(sph_rj,                               &
     &          rj_fld, ipol_fld, ipol_sym, ipol_asym)
!
      integer(kind = kint), intent(in) :: ipol_fld, ipol_sym, ipol_asym
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl, m_gl, lm_odd
!
!
      if(ipol_sym .le. 0 .and. ipol_asym .le. 0 ) return
! write(*,*) 'sym scalar', ipol_sym 
! write(*,*) 'asym scalar', ipol_asym 
!$omp parallel do private(inod, l_gl, m_gl, lm_odd)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        m_gl = sph_rj%idx_global_rj(inod,2) - l_gl*(l_gl + 1)
!        lm_odd = mod(l_gl - abs(m_gl), 2)
!        lm_odd = (l_gl - abs(m_gl)+1)/2 - (l_gl - abs(m_gl))/2
        lm_odd = int(l_gl - abs(m_gl)+1)/2 - int(l_gl - abs(m_gl))/2
! 
        rj_fld%d_fld(inod, ipol_sym)                                    &
     &   = dble(1-lm_odd) * rj_fld%d_fld(inod, ipol_fld)
        rj_fld%d_fld(inod, ipol_asym)                                   &
     &   = dble(lm_odd) * rj_fld%d_fld(inod, ipol_fld)
! 
!      if ( l_gl .eq. abs(m_gl) ) then 
!         rj_fld%d_fld(inod, ipol_sym)                                  &
!     &   = rj_fld%d_fld(inod, ipol_fld)
!         rj_fld%d_fld(inod, ipol_asym)                                 &
!     &   = 0.0d0
!         write(*,*) ' l = m is called'
!       end if
      end do
!$omp end parallel do
!
      end subroutine decomp_rj_scalar
!
!-----------------------------------------------------------------------
!!
!       subroutine decomp_rj_sym_tensor(ltr_filter, sph_rj,             &
!       &          rj_fld, ipol_fld, ipol_filtered)
!!
!       integer(kind = kint), intent(in) :: ltr_filter
!       integer(kind = kint), intent(in) :: ipol_fld, ipol_filtered
!       type(sph_rj_grid), intent(in) :: sph_rj
!       type(phys_data), intent(inout) :: rj_fld
!!
!      integer(kind = kint) :: inod, l_gl
!!
!!
!      if(ipol_sym .le. 0 .or. ipol_asym .le. 0 ) return
!!     write(*,*) 'Filtering tensor', ipol_filtered
! !$omp parallel do private(inod, l_gl)
!      do inod = 1, sph_rj%nnod_rj
!        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
!        if(l_gl .le. ltr_filter) then
!          rj_fld%d_fld(inod,ipol_filtered  )                            &
!     &        = rj_fld%d_fld(inod,ipol_fld  )
!          rj_fld%d_fld(inod,ipol_filtered+1)                            &
!     &        = rj_fld%d_fld(inod,ipol_fld+1)
!          rj_fld%d_fld(inod,ipol_filtered+2)                            &
!     &        = rj_fld%d_fld(inod,ipol_fld+2)
!          rj_fld%d_fld(inod,ipol_filtered+3)                            &
!     &        = rj_fld%d_fld(inod,ipol_fld+3)
!          rj_fld%d_fld(inod,ipol_filtered+4)                            &
!     &        = rj_fld%d_fld(inod,ipol_fld+4)
!          rj_fld%d_fld(inod,ipol_filtered+5)                            &
!     &        = rj_fld%d_fld(inod,ipol_fld+5)
!        else
!          rj_fld%d_fld(inod,ipol_filtered  ) = zero
!          rj_fld%d_fld(inod,ipol_filtered+1) = zero
!          rj_fld%d_fld(inod,ipol_filtered+2) = zero
!          rj_fld%d_fld(inod,ipol_filtered+3) = zero
!          rj_fld%d_fld(inod,ipol_filtered+4) = zero
!          rj_fld%d_fld(inod,ipol_filtered+5) = zero
!        end if
!      end do
!!$omp end parallel do
!!
!      end subroutine decomp_rj_sym_tensor
!!
!-----------------------------------------------------------------------
!
      end module decomp_w_sym_rj_base_field
