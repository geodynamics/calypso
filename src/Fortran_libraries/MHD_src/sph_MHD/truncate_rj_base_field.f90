!>@file   truncate_rj_base_field.f90
!!        module truncate_rj_base_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Truncate base fields
!!
!!@verbatim
!!      subroutine s_truncate_rj_base_field(ltr_filter, sph_rj,         &
!!     &          base_fld, filter_fld, rj_fld)
!!        integer(kind = kint), intent(in) :: ltr_filter
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_field_address), intent(in) :: filter_fld
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
      module truncate_rj_base_field
!
      use m_precision
      use t_spheric_rj_data
      use t_phys_data
      use t_base_field_labels
!
      implicit  none
! 
      private :: truncate_rj_vector, truncate_rj_scalar
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_truncate_rj_base_field(ltr_filter, sph_rj,           &
     &          base_fld, filter_fld, rj_fld)
!
      integer(kind = kint), intent(in) :: ltr_filter
      type(sph_rj_grid), intent(in) :: sph_rj
      type(base_field_address), intent(in) :: base_fld
      type(base_field_address), intent(in) :: filter_fld
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call truncate_rj_vector(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_velo, filter_fld%i_velo)
      call truncate_rj_vector(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_vort, filter_fld%i_vort)
      call truncate_rj_vector(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_magne, filter_fld%i_magne)
      call truncate_rj_vector(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_vecp, filter_fld%i_vecp)
      call truncate_rj_vector(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_current, filter_fld%i_current)
!
!
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_temp, filter_fld%i_temp)
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &    base_fld%i_per_temp, filter_fld%i_per_temp)
!
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_light, filter_fld%i_light)
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &    base_fld%i_per_light, filter_fld%i_per_light)
!
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &                        base_fld%i_density, filter_fld%i_density)
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &    base_fld%i_per_density, filter_fld%i_per_density)
!
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &    base_fld%i_entropy, filter_fld%i_entropy)
      call truncate_rj_scalar(ltr_filter, sph_rj, rj_fld,               &
     &    base_fld%i_per_entropy, filter_fld%i_per_entropy)
!
      end subroutine s_truncate_rj_base_field
!
!-----------------------------------------------------------------------
!
      subroutine truncate_rj_vector(ltr_filter, sph_rj,                 &
     &          rj_fld, ipol_fld, ipol_filtered)
!
      integer(kind = kint), intent(in) :: ltr_filter
      integer(kind = kint), intent(in) :: ipol_fld, ipol_filtered
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl
!
!
      if(ipol_filtered .le. 0) return
!      write(*,*) 'Filtering vector', ipol_filtered
!$omp parallel do private(inod, l_gl)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        if(l_gl .le. ltr_filter) then
          rj_fld%d_fld(inod,ipol_filtered  )                            &
     &          = rj_fld%d_fld(inod,ipol_fld  )
          rj_fld%d_fld(inod,ipol_filtered+1)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+1)
          rj_fld%d_fld(inod,ipol_filtered+2)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+2)
        else
          rj_fld%d_fld(inod,ipol_filtered  ) = zero
          rj_fld%d_fld(inod,ipol_filtered+1) = zero
          rj_fld%d_fld(inod,ipol_filtered+2) = zero
        end if
      end do
!$omp end parallel do
!
      end subroutine truncate_rj_vector
!
!-----------------------------------------------------------------------
!
      subroutine truncate_rj_scalar(ltr_filter, sph_rj,                 &
     &          rj_fld, ipol_fld, ipol_filtered)
!
      integer(kind = kint), intent(in) :: ltr_filter
      integer(kind = kint), intent(in) :: ipol_fld, ipol_filtered
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl
!
!
      if(ipol_filtered .le. 0) return
!      write(*,*) 'Filtering scalar', ipol_filtered
!$omp parallel do private(inod, l_gl)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        if(l_gl .le. ltr_filter) then
          rj_fld%d_fld(inod,ipol_filtered  )                            &
     &          = rj_fld%d_fld(inod,ipol_fld  )
        else
          rj_fld%d_fld(inod,ipol_filtered  ) = zero
        end if
      end do
!$omp end parallel do
!
      end subroutine truncate_rj_scalar
!
!-----------------------------------------------------------------------
!
      subroutine truncate_rj_sym_tensor(ltr_filter, sph_rj,             &
     &          rj_fld, ipol_fld, ipol_filtered)
!
      integer(kind = kint), intent(in) :: ltr_filter
      integer(kind = kint), intent(in) :: ipol_fld, ipol_filtered
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl
!
!
      if(ipol_filtered .le. 0) return
!      write(*,*) 'Filtering tensor', ipol_filtered
!$omp parallel do private(inod, l_gl)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        if(l_gl .le. ltr_filter) then
          rj_fld%d_fld(inod,ipol_filtered  )                            &
     &          = rj_fld%d_fld(inod,ipol_fld  )
          rj_fld%d_fld(inod,ipol_filtered+1)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+1)
          rj_fld%d_fld(inod,ipol_filtered+2)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+2)
          rj_fld%d_fld(inod,ipol_filtered+3)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+3)
          rj_fld%d_fld(inod,ipol_filtered+4)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+4)
          rj_fld%d_fld(inod,ipol_filtered+5)                            &
     &          = rj_fld%d_fld(inod,ipol_fld+5)
        else
          rj_fld%d_fld(inod,ipol_filtered  ) = zero
          rj_fld%d_fld(inod,ipol_filtered+1) = zero
          rj_fld%d_fld(inod,ipol_filtered+2) = zero
          rj_fld%d_fld(inod,ipol_filtered+3) = zero
          rj_fld%d_fld(inod,ipol_filtered+4) = zero
          rj_fld%d_fld(inod,ipol_filtered+5) = zero
        end if
      end do
!$omp end parallel do
!
      end subroutine truncate_rj_sym_tensor
!
!-----------------------------------------------------------------------
!
      end module truncate_rj_base_field
