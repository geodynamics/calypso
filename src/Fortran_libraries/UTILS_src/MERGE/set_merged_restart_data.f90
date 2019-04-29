!
!     module set_merged_restart_data
!
!      Written by H.Matsui
!
!!      subroutine rescale_4_magne(b_ratio, fld)
!!        type(phys_data), intent(inout) :: fld
!
      module set_merged_restart_data
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rescale_4_magne(b_ratio, fld)
!
      use m_phys_labels
      use t_phys_data
!
      real(kind = kreal), intent(in) :: b_ratio
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i, j, jst, jed, inod
!
!
      if(b_ratio.eq.0.0d0 .or. b_ratio.eq.1.0d0) return
      do i = 1, fld%num_phys
        if (    fld%phys_name(i) .eq. fhd_vecp                          &
     &     .or. fld%phys_name(i) .eq. fhd_magne                         &
     &     .or. fld%phys_name(i) .eq. fhd_mag_potential                 &
     &     .or. fld%phys_name(i) .eq. fhd_pre_uxb                       &
     &     .or. fld%phys_name(i) .eq. fhd_chk_uxb) then
        jst = fld%istack_component(i-1) + 1
        jed = fld%istack_component(i)
!$omp parallel
          do j = jst, jed
!$omp do
            do inod = 1, fld%n_point
              fld%d_fld(inod,j) = b_ratio * fld%d_fld(inod,j)
            end do
!$omp end do nowait
          end do
!$omp end parallel
        end if
      end do
!
      end subroutine rescale_4_magne
!
!------------------------------------------------------------------
!
      end module set_merged_restart_data
