!>@file   copy_snap_4_sph_trans.f90
!!@brief  module copy_snap_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer
!!
!!@verbatim
!!      subroutine copy_scalar_spec_to_trans(ncomp_trans,               &
!!     &          is_spec, i_trns)
!!      subroutine copy_scalar_spec_from_trans(ncomp_trans,             &
!!     &          is_spec, i_trns)
!!
!!      subroutine copy_vec_spec_to_trans(ncomp_trans, is_spec, i_trns)
!!      subroutine copy_vec_spec_from_trans(ncomp_trans, is_spec, i_trns)
!!
!!      subroutine copy_tsr_spec_to_trans(ncomp_trans, is_spec, i_trns)
!!      subroutine copy_tsr_spec_from_trans(ncomp_trans, is_spec, i_trns)
!!@endverbatim
!
      module copy_spectr_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use m_sph_spectr_data
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_spec_to_trans(ncomp_trans,                 &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ncomp_trans
          sp_rj(jnod  ) = d_rj(inod,is_spec  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_spec_from_trans(ncomp_trans,               &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ncomp_trans
          d_rj(inod,is_spec  ) = sp_rj(jnod  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vec_spec_to_trans(ncomp_trans, is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ncomp_trans
          sp_rj(jnod  ) = d_rj(inod,is_spec  )
          sp_rj(jnod+1) = d_rj(inod,is_spec+1)
          sp_rj(jnod+2) = d_rj(inod,is_spec+2)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vec_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vec_spec_from_trans(ncomp_trans, is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ncomp_trans
          d_rj(inod,is_spec  ) = sp_rj(jnod  )
          d_rj(inod,is_spec+1) = sp_rj(jnod+1)
          d_rj(inod,is_spec+2) = sp_rj(jnod+2)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tsr_spec_to_trans(ncomp_trans, is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ncomp_trans
          sp_rj(jnod  ) = d_rj(inod,is_spec  )
          sp_rj(jnod+1) = d_rj(inod,is_spec+1)
          sp_rj(jnod+2) = d_rj(inod,is_spec+2)
          sp_rj(jnod+3) = d_rj(inod,is_spec+3)
          sp_rj(jnod+4) = d_rj(inod,is_spec+4)
          sp_rj(jnod+5) = d_rj(inod,is_spec+5)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tsr_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tsr_spec_from_trans(ncomp_trans, is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ncomp_trans
          d_rj(inod,is_spec  ) = sp_rj(jnod  )
          d_rj(inod,is_spec+1) = sp_rj(jnod+1)
          d_rj(inod,is_spec+2) = sp_rj(jnod+2)
          d_rj(inod,is_spec+3) = sp_rj(jnod+3)
          d_rj(inod,is_spec+4) = sp_rj(jnod+4)
          d_rj(inod,is_spec+5) = sp_rj(jnod+5)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tsr_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_spectr_4_sph_trans
