!copy_field_4_sph_trans.f90
!     module copy_field_4_sph_trans
!
!      subroutine copy_scalar_from_trans(nscalar_trans, i_trns,         &
!     &          nnod, dnod_sph)
!      subroutine copy_scalar_to_trans(nscalar_trans, i_trns,           &
!     &          nnod, dnod_sph)
!
!      subroutine copy_vector_from_trans(nvector_trans, i_trns,         &
!     &          nnod, dnod_sph)
!      subroutine copy_vector_to_trans(nvector_trans, i_trns,           &
!     &          nnod, dnod_sph)
!
!      subroutine copy_tensor_from_trans(ntensor_trans, i_trns,         &
!     &          nnod, dnod_sph)
!      subroutine copy_tensor_to_trans(ntensor_trans, i_trns,           &
!     &          nnod, dnod_sph)
!
!      subroutine copy_vector_tmp_to_trans(nvector_trans, i_trns)
!      subroutine copy_tensor_tmp_to_trans(ntensor_trans, i_trns)
!
!      Written by H. Matsui on Feb., 2008
!
      module copy_field_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_from_trans(nscalar_trans, i_trns,          &
     &          nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: nscalar_trans, i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod)
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nscalar_trans
          dnod_sph(inod) = vr_rtp(jnod)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_to_trans(nscalar_trans, i_trns,            &
     &          nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: nscalar_trans, i_trns, nnod
      real(kind = kreal), intent(in) :: dnod_sph(nnod)
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(inod,jnod,ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nscalar_trans
          vr_rtp(jnod) = dnod_sph(inod)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_to_trans
!
!-----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_vector_from_trans(nvector_trans, i_trns,          &
     &          nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: nvector_trans, i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod,3)
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nvector_trans
          dnod_sph(inod,1) = vr_rtp(3*jnod-2)
          dnod_sph(inod,2) = vr_rtp(3*jnod-1)
          dnod_sph(inod,3) = vr_rtp(3*jnod  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vector_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_to_trans(nvector_trans, i_trns,            &
     &          nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: nvector_trans, i_trns, nnod
      real(kind = kreal), intent(in) :: dnod_sph(nnod,3)
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(inod,jnod,ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nvector_trans
          vr_rtp(3*jnod-2) = dnod_sph(inod,1)
          vr_rtp(3*jnod-1) = dnod_sph(inod,2)
          vr_rtp(3*jnod  ) = dnod_sph(inod,3)
        end do
      end do
!$omp end do
!
      end subroutine copy_vector_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_trans(ntensor_trans, i_trns,          &
     &          nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: ntensor_trans, i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod,6)
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ntensor_trans
          dnod_sph(inod,1) = vr_rtp(6*jnod-5)
          dnod_sph(inod,2) = vr_rtp(6*jnod-4)
          dnod_sph(inod,3) = vr_rtp(6*jnod-3)
          dnod_sph(inod,4) = vr_rtp(6*jnod-2)
          dnod_sph(inod,5) = vr_rtp(6*jnod-1)
          dnod_sph(inod,6) = vr_rtp(6*jnod  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_to_trans(ntensor_trans, i_trns,            &
     &          nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: ntensor_trans, i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod,6)
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(inod,jnod,ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ntensor_trans
          vr_rtp(6*jnod-5) = dnod_sph(inod,1)
          vr_rtp(6*jnod-4) = dnod_sph(inod,2)
          vr_rtp(6*jnod-3) = dnod_sph(inod,3)
          vr_rtp(6*jnod-2) = dnod_sph(inod,4)
          vr_rtp(6*jnod-1) = dnod_sph(inod,5)
          vr_rtp(6*jnod  ) = dnod_sph(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vector_tmp_to_trans(nvector_trans, i_trns)
!
      integer(kind = kint), intent(in) :: nvector_trans, i_trns
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(inod,jnod,ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nvector_trans
          vr_rtp(3*jnod-2) = d_nod_rtp(inod,1)
          vr_rtp(3*jnod-1) = d_nod_rtp(inod,2)
          vr_rtp(3*jnod  ) = d_nod_rtp(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vector_tmp_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_tmp_to_trans(ntensor_trans, i_trns)
!
      integer(kind = kint), intent(in) :: ntensor_trans, i_trns
!
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
!$omp do private(inod,jnod,ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ntensor_trans
          vr_rtp(6*jnod-5) = d_nod_rtp(inod,1)
          vr_rtp(6*jnod-4) = d_nod_rtp(inod,2)
          vr_rtp(6*jnod-3) = d_nod_rtp(inod,3)
          vr_rtp(6*jnod-2) = d_nod_rtp(inod,4)
          vr_rtp(6*jnod-1) = d_nod_rtp(inod,5)
          vr_rtp(6*jnod  ) = d_nod_rtp(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_tmp_to_trans
!
!-----------------------------------------------------------------------
!
      end module copy_field_4_sph_trans
